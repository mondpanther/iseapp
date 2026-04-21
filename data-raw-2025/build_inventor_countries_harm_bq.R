# ============================================================================
# build_inventor_countries_harm_bq.R
# ----------------------------------------------------------------------------
# BigQuery-native rebuild of the inventor/holder country harmonization.
#
# Sources (all in patbis.fromPATSTAT2025):
#   tls201_appln        — appln_id -> docdb_family_id, earliest_publn_year
#   tls206_person       — person_id -> psn_name, person_ctry_code, person_address
#   tls207_pers_appln   — person_id x appln_id with invt_seq_nr / applt_seq_nr
#
# Universe: persons linked to at least one appln with earliest_publn_year >= 1999.
#
# Pipeline stages (each writes a table in patbis.iseapp):
#   S1. persons_post1999          — person_ids in scope
#   S2. persons_clean             — normalized psn_name (uppercased, stripped)
#                                   + psn_name_raw (uppercased but punctuation
#                                   preserved so bracket tokens survive),
#                                   TRIM-validated ctry
#   S3. persons_addr_inferred     — country inference cascade (first non-null
#                                   wins; ctry_source tag records the winner):
#                                     raw              — valid ISO2 in data
#                                     name_iso2        — trailing (XX) or [XX]
#                                                        in psn_name_raw
#                                     addr_iso2        — trailing ISO2 in addr
#                                     addr_country_name— country-name match
#                                     addr_postal      — postal-code pattern
#                                     name_legal       — legal-form suffix
#                                                        (GMBH->DE, KK->JP,
#                                                        OY->FI, SPA->IT,
#                                                        SARL->FR, SP Z OO->PL,
#                                                        PTE LTD->SG,
#                                                        PTY LTD->AU, ...)
#                                     unknown
#   S4. work                      — persons × appln rows with type in
#                                   {inventor, holder}, restricted to scope
#   S5. name_ctry_counts, name_top, cand_counts, fam_mode, tied_best
#                                 — harmonization SQL:
#                                   - Unambiguous name -> single winner.
#                                   - Tied name within a family: intersect
#                                     tied candidates with the family mode(s)
#                                     (majority country/ies across ALL persons
#                                     in the family — both inventors and
#                                     holders; multiple modes are kept when
#                                     tied). If the intersection is non-empty
#                                     those are retained; otherwise all tied
#                                     candidates are kept.
#   S6. inventor_countries_harm, holder_countries_harm — final tables
#
# Outputs: downloaded to .bigdata/{inventor,holder}_countries_harm.parquet so
# the rest of the local pipeline (build_nationalkey.R, 01-build-app-parquets.R)
# continues to work unchanged.
#
# One-time seed uploads (idempotent): iso2_codelist and country_name_lookup
# in patbis.iseapp. Uses countrycode::codelist for canonical names.
#
# Billing: expect ~$1-3 per full run at BQ's $5/TB scanned pricing.
# ============================================================================

library(DBI)
library(bigrquery)
library(data.table)
library(arrow)
library(countrycode)
library(tictoc)

project   <- "patbis"
dataset   <- "iseapp"     # new dataset for this pipeline's outputs
src_ds    <- "fromPATSTAT2025"
bigdata   <- ".bigdata"
dir.create(bigdata, showWarnings = FALSE)

out_inv   <- file.path(bigdata, "inventor_countries_harm.parquet")
out_hold  <- file.path(bigdata, "holder_countries_harm.parquet")

# ---- Helpers ---------------------------------------------------------------

bq_run <- function(sql, label = NULL) {
  if (!is.null(label)) cat("  >> ", label, "\n", sep = "")
  bq_project_query(project, sql)
  invisible(TRUE)
}

bq_count <- function(fqtn) {
  as.integer(bq_table_download(
    bq_project_query(project,
      sprintf("SELECT COUNT(*) AS n FROM `%s`", fqtn)),
    page_size = 1L, bigint = "integer64")$n)
}

fqtn <- function(table) sprintf("%s.%s.%s", project, dataset, table)

# ---- 0. Ensure target dataset + seed tables exist --------------------------

cat("=== 0. Dataset & seed tables ===\n")

# Create dataset if missing
ds_ref <- bq_dataset(project, dataset)
if (!bq_dataset_exists(ds_ref)) {
  cat("  Creating dataset ", project, ".", dataset, " ...\n", sep = "")
  bq_dataset_create(ds_ref, location = "US")
}

# Seed 1: ISO2 codelist (~249 rows from countrycode::codelist)
iso_tbl <- fqtn("iso2_codelist")
if (!bq_table_exists(bq_table(project, dataset, "iso2_codelist"))) {
  cat("  Seeding iso2_codelist ...\n")
  iso_df <- data.frame(
    iso2 = unique(na.omit(countrycode::codelist$iso2c)),
    stringsAsFactors = FALSE
  )
  bq_table_upload(bq_table(project, dataset, "iso2_codelist"),
                  values = iso_df, fields = list(bq_field("iso2", "STRING")))
}

# Seed 1b: ISO2 aliases — non-canonical two-letter codes often seen in
# patent data (name brackets, addresses) that should be remapped to the
# canonical ISO2 before validation. Extend here as new aliases surface.
alias_tbl <- fqtn("iso2_aliases")
if (!bq_table_exists(bq_table(project, dataset, "iso2_aliases"))) {
  cat("  Seeding iso2_aliases ...\n")
  alias_df <- data.frame(
    alias = c("UK", "EL", "SU", "DD", "ZR", "TP", "YU", "CS", "FX", "AN"),
    iso2  = c("GB", "GR", "RU", "DE", "CD", "TL", "RS", "CZ", "FR", "NL"),
    stringsAsFactors = FALSE
    # UK=Great Britain (ISO2 is GB), EL=Greece (EU customs), SU=USSR->RU,
    # DD=East Germany->DE, ZR=Zaire->DR Congo, TP=old East Timor->TL,
    # YU=Yugoslavia->Serbia (best proxy), CS=Czechoslovakia->Czechia,
    # FX=Metropolitan France->FR, AN=Netherlands Antilles->NL.
  )
  bq_table_upload(bq_table(project, dataset, "iso2_aliases"),
                  values = alias_df,
                  fields = list(
                    bq_field("alias", "STRING"),
                    bq_field("iso2",  "STRING")))
}

# Seed 2: country name lookup (English + common variants -> ISO2).
#   Upload ONCE. We use countrycode::codelist$country.name.en plus a small
#   hand-curated list of aliases commonly seen in PATSTAT addresses.
cn_tbl <- fqtn("country_name_lookup")
if (!bq_table_exists(bq_table(project, dataset, "country_name_lookup"))) {
  cat("  Seeding country_name_lookup ...\n")
  cl <- countrycode::codelist[, c("iso2c", "country.name.en")]
  cl <- cl[!is.na(cl$iso2c) & !is.na(cl$country.name.en), ]

  aliases <- data.frame(
    name = c("USA", "U.S.A.", "U.S.", "United States of America",
             "UK", "Great Britain", "Britain",
             "West Germany", "East Germany", "Deutschland",
             "People's Republic of China", "PRC", "Mainland China",
             "Republic of Korea", "South Korea", "Korea",
             "North Korea", "DPRK",
             "Republic of China", "Chinese Taipei",
             "Russian Federation", "Russia", "USSR",
             "Iran, Islamic Republic of",
             "Czech Republic",
             "Vietnam", "Viet Nam",
             "Netherlands, The", "Holland",
             "Swiss Confederation", "Schweiz", "Suisse",
             "Republic of Ireland", "Eire",
             "Republic of South Africa",
             "Republic of Singapore",
             "Kingdom of Saudi Arabia", "KSA",
             "United Arab Emirates", "UAE"),
    iso2 = c("US","US","US","US",
             "GB","GB","GB",
             "DE","DE","DE",
             "CN","CN","CN",
             "KR","KR","KR",
             "KP","KP",
             "TW","TW",
             "RU","RU","RU",
             "IR",
             "CZ",
             "VN","VN",
             "NL","NL",
             "CH","CH","CH",
             "IE","IE",
             "ZA",
             "SG",
             "SA","SA",
             "AE","AE"),
    stringsAsFactors = FALSE
  )

  cn_df <- rbind(
    data.frame(name = cl$country.name.en, iso2 = cl$iso2c,
               stringsAsFactors = FALSE),
    aliases
  )
  cn_df$name_lc <- tolower(cn_df$name)
  cn_df <- cn_df[!duplicated(cn_df$name_lc), ]

  bq_table_upload(bq_table(project, dataset, "country_name_lookup"),
                  values = cn_df,
                  fields = list(
                    bq_field("name",    "STRING"),
                    bq_field("iso2",    "STRING"),
                    bq_field("name_lc", "STRING")))
}

# ---- 1. Person universe: linked to an appln with earliest_publn_year >= 1999

cat("\n=== 1. persons_post1999 ===\n")
tic("S1")
bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  SELECT DISTINCT pa.person_id
  FROM `%s.%s.tls207_pers_appln`    pa
  JOIN `%s.%s.tls201_appln`         a
    USING (appln_id)
  WHERE a.earliest_publn_year >= 1999
    AND pa.person_id IS NOT NULL
    AND (pa.invt_seq_nr > 0 OR pa.applt_seq_nr > 0)
", fqtn("persons_post1999"),
   project, src_ds,
   project, src_ds))
cat("  persons_post1999 rows:", format(bq_count(fqtn("persons_post1999")),
                                        big.mark = ","), "\n")
toc()

# ---- 2. persons_clean: normalize name, TRIM-validate ctry ------------------
# Note: person_ctry_code sometimes has leading/trailing whitespace ("  "),
# so TRIM before the ISO2 check. Invalid trimmed codes (including empty) are
# nulled here, ready to be filled from the address in stage 3.

cat("\n=== 2. persons_clean (normalize name + validate ctry) ===\n")
tic("S2")
bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  SELECT
    p.person_id,
    -- Uppercased but punctuation preserved, so tokens like '(JP)' / '[DE]'
    -- remain extractable by the name_iso2 inference step in S3.
    UPPER(IFNULL(p.psn_name, ''))              AS psn_name_raw,
    -- Aggressive name normalization:
    --   strip all non-alphanumerics -> single spaces, trim, uppercase.
    TRIM(REGEXP_REPLACE(
           UPPER(IFNULL(p.psn_name, '')),
           r'[^A-Z0-9]+', ' ')
        )                                     AS psn_name,
    CASE
      WHEN iso.iso2 IS NOT NULL THEN iso.iso2
      ELSE NULL
    END                                       AS person_ctry_code_raw,
    p.person_address                          AS person_address
  FROM `%s.%s.tls206_person` p
  INNER JOIN `%s` u
    ON p.person_id = u.person_id
  LEFT JOIN `%s` iso
    ON TRIM(p.person_ctry_code) = iso.iso2
", fqtn("persons_clean"),
   project, src_ds,
   fqtn("persons_post1999"),
   iso_tbl))

# Diagnostics: how many still need country inference?
diag2 <- bq_table_download(bq_project_query(project, sprintf("
  SELECT
    COUNT(*) AS total,
    SUM(CASE WHEN person_ctry_code_raw IS NULL THEN 1 ELSE 0 END) AS missing_ctry,
    SUM(CASE WHEN person_ctry_code_raw IS NULL
              AND person_address IS NOT NULL
              AND LENGTH(TRIM(person_address)) > 0
             THEN 1 ELSE 0 END) AS missing_with_addr
  FROM `%s`
", fqtn("persons_clean"))), bigint = "integer64")
cat(sprintf("  persons_clean rows:  %s\n", format(diag2$total,             big.mark=",")))
cat(sprintf("    missing ctry:      %s  (%.2f%%)\n",
            format(diag2$missing_ctry, big.mark=","),
            100 * as.numeric(diag2$missing_ctry) / as.numeric(diag2$total)))
cat(sprintf("    w/ address to infer from: %s\n",
            format(diag2$missing_with_addr, big.mark=",")))
toc()

# ---- 3. persons_addr_inferred: country inference cascade -------------------
# Applied in priority order; first non-null wins.
#
#   N1. ISO2 in brackets/parens in the raw name      -> name_iso2
#         e.g. "ACME CORP (JP)", "SOMEONE [DE]"
#   A1. Trailing ISO2 token at end of address        -> addr_iso2
#   A2. Country-name match in address (longest-first)-> addr_country_name
#   A3. Postal-code patterns                         -> addr_postal
#   N2. Legal-form suffix keyword in normalized name -> name_legal
#         Weakest; only used as last resort.

cat("\n=== 3. persons_addr_inferred (name + address cascade) ===\n")
tic("S3")

bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  WITH
  -- N1: ISO2 inside brackets/parens anywhere in the raw name. Greedy prefix
  -- (^.*) pushes the capture to the LAST occurrence so 'FOO (US) BAR (JP)'
  -- resolves to JP.
  n1 AS (
    SELECT
      pc.person_id,
      REGEXP_EXTRACT(
        pc.psn_name_raw,
        r'^.*[\\(\\[]([A-Z]{2})[\\)\\]]'
      ) AS candidate
    FROM `%s` pc
    WHERE pc.person_ctry_code_raw IS NULL
      AND pc.psn_name_raw IS NOT NULL
      AND pc.psn_name_raw != ''
  ),
  -- Remap common non-canonical codes (UK->GB etc.) before validating.
  n1_normalized AS (
    SELECT n.person_id,
           COALESCE(al.iso2, n.candidate) AS candidate
    FROM n1 n
    LEFT JOIN `%s` al ON n.candidate = al.alias
  ),
  n1_valid AS (
    SELECT nn.person_id, iso.iso2 AS inferred
    FROM n1_normalized nn
    JOIN `%s` iso ON nn.candidate = iso.iso2
  ),

  -- A1: trailing ISO2 at end of the address, optionally preceded by ,/;/space
  a1 AS (
    SELECT
      pc.person_id,
      REGEXP_EXTRACT(
        UPPER(pc.person_address),
        r'(?:^|[,;\\s])([A-Z]{2})\\s*$'
      ) AS candidate
    FROM `%s` pc
    WHERE pc.person_ctry_code_raw IS NULL
      AND pc.person_address IS NOT NULL
  ),
  a1_normalized AS (
    SELECT a.person_id,
           COALESCE(al.iso2, a.candidate) AS candidate
    FROM a1 a
    LEFT JOIN `%s` al ON a.candidate = al.alias
  ),
  a1_valid AS (
    SELECT an.person_id, iso.iso2 AS inferred
    FROM a1_normalized an
    JOIN `%s` iso ON an.candidate = iso.iso2
  ),

  -- A2: country-name match. Pick the longest name matched (case-insensitive)
  -- so multi-word names like 'United Kingdom' beat short ones like 'UK' only
  -- when they appear; ties broken by alphabetical iso2 for determinism.
  addr_lc AS (
    SELECT
      pc.person_id,
      CONCAT(' ', LOWER(pc.person_address), ' ') AS addr_lc
    FROM `%s` pc
    WHERE pc.person_ctry_code_raw IS NULL
      AND pc.person_address IS NOT NULL
  ),
  a2_all AS (
    SELECT
      al.person_id,
      cn.iso2,
      cn.name_lc,
      LENGTH(cn.name_lc) AS name_len
    FROM addr_lc al
    JOIN `%s` cn
      ON STRPOS(al.addr_lc, CONCAT(' ', cn.name_lc, ' ')) > 0
      OR STRPOS(al.addr_lc, CONCAT(',', cn.name_lc, ' ')) > 0
      OR STRPOS(al.addr_lc, CONCAT(' ', cn.name_lc, ','))  > 0
      OR STRPOS(al.addr_lc, CONCAT(',', cn.name_lc, ','))  > 0
  ),
  a2_pick AS (
    SELECT person_id, iso2 AS inferred
    FROM (
      SELECT *,
             ROW_NUMBER() OVER (
               PARTITION BY person_id
               ORDER BY name_len DESC, iso2 ASC
             ) AS rnk
      FROM a2_all
    )
    WHERE rnk = 1
  ),

  -- A3: postal-code patterns (deterministic subset)
  --   Canadian postal: A1A 1A1
  --   US ZIP+4        : 12345-6789
  --   UK postcode     : e.g. SW1A 1AA, M1 1AA
  a3 AS (
    SELECT
      pc.person_id,
      CASE
        WHEN REGEXP_CONTAINS(UPPER(pc.person_address), r'\\b[A-CEGHJ-NPR-TVXY]\\d[A-CEGHJ-NPR-TV-Z][[:space:]]?\\d[A-CEGHJ-NPR-TV-Z]\\d\\b') THEN 'CA'
        WHEN REGEXP_CONTAINS(pc.person_address, r'\\b\\d{5}-\\d{4}\\b') THEN 'US'
        WHEN REGEXP_CONTAINS(UPPER(pc.person_address), r'\\b[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}\\b') THEN 'GB'
        ELSE NULL
      END AS inferred
    FROM `%s` pc
    WHERE pc.person_ctry_code_raw IS NULL
      AND pc.person_address IS NOT NULL
  ),

  -- N2: legal-form suffix keyword match on the normalized (token-spaced) name.
  --   Matched only as whole-word tokens (spaces or string boundary around).
  --   Only highly country-specific suffixes are mapped here — noisy ones
  --   (LTD, INC, CORP, SA, NV, AB, AG) are intentionally omitted.
  n2 AS (
    SELECT
      pc.person_id,
      CASE
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)GMBH(?:\\s|$)')              THEN 'DE'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'KABUSHIKI KAISHA')                   THEN 'JP'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)KK\\s*$')                   THEN 'JP'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)OYJ?(?:\\s|$)')             THEN 'FI'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)SPA(?:\\s|$)')              THEN 'IT'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)SRL(?:\\s|$)')              THEN 'IT'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)SARL(?:\\s|$)')             THEN 'FR'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)SAS(?:\\s|$)')              THEN 'FR'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)SP Z O O(?:\\s|$)')         THEN 'PL'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)PTE LTD(?:\\s|$)')          THEN 'SG'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)PTY LTD(?:\\s|$)')          THEN 'AU'
        WHEN REGEXP_CONTAINS(pc.psn_name, r'(?:^|\\s)BV\\s*$')                   THEN 'NL'
        ELSE NULL
      END AS inferred
    FROM `%s` pc
    WHERE pc.person_ctry_code_raw IS NULL
      AND pc.psn_name IS NOT NULL
      AND pc.psn_name != ''
  ),

  final AS (
    SELECT
      pc.person_id,
      pc.psn_name,
      pc.person_address,
      pc.person_ctry_code_raw,
      COALESCE(n1.inferred, a1.inferred, a2.inferred, a3.inferred, n2.inferred)
        AS inferred_ctry,
      CASE
        WHEN pc.person_ctry_code_raw IS NOT NULL THEN 'raw'
        WHEN n1.inferred IS NOT NULL              THEN 'name_iso2'
        WHEN a1.inferred IS NOT NULL              THEN 'addr_iso2'
        WHEN a2.inferred IS NOT NULL              THEN 'addr_country_name'
        WHEN a3.inferred IS NOT NULL              THEN 'addr_postal'
        WHEN n2.inferred IS NOT NULL              THEN 'name_legal'
        ELSE 'unknown'
      END AS ctry_source
    FROM `%s` pc
    LEFT JOIN n1_valid n1 USING (person_id)
    LEFT JOIN a1_valid a1 USING (person_id)
    LEFT JOIN a2_pick  a2 USING (person_id)
    LEFT JOIN a3       a3 USING (person_id)
    LEFT JOIN n2       n2 USING (person_id)
  )
  SELECT
    person_id,
    psn_name,
    COALESCE(person_ctry_code_raw, inferred_ctry) AS person_ctry_code,
    ctry_source
  FROM final
  WHERE COALESCE(person_ctry_code_raw, inferred_ctry) IS NOT NULL
",
  fqtn("persons_addr_inferred"),
  fqtn("persons_clean"),          # n1
  alias_tbl,                       # n1_normalized (alias remap)
  iso_tbl,                         # n1_valid
  fqtn("persons_clean"),          # a1
  alias_tbl,                       # a1_normalized (alias remap)
  iso_tbl,                         # a1_valid
  fqtn("persons_clean"),          # addr_lc
  cn_tbl,                          # a2_all
  fqtn("persons_clean"),          # a3
  fqtn("persons_clean"),          # n2
  fqtn("persons_clean")           # final FROM
))

# Diagnostics: how much did Tier A recover?
diag3 <- bq_table_download(bq_project_query(project, sprintf("
  SELECT ctry_source, COUNT(*) AS n
  FROM `%s`
  GROUP BY ctry_source
  ORDER BY n DESC
", fqtn("persons_addr_inferred"))))
cat("  Country source breakdown:\n")
for (i in seq_len(nrow(diag3))) {
  cat(sprintf("    %-20s  %s\n", diag3$ctry_source[i],
              format(as.integer(diag3$n[i]), big.mark = ",")))
}
toc()

# ---- 4. work: persons × applns, split by type -----------------------------

cat("\n=== 4. work (persons x applns, inventor/holder split) ===\n")
tic("S4")
bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s`
  PARTITION BY RANGE_BUCKET(docdb_family_id, GENERATE_ARRAY(0, 80000000, 1000000))
  CLUSTER BY psn_name
  AS
  SELECT
    p.psn_name,
    a.docdb_family_id,
    p.person_ctry_code,
    CASE
      WHEN pa.invt_seq_nr  > 0 THEN 'inventor'
      WHEN pa.applt_seq_nr > 0 THEN 'holder'
    END AS type
  FROM `%s` p
  JOIN `%s.%s.tls207_pers_appln` pa USING (person_id)
  JOIN `%s.%s.tls201_appln`      a  USING (appln_id)
  WHERE a.earliest_publn_year >= 1999
    AND (pa.invt_seq_nr > 0 OR pa.applt_seq_nr > 0)
", fqtn("work"),
   fqtn("persons_addr_inferred"),
   project, src_ds,
   project, src_ds))

# Row count diag
w_n <- bq_count(fqtn("work"))
cat("  work rows:", format(w_n, big.mark=","), "\n")
toc()

# ---- 5. Harmonization SQL: per-name vote + family mode + tie-break ---------

cat("\n=== 5. Harmonization aggregations ===\n")
tic("S5")

# 5a: per-name country counts + top candidate(s)
bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  SELECT psn_name, person_ctry_code, COUNT(*) AS N
  FROM `%s`
  GROUP BY psn_name, person_ctry_code
", fqtn("name_ctry_counts"), fqtn("work")), "name_ctry_counts")

bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  SELECT psn_name, person_ctry_code, N
  FROM (
    SELECT *, MAX(N) OVER (PARTITION BY psn_name) AS max_N
    FROM `%s`
  )
  WHERE N = max_N
", fqtn("name_top"), fqtn("name_ctry_counts")), "name_top")

bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  SELECT psn_name, COUNT(*) AS n_candidates
  FROM `%s`
  GROUP BY psn_name
", fqtn("cand_counts"), fqtn("name_top")), "cand_counts")

bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  SELECT nt.psn_name, nt.person_ctry_code AS harm_ctry
  FROM `%s` nt
  JOIN `%s` cc USING (psn_name)
  WHERE cc.n_candidates = 1
", fqtn("name_best_unambig"),
   fqtn("name_top"),
   fqtn("cand_counts")), "name_best_unambig")

# 5b: Family-level mode country/ies.
# Counts person rows (both inventors AND holders) per (family, country) and
# keeps all countries tied at the family max. A family can therefore have
# MULTIPLE modes if several countries are equally common.
bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  WITH fam_ctry_counts AS (
    SELECT docdb_family_id, person_ctry_code, COUNT(*) AS n
    FROM `%s`
    GROUP BY docdb_family_id, person_ctry_code
  ),
  fam_max AS (
    SELECT docdb_family_id, MAX(n) AS max_n
    FROM fam_ctry_counts
    GROUP BY docdb_family_id
  )
  SELECT c.docdb_family_id, c.person_ctry_code AS fam_mode
  FROM fam_ctry_counts c
  JOIN fam_max m USING (docdb_family_id)
  WHERE c.n = m.max_n
", fqtn("fam_mode"),
   fqtn("work")), "fam_mode")

# 5c: tie resolution.
# Rule:
#   1. For a psn_name with multiple tied top candidates within a family, keep
#      only the tied candidates that are ALSO family-mode countries.
#   2. If the family has multiple modes, any tied candidate that matches any
#      of those modes is retained (all of them).
#   3. If NONE of the tied candidates intersects the family modes, fall back
#      to keeping all tied candidates (no tie-break).
bq_run(sprintf("
  CREATE OR REPLACE TABLE `%s` AS
  WITH tied_names AS (
    SELECT psn_name FROM `%s` WHERE n_candidates > 1
  ),
  tied_fam AS (
    SELECT DISTINCT w.psn_name, w.docdb_family_id
    FROM `%s` w
    JOIN tied_names tn USING (psn_name)
  ),
  tied_cross AS (
    SELECT
      tf.psn_name,
      tf.docdb_family_id,
      nt.person_ctry_code,
      CASE WHEN fm.fam_mode IS NOT NULL THEN 1 ELSE 0 END AS match_flag
    FROM tied_fam tf
    JOIN `%s` nt USING (psn_name)
    LEFT JOIN `%s` fm
      ON fm.docdb_family_id = tf.docdb_family_id
     AND fm.fam_mode        = nt.person_ctry_code
  ),
  per_group AS (
    SELECT psn_name, docdb_family_id, MAX(match_flag) AS any_match
    FROM tied_cross
    GROUP BY psn_name, docdb_family_id
  )
  SELECT tc.psn_name, tc.docdb_family_id,
         tc.person_ctry_code AS harm_ctry
  FROM tied_cross tc
  JOIN per_group pg USING (psn_name, docdb_family_id)
  WHERE (pg.any_match = 1 AND tc.match_flag = 1)   -- intersect w/ family modes
     OR (pg.any_match = 0)                         -- no intersection: keep all tied
", fqtn("tied_best"),
   fqtn("cand_counts"),
   fqtn("work"),
   fqtn("name_top"),
   fqtn("fam_mode")), "tied_best")

toc()

# ---- 6. Final outputs: (docdb_family_id, harm_ctry) per type --------------

cat("\n=== 6. Final harmonized outputs ===\n")
tic("S6")

# Materialize the harmonized row-level table, then the DISTINCT per-type views.
# (One CREATE per output so each query is simple and BQ can cache.)
for (role in c("inventor", "holder")) {
  out_table <- paste0(role, "_countries_harm")
  bq_run(sprintf("
    CREATE OR REPLACE TABLE `%s` AS
    SELECT DISTINCT
      w.docdb_family_id,
      COALESCE(nu.harm_ctry, tb.harm_ctry, w.person_ctry_code)
        AS person_ctry_code
    FROM `%s` w
    LEFT JOIN `%s` nu
      ON w.psn_name = nu.psn_name
    LEFT JOIN `%s` tb
      ON w.psn_name = tb.psn_name
     AND w.docdb_family_id = tb.docdb_family_id
    WHERE w.type = '%s'
  ", fqtn(out_table),
     fqtn("work"),
     fqtn("name_best_unambig"),
     fqtn("tied_best"),
     role), paste("emit", role))
  cat("  ", role, ":", format(bq_count(fqtn(out_table)), big.mark=","), "rows\n")
}
toc()

# ---- 7. Download to local parquets ----------------------------------------

cat("\n=== 7. Downloading outputs to .bigdata/ ===\n")
tic("S7")
for (role in c("inventor", "holder")) {
  local_path <- if (role == "inventor") out_inv else out_hold
  cat("  Downloading", role, "-> ", local_path, "...\n")
  df <- bq_table_download(
    bq_table(project, dataset, paste0(role, "_countries_harm")),
    page_size = 100000, bigint = "integer"
  )
  arrow::write_parquet(df, local_path,
                       compression = "zstd", compression_level = 3)
  cat("    wrote ", nrow(df), " rows -> ", local_path, "\n", sep = "")
}
toc()

# ---- 8. Coverage stats: distinct families by filing-year window -----------
# "Mapped" = family has at least one country in inventor_countries_harm OR
# holder_countries_harm. Family filing year = MIN(earliest_filing_year) over
# all applns in that family (standard PATSTAT convention).

cat("\n=== 8. Coverage stats (distinct docdb_family_ids) ===\n")
tic("S8")
stats <- bq_table_download(bq_project_query(project, sprintf("
  WITH fam_filing_year AS (
    SELECT docdb_family_id, MIN(earliest_filing_year) AS fam_year
    FROM `%s.%s.tls201_appln`
    WHERE docdb_family_id IS NOT NULL
    GROUP BY docdb_family_id
  ),
  mapped AS (
    SELECT docdb_family_id FROM `%s`
    UNION DISTINCT
    SELECT docdb_family_id FROM `%s`
  )
  SELECT
    COUNTIF(f.fam_year BETWEEN 2009 AND 2018) AS n_2009_2018,
    COUNTIF(f.fam_year BETWEEN 2013 AND 2022) AS n_2013_2022,
    COUNT(*)                                  AS n_total_mapped
  FROM mapped m
  JOIN fam_filing_year f USING (docdb_family_id)
", project, src_ds,
   fqtn("inventor_countries_harm"),
   fqtn("holder_countries_harm"))), bigint = "integer64")

cat(sprintf("  mapped families, filing year 2009-2018: %s\n",
            format(as.numeric(stats$n_2009_2018), big.mark = ",")))
cat(sprintf("  mapped families, filing year 2013-2022: %s\n",
            format(as.numeric(stats$n_2013_2022), big.mark = ",")))
cat(sprintf("  mapped families, all years:             %s\n",
            format(as.numeric(stats$n_total_mapped), big.mark = ",")))
toc()

cat("\n=== Done. BQ outputs in patbis.iseapp.* and local .bigdata/ ===\n")
