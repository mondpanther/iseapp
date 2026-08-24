[Code for the Innovation Strategy Explorer](https://0199f79a-f4b5-17c7-0eaa-8397f73494fa.share.connect.posit.cloud/)


# (Re) deploy
rsconnect::writeManifest()


## Data pipeline notes

### No BigQuery (changed 2026-08-24)

`data-raw-2025/01-build-app-parquets.R` no longer uses `bigrquery`. The four
tables it used to download from `patbis.fromPATSTAT2025` are read as parquet
from `<dropbox>/PATSTAT autumn 2025 data/patstat_clean/` via DuckDB:

- `tls225_docdb_fam_cpc` -> `cpcs.fst` (space-stripping pushed into SQL, and
  `docdb_family_id` cast to INTEGER -- DuckDB maps BIGINT to double, whereas
  `bq_table_download()` defaulted to `bigint = "integer"`)
- `tls201_appln` -> the granted-family and family-size sets

`bq_cache()` source ids changed accordingly, so a cache built from BigQuery is
refreshed once rather than silently reused. Tune the scan with
`ISEAPP_DUCK_MEMORY` (default 8GB).

`whole_databuild_pipeline.R` gains `HARMONIZATION_BACKEND`, defaulting to
`"duckdb"` -- the country harmonisation now runs locally via
`LMICinnovation/code2025/build_countries_harm_duck.R` (override with
`LMIC_HARM_SCRIPT`). Set `HARMONIZATION_BACKEND="bq"` for the old path.

### Publishing to Dropbox (added 2026-08-24)

`data-raw-2025/publish_to_dropbox.R` copies

    inst/extdata/*.parquet  ->  <dropbox>/iseapp/database/

and writes a `manifest.json`. It runs automatically at the end of
`01-build-app-parquets.R` and again as the last step of
`whole_databuild_pipeline.R`, so `citenet.parquet` (built in step 4) is
included. Files already identical are skipped. `ISEAPP_NO_PUBLISH=1` skips it;
`publish_iseapp_database(dry_run = TRUE)` previews.

**This did not exist before** -- the shared copy was maintained by hand, so
nothing guaranteed it matched the build. `LMICinnovation/code2025` and
`code_linkedin` both read `<dropbox>/iseapp/database/`.

Note the destination is `<dropbox>/iseapp/`, **not** `<dropbox>/Apps/iseapp/`.
Two different folders, easy to conflate: `Apps/iseapp/` holds the legacy
assets (`istraxes/`, `inglobe/`, `duck/`, `LMICinnovation_repo/`).

### Country definitions (changed 2026-08-24)

`01-build-app-parquets.R` no longer hardcodes `lmics` / `eu_countries`. It
sources `LMICinnovation/code2025/country_definitions.R` (override with
`LMIC_COUNTRY_DEFS`), so `inst/extdata/country_lookup.parquet` is a derived
artefact of the same definition the paper uses rather than a second copy that
happens to match. Do not invert that: `country_lookup.parquet` only covers
countries observed in the patent data, so deriving the LMIC list from it would
drop GW, ST, TD and TL and reclassify them as high income.


### `RUN_VERSION` (added 2026-05-10)

`data-raw-2025/01-build-app-parquets.R` now reads from
`patbis2025/data/fromWATSON_<RUN_VERSION>/` instead of the legacy
`fromWATSON/`. `RUN_VERSION` defaults to `"basic"` (the inaugural
inventor-only / UM=0 PV run) and can be overridden:

```sh
PATBIS_RUN_VERSION=v2 Rscript data-raw-2025/01-build-app-parquets.R
PATBIS_RUN_VERSION=    Rscript data-raw-2025/01-build-app-parquets.R   # legacy bare folder
```

### Subclass-level istrax (changed 2026-05-10)

The `patchar` table (per-innovation `cost` / `alpha` / `istrax_global`) now
reads from **`innos_istraxsubclass_global_2013_2022.parquet`** instead of
`innos_istraxfield_global_2013_2022.parquet`. Same schema, but at IPC
subclass level (~648 subclasses × 10 years) instead of Hidden Giants field
level (only 6 of 41 fields have any meaningful coverage in the current
build — the WIPO-Schmoch traditional fields catid 1-33 are essentially
empty). This expands the patent_database universe from ~3.7M families to
~25M+ families. See the GitHub issue tracking the Hidden Giants field
coverage gap (assigned to Dennis).

Side effect: `cost` / `alpha` displayed in the app are now averages across
the family's IPC subclasses rather than its Hidden Giants field, which is
a coarser-resolution view of cost/alpha but covers ~7× more patents.

### Country-tab geocode filter (fixed 2026-05-10)

`build_city_clause_v2()` in `R/functions_data_v2.R` no longer applies the
`(NOT geocode_missing)` constraint when no city has been selected. Previously
the headline "X innovations" count on Country Explorer → Value flows by
Technology was silently restricted to the ~17% of families with a
successfully geocoded inventor city, even with "No city filter" set.
HiGGlobe's separate geocode logic is unaffected.

### "Exclude utility model patents" toggle (added 2026-05-12)

A new sidebar checkbox in Country Explorer, Region Explorer, and HiGGlobe.
When ticked, every query adds `AND p.is_um = FALSE` to filter out families
flagged as utility-model-only by patbis2025 (`is_um=TRUE` iff every
application in the family has `appln_kind` in `('U','W')`). About 30% of
PATSTAT families and ~42% of the 2013-22 PV-extrapolated cohort are
flagged UM, so the toggle has a substantial effect — especially on China-
heavy slices where utility models dominate.

Wiring summary:
- `data-raw-2025/01-build-app-parquets.R`: loads `fromWATSON_<ver>/innos_um.parquet`
  and merges `is_um` into `patchar`; the column propagates through
  `patchar_slim` into `patent_database.parquet`.
- `R/functions_data_v2.R`: new `build_exclude_um_clause_v2()` plus
  `exclude_um = FALSE` argument on every `sql_*_v2()` builder; clause
  interpolated next to `{granted_clause}` in every SQL block.
- `R/module_{country,region,hglobe}.R`: new `checkboxInput` next to the
  existing `granted_only` / `multifam_only` toggles, and `input$exclude_um`
  threaded into every SQL-builder call and `bindCache()` invalidation key.

The toggle defaults to FALSE (UM families included) so it doesn't change
existing behaviour unless explicitly ticked.
