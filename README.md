[Code for the Innovation Strategy Explorer](https://0199f79a-f4b5-17c7-0eaa-8397f73494fa.share.connect.posit.cloud/)


# (Re) deploy
rsconnect::writeManifest()


## Data pipeline notes

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
