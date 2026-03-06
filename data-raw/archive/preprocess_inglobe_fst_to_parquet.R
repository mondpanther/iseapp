# Run this once to convert
library(arrow)
df_raw <- fst::read_fst("data-raw/big_files/long_final.fst")

df_processed <- df_raw |>
  dplyr::arrange(sce_country, tech_group, tech_subgroup, source_id, wave) |>
  dplyr::mutate(
    wave = as.integer(wave),
    chain_id = paste0(
      sce_country, "_", tech_group, "_",
      ifelse(is.na(tech_subgroup), "ALL", tech_subgroup), "_",
      sample_size, "_", source_id
    )
  )

df_processed <- df_processed |>
  dplyr::select(
    sce_country,
    sce_tech_display,
    tech_group,
    sample_size,
    wave,
    source_lon,
    source_lat,
    target_lon,
    target_lat,
    chain_id
  )

arrow::write_parquet(df_processed, "inst/extdata/long_final_processed.parquet")

# Precompute metadata
metadata <- df_processed |>
  dplyr::distinct(sce_country, sce_tech_display, tech_group, sample_size)

saveRDS(metadata, "inst/extdata/metadata.rds")

# Precompute wave range
wave_range <- data.frame(
  min_wave = min(df_processed$wave),
  max_wave = max(df_processed$wave)
)

saveRDS(wave_range, "inst/extdata/wave_range.rds")

# Precompute tech group definitions
tech_group_definitions <- list(
  "All" = sort(unique(metadata$sce_tech_display[metadata$tech_group == "All"])),
  "Green" = sort(unique(metadata$sce_tech_display[metadata$tech_group == "Green"])),
  "Non-Green" = sort(unique(metadata$sce_tech_display[metadata$tech_group == "Non-Green"]))
)

saveRDS(tech_group_definitions, "inst/extdata/tech_groups.rds")
