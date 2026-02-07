# Run this once to convert
library(arrow)
df_raw <- fst::read_fst("inst/extdata/long_final.fst")

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

arrow::write_parquet(df_processed, "inst/extdata/long_final.parquet")
