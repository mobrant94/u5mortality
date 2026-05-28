# ============================================================
# Full analytical pipeline
# ============================================================
# Run from the repository root, or set U5MORTALITY_BASE_DIR to the
# directory where the data files are stored.

source("scripts/00_config.R")

pipeline <- c(
  "scripts/00_join_unificado_mensal_anual.R",
  "scripts/01_diagnostico_denominador_base_anual.R",
  "scripts/02_tendencias_anuais_faixa_etaria.R",
  "scripts/03_modelo_rmt_dicotomico_base_anual_v9.R",
  "scripts/04_modelo_v10_phc_2lines_temporal.R",
  "scripts/05_modelo_v10_phc_difference_table.R",
  "scripts/06_counterfactual_panel_by_age.R"
)

for (script in pipeline) {
  message("\n============================================================")
  message("Running: ", script)
  message("============================================================")
  source(script)
}

message("\nPipeline completed.")
