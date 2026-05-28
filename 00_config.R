# ============================================================
# Project configuration
# ============================================================
# Define U5MORTALITY_BASE_DIR before running the scripts, or edit the
# fallback path below. This directory should contain the raw/processed
# data files used by the pipeline.

options(stringsAsFactors = FALSE)

base_dir <- Sys.getenv("U5MORTALITY_BASE_DIR", unset = getwd())
pasta_base <- base_dir

message("Using base_dir: ", base_dir)
