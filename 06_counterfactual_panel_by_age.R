# ============================================================
# COUNTERFACTUAL PANEL BY AGE GROUP - FINAL ROBUST VERSION
#
# Observed deaths vs counterfactual deaths if municipalities with
# policy expansion alone had followed the mortality trajectory observed
# under policy expansion plus primary health care expansion.
#
# Required input:
#   04_time_2lines_change.csv
#   or object `temporal3` already loaded in the R environment.
#
# Outputs:
#   counterfactual_panel_by_age/figures/
#   counterfactual_panel_by_age/tables/
# ============================================================

options(stringsAsFactors = FALSE)

# ============================================================
# 0. USER SETTINGS
# ============================================================

if (!exists("base_dir")) {
  base_dir <- Sys.getenv("U5MORTALITY_BASE_DIR", unset = getwd())
}

# If automatic search fails, paste the full file path here:
# input_file_manual <- "C:/.../04_time_2lines_change.csv"
input_file_manual <- NA_character_

ANO_INICIAL <- 2001L
ANO_FINAL   <- 2021L

# TRUE: labels use only positive avoided deaths.
# FALSE: labels use net avoided deaths.
use_positive_only_for_labels <- TRUE

# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c("data.table", "ggplot2", "scales")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(ggplot2)
library(scales)

# ============================================================
# 2. OUTPUT FOLDERS
# ============================================================

if (!dir.exists(base_dir)) {
  warning("base_dir does not exist. Using current working directory instead: ", getwd())
  base_dir <- getwd()
}

out_dir <- file.path(base_dir, "counterfactual_panel_by_age")
dir_fig <- file.path(out_dir, "figures")
dir_tab <- file.path(out_dir, "tables")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "NULL", "NaN", "Inf", "-Inf")] <- NA_character_
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(" ", "", x, fixed = TRUE)

  has_comma <- grepl(",", x, fixed = TRUE)

  # Brazilian decimal format: 1.234,56 -> 1234.56.
  # Uses fixed = TRUE; no regex escape is needed.
  x[has_comma] <- gsub(".", "", x[has_comma], fixed = TRUE)
  x[has_comma] <- gsub(",", ".", x[has_comma], fixed = TRUE)

  suppressWarnings(as.numeric(x))
}

age_label_en <- function(x) {
  x <- as.character(x)
  out <- x

  out[x %in% c("Geral <5 anos", "00-59 meses", "0-59 meses")] <- "Overall <5 years"
  out[x %in% c("00-11 meses", "0-11 meses")] <- "0-11 months"
  out[x %in% c("12-23 meses")] <- "12-23 months"
  out[x %in% c("24-35 meses")] <- "24-35 months"
  out[x %in% c("36-47 meses")] <- "36-47 months"
  out[x %in% c("48-59 meses")] <- "48-59 months"

  out
}

clinical_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, lineheight = 1.05),
      plot.caption = ggplot2::element_text(size = base_size - 3, color = "grey35", hjust = 0),
      axis.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

fmt_big <- scales::label_comma(accuracy = 1)

find_temporal_file <- function(base_dir, input_file_manual = NA_character_) {

  if (!is.na(input_file_manual) && nzchar(input_file_manual) && file.exists(input_file_manual)) {
    return(normalizePath(input_file_manual, winslash = "/", mustWork = TRUE))
  }

  candidate_dirs <- unique(c(
    base_dir,
    getwd(),
    dirname(base_dir),
    file.path(base_dir, "modelo_rmt_dicotomico_base_anual_v9", "v10_phc_en_2lines_same_y", "tabelas"),
    file.path(base_dir, "v10_phc_en_2lines_same_y", "tabelas"),
    file.path(dirname(base_dir), "modelo_rmt_dicotomico_base_anual_v9", "v10_phc_en_2lines_same_y", "tabelas"),
    file.path(dirname(base_dir), "v10_phc_en_2lines_same_y", "tabelas")
  ))

  candidate_files <- unique(c(
    file.path(candidate_dirs, "04_time_2lines_change.csv"),
    file.path(candidate_dirs, "04_time_2lines_change(1).csv"),
    file.path(candidate_dirs, "04_time_2lines_change(2).csv")
  ))

  found <- candidate_files[file.exists(candidate_files)]
  if (length(found) > 0) {
    return(normalizePath(found[1], winslash = "/", mustWork = TRUE))
  }

  roots <- unique(c(base_dir, getwd(), dirname(base_dir)))
  roots <- roots[dir.exists(roots)]

  recursive_found <- character(0)

  for (rr in roots) {
    files <- tryCatch(
      list.files(
        rr,
        pattern = "^04_time_2lines_change.*[.]csv$",
        recursive = TRUE,
        full.names = TRUE
      ),
      error = function(e) character(0)
    )
    recursive_found <- c(recursive_found, files)
  }

  recursive_found <- unique(recursive_found[file.exists(recursive_found)])
  if (length(recursive_found) > 0) {
    return(normalizePath(recursive_found[1], winslash = "/", mustWork = TRUE))
  }

  if (interactive()) {
    message("Could not find 04_time_2lines_change.csv automatically.")
    message("Please select the file manually.")
    selected <- file.choose()
    if (!is.na(selected) && file.exists(selected)) {
      return(normalizePath(selected, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Could not find '04_time_2lines_change.csv'.\n",
    "Set input_file_manual to the full path or place the file inside base_dir.\n",
    "Current base_dir: ", base_dir, "\n",
    "Current getwd(): ", getwd()
  )
}

save_plot <- function(plot, filename, width, height, dpi = 320) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
  invisible(filename)
}

# ============================================================
# 4. READ TEMPORAL DATA
# ============================================================

if (exists("temporal3", envir = .GlobalEnv)) {
  message("Using object 'temporal3' from the global environment.")
  dt <- data.table::as.data.table(data.table::copy(get("temporal3", envir = .GlobalEnv)))
  input_file <- "global_object: temporal3"
} else {
  input_file <- find_temporal_file(base_dir, input_file_manual)
  message("Using input file: ", input_file)
  dt <- data.table::fread(input_file)
}

setDT(dt)

if (!"faixa_analise" %in% names(dt) && "faixa_etaria" %in% names(dt)) {
  dt[, faixa_analise := faixa_etaria]
}

if (!"grupo3_clinico" %in% names(dt) && "grupo" %in% names(dt)) {
  dt[, grupo3_clinico := grupo]
}

required_cols <- c(
  "faixa_analise",
  "marcador_id",
  "marcador_label",
  "ano",
  "grupo3_clinico",
  "obitos",
  "risco",
  "taxa_1000"
)

missing_cols <- setdiff(required_cols, names(dt))

if (length(missing_cols) > 0) {
  stop("Missing columns in temporal input: ", paste(missing_cols, collapse = ", "))
}

dt[, ano := as.integer(to_num(ano))]
dt[, faixa_analise := as.character(faixa_analise)]
dt[, marcador_id := as.character(marcador_id)]
dt[, marcador_label := as.character(marcador_label)]
dt[, grupo3_clinico := as.character(grupo3_clinico)]
dt[, obitos := to_num(obitos)]
dt[, risco := to_num(risco)]
dt[, taxa_1000 := to_num(taxa_1000)]

dt <- dt[
  ano >= ANO_INICIAL &
    ano <= ANO_FINAL &
    grupo3_clinico %in% c("Policy only", "Policy + PHC") &
    is.finite(ano) &
    is.finite(obitos) &
    is.finite(risco) &
    risco > 0 &
    is.finite(taxa_1000)
]

if (nrow(dt) == 0) {
  stop("No valid observations after filtering Policy only and Policy + PHC.")
}

# Safe internal group names.
dt[, group_key := data.table::fifelse(
  grupo3_clinico == "Policy only",
  "policy_only",
  "policy_phc"
)]

# ============================================================
# 5. CUMULATIVE COUNTERFACTUAL CALCULATION
# ============================================================

setorder(dt, faixa_analise, marcador_id, group_key, ano)

ref <- dt[
  is.finite(taxa_1000) & taxa_1000 > 0,
  .SD[1],
  by = .(faixa_analise, marcador_id, group_key)
][
  ,
  .(
    faixa_analise,
    marcador_id,
    group_key,
    reference_year = ano,
    reference_rate = taxa_1000
  )
]

dt <- merge(
  dt,
  ref,
  by = c("faixa_analise", "marcador_id", "group_key"),
  all.x = TRUE
)

dt[, cumulative_index := data.table::fifelse(
  is.finite(reference_rate) & reference_rate > 0,
  taxa_1000 / reference_rate,
  NA_real_
)]

wide <- data.table::dcast(
  dt[
    ,
    .(
      faixa_analise,
      marcador_id,
      marcador_label,
      ano,
      group_key,
      obitos,
      risco,
      taxa_1000,
      reference_rate,
      cumulative_index
    )
  ],
  faixa_analise + marcador_id + marcador_label + ano ~ group_key,
  value.var = c("obitos", "risco", "taxa_1000", "reference_rate", "cumulative_index")
)

needed <- c(
  "obitos_policy_only",
  "risco_policy_only",
  "reference_rate_policy_only",
  "cumulative_index_policy_phc"
)

miss <- setdiff(needed, names(wide))

if (length(miss) > 0) {
  stop(
    "Counterfactual computation failed after reshaping. Missing columns: ",
    paste(miss, collapse = ", ")
  )
}

wide[, counterfactual_rate := reference_rate_policy_only * cumulative_index_policy_phc]
wide[, counterfactual_deaths := counterfactual_rate * risco_policy_only / 1000]
wide[, deaths_avoided := obitos_policy_only - counterfactual_deaths]
wide[, deaths_avoided_positive := pmax(deaths_avoided, 0)]
wide[, age_group := age_label_en(faixa_analise)]

wide <- wide[
  is.finite(obitos_policy_only) &
    is.finite(counterfactual_deaths) &
    is.finite(deaths_avoided)
]

# ============================================================
# 6. AGGREGATION BY AGE GROUP
# ============================================================

age_order <- c(
  "0-11 months",
  "12-23 months",
  "24-35 months",
  "36-47 months",
  "48-59 months",
  "Overall <5 years"
)

annual_age <- wide[
  ,
  .(
    observed_deaths = sum(obitos_policy_only, na.rm = TRUE),
    counterfactual_deaths = sum(counterfactual_deaths, na.rm = TRUE),
    deaths_avoided = sum(deaths_avoided, na.rm = TRUE),
    deaths_avoided_positive = sum(deaths_avoided_positive, na.rm = TRUE)
  ),
  by = .(ano, age_group)
]

setorder(annual_age, age_group, ano)

annual_age[
  ,
  cumulative_deaths_avoided := cumsum(deaths_avoided),
  by = age_group
]

annual_age[
  ,
  cumulative_deaths_avoided_positive := cumsum(deaths_avoided_positive),
  by = age_group
]

annual_age[
  ,
  age_group := factor(age_group, levels = age_order)
]

summary_age <- annual_age[
  ,
  .(
    observed_deaths_total = sum(observed_deaths, na.rm = TRUE),
    counterfactual_deaths_total = sum(counterfactual_deaths, na.rm = TRUE),
    deaths_avoided_total = sum(deaths_avoided, na.rm = TRUE),
    deaths_avoided_total_positive_only = sum(deaths_avoided_positive, na.rm = TRUE),
    first_year = min(ano, na.rm = TRUE),
    last_year = max(ano, na.rm = TRUE)
  ),
  by = .(age_group)
]

summary_age[
  ,
  label_total := paste0(
    "Avoided deaths: ",
    scales::comma(
      round(
        if (use_positive_only_for_labels) {
          deaths_avoided_total_positive_only
        } else {
          deaths_avoided_total
        }
      ),
      accuracy = 1
    )
  )
]

data.table::fwrite(
  wide,
  file.path(dir_tab, "counterfactual_policy_age_year.csv")
)

data.table::fwrite(
  annual_age,
  file.path(dir_tab, "counterfactual_annual_by_age.csv")
)

data.table::fwrite(
  summary_age,
  file.path(dir_tab, "counterfactual_summary_by_age.csv")
)

# ============================================================
# 7. DIDACTIC PANEL: OBSERVED VS COUNTERFACTUAL BY AGE
# ============================================================

panel_data <- annual_age[age_group != "Overall <5 years"]

ann <- summary_age[age_group != "Overall <5 years"]
ann[, x := last_year - 1L]

ann <- merge(
  ann,
  panel_data[
    ,
    .(y = max(pmax(observed_deaths, counterfactual_deaths), na.rm = TRUE) * 0.94),
    by = age_group
  ],
  by = "age_group",
  all.x = TRUE
)

p_panel <- ggplot(panel_data, aes(x = ano)) +
  geom_ribbon(
    aes(
      ymin = pmin(observed_deaths, counterfactual_deaths),
      ymax = pmax(observed_deaths, counterfactual_deaths)
    ),
    fill = "#DDEAF7",
    alpha = 0.70
  ) +
  geom_line(
    aes(y = observed_deaths, color = "Observed deaths"),
    linewidth = 1.10
  ) +
  geom_point(
    aes(y = observed_deaths, color = "Observed deaths"),
    size = 1.20
  ) +
  geom_line(
    aes(y = counterfactual_deaths, color = "Counterfactual deaths"),
    linewidth = 1.05,
    linetype = "22"
  ) +
  geom_point(
    aes(y = counterfactual_deaths, color = "Counterfactual deaths"),
    size = 1.05
  ) +
  geom_text(
    data = ann,
    aes(x = x, y = y, label = label_total),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 1,
    size = 3.45,
    fontface = "bold",
    color = "#1D3557"
  ) +
  scale_color_manual(
    values = c(
      "Observed deaths" = "#C0392B",
      "Counterfactual deaths" = "#1D3557"
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      min(panel_data$ano, na.rm = TRUE),
      max(panel_data$ano, na.rm = TRUE),
      by = 2
    )
  ) +
  scale_y_continuous(labels = fmt_big) +
  facet_wrap(~ age_group, scales = "free_y", ncol = 2) +
  labs(
    title = "Observed deaths and counterfactual deaths if primary health care expansion had accompanied policy expansion",
    subtitle = "Annual deaths in municipalities with policy expansion alone compared with the counterfactual number expected under the cumulative trajectory observed when policy expansion occurred together with primary health care expansion.",
    x = "Year",
    y = "Number of deaths",
    caption = "The shaded area represents the gap between observed and counterfactual deaths. Estimates are descriptive cumulative contrasts and should not be interpreted as causal effects."
  ) +
  clinical_theme(12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(
  plot = p_panel,
  filename = file.path(dir_fig, "counterfactual_panel_observed_vs_counterfactual_by_age.png"),
  width = 13,
  height = 10
)

# ============================================================
# 8. OVERALL UNDER-5 FIGURE
# ============================================================

overall_data <- annual_age[age_group == "Overall <5 years"]
overall_lab <- summary_age[age_group == "Overall <5 years"]

if (nrow(overall_data) > 0 && nrow(overall_lab) > 0) {

  overall_total <- if (use_positive_only_for_labels) {
    overall_lab$deaths_avoided_total_positive_only[1]
  } else {
    overall_lab$deaths_avoided_total[1]
  }

  p_overall <- ggplot(overall_data, aes(x = ano)) +
    geom_ribbon(
      aes(
        ymin = pmin(observed_deaths, counterfactual_deaths),
        ymax = pmax(observed_deaths, counterfactual_deaths)
      ),
      fill = "#DDEAF7",
      alpha = 0.75
    ) +
    geom_line(
      aes(y = observed_deaths, color = "Observed deaths"),
      linewidth = 1.25
    ) +
    geom_point(
      aes(y = observed_deaths, color = "Observed deaths"),
      size = 1.40
    ) +
    geom_line(
      aes(y = counterfactual_deaths, color = "Counterfactual deaths"),
      linewidth = 1.15,
      linetype = "22"
    ) +
    geom_point(
      aes(y = counterfactual_deaths, color = "Counterfactual deaths"),
      size = 1.25
    ) +
    annotate(
      "label",
      x = max(overall_data$ano, na.rm = TRUE) - 1,
      y = max(pmax(overall_data$observed_deaths, overall_data$counterfactual_deaths), na.rm = TRUE) * 0.96,
      label = paste0(
        "Cumulative avoided deaths: ",
        scales::comma(round(overall_total), accuracy = 1)
      ),
      hjust = 1,
      vjust = 1,
      size = 4.0,
      fontface = "bold",
      label.size = 0.25,
      fill = "white",
      color = "#1D3557"
    ) +
    scale_color_manual(
      values = c(
        "Observed deaths" = "#C0392B",
        "Counterfactual deaths" = "#1D3557"
      )
    ) +
    scale_x_continuous(
      breaks = seq(
        min(overall_data$ano, na.rm = TRUE),
        max(overall_data$ano, na.rm = TRUE),
        by = 2
      )
    ) +
    scale_y_continuous(labels = fmt_big) +
    labs(
      title = "Overall under-5 deaths: observed and counterfactual",
      subtitle = "Municipalities with policy expansion alone under the cumulative mortality trajectory observed when policy expansion occurred together with primary health care expansion.",
      x = "Year",
      y = "Number of deaths",
      caption = "The shaded area represents the annual gap between observed and counterfactual deaths. Estimates are descriptive cumulative contrasts across policy scenarios."
    ) +
    clinical_theme(12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_plot(
    plot = p_overall,
    filename = file.path(dir_fig, "counterfactual_overall_under5_observed_vs_counterfactual.png"),
    width = 12,
    height = 6.5
  )
}

# ============================================================
# 9. COMPLEMENTARY FIGURE: ANNUAL AVOIDED DEATHS
# ============================================================

p_avoided <- ggplot(
  panel_data,
  aes(x = ano, y = deaths_avoided_positive, group = 1)
) +
  geom_col(fill = "#4E79A7", width = 0.70) +
  geom_line(color = "#1D3557", linewidth = 0.80) +
  scale_x_continuous(
    breaks = seq(
      min(panel_data$ano, na.rm = TRUE),
      max(panel_data$ano, na.rm = TRUE),
      by = 2
    )
  ) +
  scale_y_continuous(labels = fmt_big) +
  facet_wrap(~ age_group, scales = "free_y", ncol = 2) +
  labs(
    title = "Annual deaths that might have been avoided with concurrent primary health care expansion",
    subtitle = "Positive annual differences between observed deaths in municipalities with policy expansion alone and counterfactual deaths expected under the policy plus primary health care trajectory.",
    x = "Year",
    y = "Deaths avoided",
    caption = "Negative annual differences were truncated to zero in this descriptive display."
  ) +
  clinical_theme(12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(
  plot = p_avoided,
  filename = file.path(dir_fig, "counterfactual_panel_annual_deaths_avoided_by_age.png"),
  width = 13,
  height = 10
)

# ============================================================
# 10. FINAL MESSAGES
# ============================================================

message("Done.")
message("Input source: ", input_file)
message("Main panel: ", file.path(dir_fig, "counterfactual_panel_observed_vs_counterfactual_by_age.png"))
message("Overall figure: ", file.path(dir_fig, "counterfactual_overall_under5_observed_vs_counterfactual.png"))
message("Annual avoided deaths panel: ", file.path(dir_fig, "counterfactual_panel_annual_deaths_avoided_by_age.png"))
message("Tables: ", dir_tab)
