# ============================================================
# MODEL V10 - CLINICAL VERSION, ENGLISH FIGURES
# PHC as an effect modifier of policy expansion
#
# Main display:
#   Two clinical trajectories only:
#   1) Policy only
#   2) Policy + PHC
#
# The "No increase" group is kept in the model as the reference structure,
# but it is removed from temporal figures to simplify clinical interpretation.
#
# Temporal outcome:
#   Percent change in mortality rate from the first observed year of each series.
#
# Input:
#   1) object in memory: base_mudanca, produced by V9; OR
#   2) file:
#      modelo_rmt_dicotomico_base_anual_v9/tabelas/base_mudanca_anual_log_razao_taxa.csv
#
# Model:
#   log_razao_taxa ~ politica_cat * aps_cat * periodo_estagnacao
#                    + ano_c + z_log_taxa_lag
#
# Main contrast:
#   PHC effect modification during the stagnation period.
#
# Output:
#   modelo_rmt_dicotomico_base_anual_v9/v10_phc_en_2lines_same_y/
# ============================================================


# ============================================================
# 1. GENERAL SETTINGS
# ============================================================

if (!exists("base_dir")) {
  base_dir <- getwd()
}

ano_inicio_estagnacao <- 2015L
correcao_obitos_log <- 0.5

min_linhas_modelo <- 150L
min_municipios_modelo <- 50L
min_anos_modelo <- 5L

categorias_modelo <- c("Nao teve aumento", "Teve aumento")
rotulo_geral_menor5 <- "Geral <5 anos"

# Policies evaluated; PHC is the modifier, not the policy exposure.
marcadores_politica <- c(
  "vac_bcg_pct",
  "vac_penta_pct",
  "vac_pneumococica_pct",
  "vac_rotavirus_pct",
  "agua_pct",
  "esgoto_pct",
  "bf_beneficios_por_1000_crianca"
)

# Figures
fig_dpi <- 300
fig_width <- 11.5
fig_height <- 7.0
fig_width_forest <- 10.5
fig_height_forest_min <- 6.0
fig_width_painel <- 12.5
fig_height_painel <- 10.0

# Visual smoothing for temporal figures.
loess_span <- 0.55
mostrar_pontos_temporal <- FALSE
mostrar_linha_bruta_fina <- FALSE

# Temporal display: common axes across all individual time plots and panels.
# This improves visual comparability across age groups and policies.
x_break_step <- 2L
y_break_step <- 20L
y_axis_round_to <- 10L

# Short file names to avoid Windows/OneDrive path issues.
max_nome_base <- 42L

pacotes <- c(
  "data.table",
  "ggplot2",
  "sandwich",
  "openxlsx",
  "scales"
)

options(stringsAsFactors = FALSE)
options(warn = 1)

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1"
)


# ============================================================
# 2. PACOTES
# ============================================================

instalar_carregar_pacotes <- function(pacotes) {
  if (is.null(getOption("repos")) ||
      is.na(getOption("repos")["CRAN"]) ||
      getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  faltantes <- pacotes[
    !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(faltantes) > 0) {
    install.packages(faltantes)
  }

  invisible(lapply(pacotes, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

instalar_carregar_pacotes(pacotes)


# ============================================================
# 3. FUNCOES AUXILIARES
# ============================================================

rbindlist_seguro <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) return(data.table::data.table())
  data.table::rbindlist(x, fill = TRUE)
}

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "NULL", "NaN", "Inf", "-Inf")] <- NA_character_
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(" ", "", x, fixed = TRUE)

  tem_virgula <- grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub("\\.", "", x[tem_virgula])
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)

  suppressWarnings(as.numeric(x))
}

fmt_num <- function(x, digits = 2) {
  out <- rep("", length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- formatC(
    x[ok],
    format = "f",
    digits = digits,
    decimal.mark = ".",
    big.mark = ","
  )
  out
}

fmt_p <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.001] <- "<0.001"
  out[!is.na(p) & p >= 0.001] <- formatC(
    p[!is.na(p) & p >= 0.001],
    format = "f",
    digits = 3,
    decimal.mark = "."
  )
  out
}

pad_code6 <- function(x) {
  x <- as.character(x)
  x <- gsub("\\D", "", x)
  x[x == ""] <- NA_character_

  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)

  out[ok & nchar(x) >= 7] <- substr(x[ok & nchar(x) >= 7], 1, 6)
  out[ok & nchar(x) == 6] <- x[ok & nchar(x) == 6]
  out[ok & nchar(x) < 6] <- sprintf("%06s", x[ok & nchar(x) < 6])

  out
}

slug_curto <- function(x, max_len = max_nome_base) {
  x <- paste(as.character(x), collapse = "_")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (is.na(x) || x == "") x <- "fig"
  substr(x, 1, max_len)
}

arquivo_curto <- function(dir, prefixo, id = NULL, ext = ".png") {
  base <- slug_curto(paste(c(prefixo, id), collapse = "_"), max_len = max_nome_base)
  # Extensao adicionada depois do truncamento para preservar .png.
  file.path(dir, paste0(base, ext))
}

sanitize_for_export <- function(dt) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  if (nrow(dt) == 0 && ncol(dt) == 0) {
    return(data.table::data.table(info = "no_results"))
  }

  for (nm in names(dt)) {
    if (is.list(dt[[nm]]) && !is.data.frame(dt[[nm]])) {
      dt[[nm]] <- vapply(
        dt[[nm]],
        function(z) paste(as.character(z), collapse = " | "),
        character(1)
      )
    }

    if (inherits(dt[[nm]], c("Date", "POSIXct", "POSIXt"))) {
      dt[[nm]] <- as.character(dt[[nm]])
    }
  }

  dt
}

dir_create_safe <- function(path) {
  if (!dir.exists(path)) {
    ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(path)) {
      stop("Could not create directory: ", path)
    }
  }
  invisible(path)
}

fwrite_safe <- function(dt, file) {
  dt <- sanitize_for_export(dt)
  dir_create_safe(dirname(file))
  data.table::fwrite(dt, file = file)
  invisible(file)
}

add_sheet_safe <- function(wb, sheet, dt) {
  dt <- sanitize_for_export(dt)
  sheet <- substr(gsub("[^A-Za-z0-9_]+", "_", sheet), 1, 31)

  if (sheet %in% openxlsx::sheets(wb)) {
    sheet <- substr(
      paste0(substr(sheet, 1, 25), "_", length(openxlsx::sheets(wb)) + 1),
      1,
      31
    )
  }

  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, as.data.frame(dt))
  invisible(wb)
}

criar_png_sem_resultado <- function(file, titulo, subtitulo) {
  dir_create_safe(dirname(file))

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.15,
      label = titulo,
      size = 5.2,
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = -0.10,
      label = subtitulo,
      size = 3.8
    ) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()

  ggplot2::ggsave(
    filename = file,
    plot = p,
    width = 10,
    height = 6,
    dpi = fig_dpi,
    bg = "white",
    limitsize = FALSE
  )

  invisible(file)
}


# ============================================================
# 4. LOCALIZAR PASTA V9 SEM HERDAR OUT_DIR ERRADO
# ============================================================

arquivo_v9_rel <- file.path("tabelas", "base_mudanca_anual_log_razao_taxa.csv")

encontrar_dir_v9 <- function(base_dir) {
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)

  candidatos <- unique(c(
    file.path(base_dir, "modelo_rmt_dicotomico_base_anual_v9"),
    base_dir,
    dirname(base_dir),
    file.path(dirname(base_dir), "modelo_rmt_dicotomico_base_anual_v9"),
    file.path(dirname(dirname(base_dir)), "modelo_rmt_dicotomico_base_anual_v9")
  ))

  candidatos <- candidatos[!is.na(candidatos) & nzchar(candidatos)]

  for (cand in candidatos) {
    arq <- file.path(cand, arquivo_v9_rel)
    if (file.exists(arq)) {
      return(normalizePath(cand, winslash = "/", mustWork = TRUE))
    }
  }

  # Busca controlada no diretorio atual e um nivel abaixo.
  possiveis <- list.dirs(base_dir, recursive = TRUE, full.names = TRUE)
  possiveis <- possiveis[basename(possiveis) == "modelo_rmt_dicotomico_base_anual_v9"]

  for (cand in possiveis) {
    arq <- file.path(cand, arquivo_v9_rel)
    if (file.exists(arq)) {
      return(normalizePath(cand, winslash = "/", mustWork = TRUE))
    }
  }

  NA_character_
}

dir_v9 <- encontrar_dir_v9(base_dir)

if (is.na(dir_v9)) {
  if (exists("base_mudanca")) {
    dir_v9 <- file.path(base_dir, "modelo_rmt_dicotomico_base_anual_v9")
  } else {
    stop(
      "Could not find a pasta do V9 com o arquivo: ",
      file.path("modelo_rmt_dicotomico_base_anual_v9", arquivo_v9_rel),
      ". Rode o V9 antes ou defina base_dir como a pasta onde esta a pasta do V9."
    )
  }
}

out_dir_v10 <- file.path(dir_v9, "v10_phc_en_2lines_same_y")

dir_tabelas <- file.path(out_dir_v10, "tabelas")
dir_logs <- file.path(out_dir_v10, "logs")
dir_fig <- file.path(out_dir_v10, "figuras")
dir_fig_temporal3 <- file.path(dir_fig, "time_2lines")
dir_fig_panel <- file.path(dir_fig, "panels_2lines")
dir_fig_forest <- file.path(dir_fig, "forest")

for (d in c(
  out_dir_v10,
  dir_tabelas,
  dir_logs,
  dir_fig,
  dir_fig_temporal3,
  dir_fig_panel,
  dir_fig_forest
)) {
  dir_create_safe(d)
}


# ============================================================
# 5. CATALOGO DE MARCADORES
# ============================================================

catalogo <- data.table::data.table(
  marcador = c(
    "vac_bcg_pct",
    "vac_penta_pct",
    "vac_pneumococica_pct",
    "vac_rotavirus_pct",
    "agua_pct",
    "esgoto_pct",
    "bf_beneficios_por_1000_crianca"
  ),
  id = c("bcg", "penta", "pneumo", "rota", "water", "sewer", "bf"),
  marcador_label = c(
    "BCG",
    "Pentavalent",
    "Pneumococcal",
    "Rotavirus",
    "Water",
    "Sewerage",
    "Bolsa Familia"
  ),
  marcador_label_longo = c(
    "BCG vaccination",
    "Pentavalent vaccination",
    "Pneumococcal vaccination",
    "Rotavirus vaccination",
    "Piped water",
    "Sewerage",
    "Bolsa Familia"
  ),
  classe = c(
    "Vaccination",
    "Vaccination",
    "Vaccination",
    "Vaccination",
    "Sanitation",
    "Sanitation",
    "Social protection"
  )
)

catalogo <- catalogo[marcador %in% marcadores_politica]

# Temporal figures use two clinically interpretable groups only.
# The no-increase group remains available for modeling, but is not plotted.
ordem_grupo3 <- c(
  "Policy only",
  "Policy + PHC"
)

cores_grupo3 <- c(
  "Policy only" = "#F2A900",
  "Policy + PHC" = "#1B9E77"
)


# ============================================================
# 6. CARREGAR BASE DE MUDANCA DO V9
# ============================================================

arquivo_base_mudanca <- file.path(dir_v9, "tabelas", "base_mudanca_anual_log_razao_taxa.csv")

if (exists("base_mudanca")) {
  base0 <- data.table::as.data.table(data.table::copy(base_mudanca))
  origem_base <- "objeto_em_memoria_base_mudanca"
} else if (file.exists(arquivo_base_mudanca)) {
  base0 <- data.table::fread(arquivo_base_mudanca)
  origem_base <- arquivo_base_mudanca
} else {
  stop(
    "Could not find 'base_mudanca' no ambiente nem o arquivo do V9: ",
    arquivo_base_mudanca,
    ". Rode o V9 antes deste script."
  )
}

base0 <- data.table::as.data.table(base0)

if (!"code" %in% names(base0)) stop("A base precisa ter coluna 'code'.")
if (!"ano" %in% names(base0)) stop("A base precisa ter coluna 'ano'.")

if (!"faixa_analise" %in% names(base0) && "faixa_etaria" %in% names(base0)) {
  base0[, faixa_analise := faixa_etaria]
}

if (!"faixa_analise" %in% names(base0)) {
  stop("A base precisa ter 'faixa_analise' ou 'faixa_etaria'.")
}

colunas_obrigatorias <- c(
  "obitos",
  "risco",
  "log_razao_taxa",
  "periodo_estagnacao",
  "ano_c",
  "z_log_taxa_lag"
)

faltantes <- setdiff(colunas_obrigatorias, names(base0))

if (length(faltantes) > 0) {
  stop(
    "Colunas obrigatorias ausentes na base de mudanca: ",
    paste(faltantes, collapse = ", ")
  )
}

base0[, code := pad_code6(code)]
base0[, ano := as.integer(to_num(ano))]
base0[, faixa_analise := as.character(faixa_analise)]
base0[, obitos := to_num(obitos)]
base0[, risco := to_num(risco)]
base0[, log_razao_taxa := to_num(log_razao_taxa)]
base0[, ano_c := to_num(ano_c)]
base0[, z_log_taxa_lag := to_num(z_log_taxa_lag)]

base0[, periodo_estagnacao := as.character(periodo_estagnacao)]
base0[periodo_estagnacao %in% c("Estagnação", "estagnacao", "estagnação"), periodo_estagnacao := "Estagnacao"]
base0[periodo_estagnacao %in% c("Pre-estagnacao", "Pré-estagnação", "pre_estagnacao"), periodo_estagnacao := "Pre_estagnacao"]
base0[, periodo_estagnacao := factor(periodo_estagnacao, levels = c("Pre_estagnacao", "Estagnacao"))]

if (!"taxa_1000" %in% names(base0)) {
  base0[, taxa_1000 := data.table::fifelse(risco > 0, obitos / risco * 1000, NA_real_)]
} else {
  base0[, taxa_1000 := to_num(taxa_1000)]
}

if (!"peso_modelo" %in% names(base0)) {
  if ("obitos_lag" %in% names(base0)) {
    base0[, obitos_lag := to_num(obitos_lag)]
    base0[, var_log_razao_aprox := 1 / (obitos + correcao_obitos_log) + 1 / (obitos_lag + correcao_obitos_log)]
    base0[, peso_modelo := data.table::fifelse(
      is.finite(var_log_razao_aprox) & var_log_razao_aprox > 0,
      1 / var_log_razao_aprox,
      1
    )]
    base0[, peso_modelo := peso_modelo / mean(peso_modelo, na.rm = TRUE)]
    base0[!is.finite(peso_modelo) | is.na(peso_modelo) | peso_modelo <= 0, peso_modelo := 1]
  } else {
    base0[, peso_modelo := 1]
  }
} else {
  base0[, peso_modelo := to_num(peso_modelo)]
  base0[!is.finite(peso_modelo) | is.na(peso_modelo) | peso_modelo <= 0, peso_modelo := 1]
}

aps_col <- if ("aps_esf_pct_aumento_cat_lag" %in% names(base0)) {
  "aps_esf_pct_aumento_cat_lag"
} else if ("aps_esf_pct_aumento_cat" %in% names(base0)) {
  "aps_esf_pct_aumento_cat"
} else {
  NA_character_
}

if (is.na(aps_col)) {
  stop(
    "Could not find the PHC expansion category column: ",
    "aps_esf_pct_aumento_cat_lag or aps_esf_pct_aumento_cat."
  )
}

politicas_disponiveis <- catalogo[
  paste0(marcador, "_aumento_cat_lag") %in% names(base0) |
    paste0(marcador, "_aumento_cat") %in% names(base0)
]

if (nrow(politicas_disponiveis) == 0) {
  stop("No policy expansion category column was found in the input data.")
}

faixa_levels <- c(
  rotulo_geral_menor5,
  sort(setdiff(unique(base0$faixa_analise), rotulo_geral_menor5))
)
faixa_levels <- faixa_levels[faixa_levels %in% unique(base0$faixa_analise)]

age_label_en <- function(x) {
  x <- as.character(x)
  out <- x
  out[x %in% c("Geral <5 anos", "00-59 meses")] <- "Overall <5 years"
  out[x %in% c("00-11 meses", "0-11 meses")] <- "0-11 months"
  out[x %in% c("12-23 meses")] <- "12-23 months"
  out[x %in% c("24-35 meses")] <- "24-35 months"
  out[x %in% c("36-47 meses")] <- "36-47 months"
  out[x %in% c("48-59 meses")] <- "48-59 months"
  out
}

faixa_map <- data.table::data.table(
  faixa_analise = faixa_levels,
  faixa_id = sprintf("fx%02d", seq_along(faixa_levels))
)
faixa_map[, faixa_label_en := age_label_en(faixa_analise)]

fwrite_safe(
  data.table::data.table(
    origem_base = origem_base,
    dir_v9 = dir_v9,
    out_dir_v10 = out_dir_v10,
    n_linhas = nrow(base0),
    n_municipios = data.table::uniqueN(base0$code),
    ano_min = min(base0$ano, na.rm = TRUE),
    ano_max = max(base0$ano, na.rm = TRUE),
    n_faixas = data.table::uniqueN(base0$faixa_analise),
    aps_col = aps_col
  ),
  file.path(dir_logs, "00_input.csv")
)

fwrite_safe(politicas_disponiveis, file.path(dir_logs, "01_policies.csv"))
fwrite_safe(faixa_map, file.path(dir_logs, "02_age_groups.csv"))


# ============================================================
# 7. PREPARE LONG POLICY x PHC DATASET
# ============================================================

preparar_base_politica <- function(dt,
                                   cat_politica_col,
                                   marcador,
                                   marcador_id,
                                   marcador_label,
                                   marcador_label_longo,
                                   classe) {
  d <- data.table::copy(dt)

  d[, aps_cat_raw := as.character(get(aps_col))]
  d[, politica_cat_raw := as.character(get(cat_politica_col))]

  d <- d[
    aps_cat_raw %in% categorias_modelo &
      politica_cat_raw %in% categorias_modelo &
      !is.na(code) &
      !is.na(ano) &
      !is.na(faixa_analise) &
      is.finite(obitos) &
      is.finite(risco) &
      risco > 0 &
      is.finite(taxa_1000) &
      is.finite(log_razao_taxa) &
      is.finite(ano_c) &
      is.finite(z_log_taxa_lag) &
      is.finite(peso_modelo) &
      !is.na(periodo_estagnacao)
  ]

  d[, aps_cat := factor(aps_cat_raw, levels = categorias_modelo)]
  d[, politica_cat := factor(politica_cat_raw, levels = categorias_modelo)]

  d[, grupo4 := data.table::fcase(
    politica_cat_raw == "Nao teve aumento" & aps_cat_raw == "Nao teve aumento", "No increase",
    politica_cat_raw == "Nao teve aumento" & aps_cat_raw == "Teve aumento", "PHC only",
    politica_cat_raw == "Teve aumento" & aps_cat_raw == "Nao teve aumento", "Policy only",
    politica_cat_raw == "Teve aumento" & aps_cat_raw == "Teve aumento", "Policy + PHC",
    default = NA_character_
  )]

  d[, grupo4 := factor(
    grupo4,
    levels = c(
      "No increase",
      "PHC only",
      "Policy only",
      "Policy + PHC"
    )
  )]

  # Main temporal display: policy expansion alone vs policy expansion with PHC.
  d[, grupo3_clinico := data.table::fcase(
    grupo4 == "Policy only", "Policy only",
    grupo4 == "Policy + PHC", "Policy + PHC",
    default = NA_character_
  )]

  d[, grupo3_clinico := factor(grupo3_clinico, levels = ordem_grupo3)]

  d[, `:=`(
    marcador = marcador,
    marcador_id = marcador_id,
    marcador_label = marcador_label,
    marcador_label_longo = marcador_label_longo,
    classe = classe
  )]

  d
}

lista_bases <- vector("list", nrow(politicas_disponiveis))

for (i in seq_len(nrow(politicas_disponiveis))) {
  mk <- politicas_disponiveis$marcador[i]
  col_lag <- paste0(mk, "_aumento_cat_lag")
  col_now <- paste0(mk, "_aumento_cat")
  cat_col <- if (col_lag %in% names(base0)) col_lag else col_now

  lista_bases[[i]] <- preparar_base_politica(
    dt = base0,
    cat_politica_col = cat_col,
    marcador = mk,
    marcador_id = politicas_disponiveis$id[i],
    marcador_label = politicas_disponiveis$marcador_label[i],
    marcador_label_longo = politicas_disponiveis$marcador_label_longo[i],
    classe = politicas_disponiveis$classe[i]
  )
}

base_v10 <- rbindlist_seguro(lista_bases)

if (nrow(base_v10) == 0) {
  stop("The V10 dataset is empty after classifying policy and PHC expansion.")
}

base_v10 <- merge(base_v10, faixa_map, by = "faixa_analise", all.x = TRUE)
base_v10[is.na(faixa_id), faixa_id := vapply(faixa_analise, slug_curto, character(1), max_len = 8)]

fwrite_safe(base_v10, file.path(dir_tabelas, "00_long_policy_phc.csv"))

resumo_grupos4 <- base_v10[, .(
  n_linhas = .N,
  n_municipios = data.table::uniqueN(code),
  n_anos = data.table::uniqueN(ano),
  obitos = sum(obitos, na.rm = TRUE),
  risco = sum(risco, na.rm = TRUE)
), by = .(faixa_analise, faixa_id, marcador_id, marcador_label, grupo4)]

resumo_grupos3 <- base_v10[
  !is.na(grupo3_clinico),
  .(
    n_linhas = .N,
    n_municipios = data.table::uniqueN(code),
    n_anos = data.table::uniqueN(ano),
    obitos = sum(obitos, na.rm = TRUE),
    risco = sum(risco, na.rm = TRUE)
  ),
  by = .(faixa_analise, faixa_id, marcador_id, marcador_label, grupo3_clinico)
]

fwrite_safe(resumo_grupos4, file.path(dir_tabelas, "01_groups_4cells.csv"))
fwrite_safe(resumo_grupos3, file.path(dir_tabelas, "02_groups_2lines.csv"))


# ============================================================
# 8. MODELOS E CONTRASTES
# ============================================================

formula_v10 <- stats::as.formula(
  "log_razao_taxa ~ politica_cat * aps_cat * periodo_estagnacao + ano_c + z_log_taxa_lag"
)

fit_lm_safe <- function(d) {
  tryCatch(
    {
      fit <- stats::lm(formula_v10, data = d, weights = peso_modelo)
      list(fit = fit, erro = NA_character_)
    },
    error = function(e) list(fit = NULL, erro = conditionMessage(e))
  )
}

vcov_cluster_safe <- function(fit, cluster) {
  tryCatch(
    sandwich::vcovCL(fit, cluster = cluster, type = "HC1"),
    error = function(e) {
      warning("Falha no vcovCL; usando vcov convencional: ", conditionMessage(e))
      stats::vcov(fit)
    }
  )
}

linear_contrast <- function(fit, vc, newdata_list, weights) {
  tryCatch(
    {
      termos <- stats::delete.response(stats::terms(fit))
      beta <- stats::coef(fit)
      beta <- beta[is.finite(beta)]

      coef_validos <- intersect(names(beta), rownames(vc))
      coef_validos <- intersect(coef_validos, colnames(vc))

      if (length(coef_validos) == 0) {
        stop("No valid coefficient was found for the contrast.")
      }

      l_total <- numeric(length(coef_validos))
      names(l_total) <- coef_validos

      for (i in seq_along(newdata_list)) {
        nd <- as.data.frame(newdata_list[[i]])
        mm <- stats::model.matrix(termos, nd)

        raw <- as.numeric(mm[1, , drop = TRUE])
        names(raw) <- colnames(mm)

        comuns <- intersect(names(raw), coef_validos)
        l_total[comuns] <- l_total[comuns] + weights[i] * raw[comuns]
      }

      v <- vc[coef_validos, coef_validos, drop = FALSE]
      b <- beta[coef_validos]

      est <- sum(l_total * b)
      var_est <- as.numeric(t(matrix(l_total, ncol = 1)) %*% v %*% matrix(l_total, ncol = 1))
      se <- ifelse(is.finite(var_est) && var_est >= 0, sqrt(var_est), NA_real_)

      t_val <- est / se
      gl <- stats::df.residual(fit)
      p <- 2 * stats::pt(abs(t_val), df = gl, lower.tail = FALSE)

      data.table::data.table(
        estimativa_log = est,
        erro_padrao = se,
        ic95_inf_log = est - 1.96 * se,
        ic95_sup_log = est + 1.96 * se,
        t = t_val,
        p_valor = p,
        erro_contraste = NA_character_
      )
    },
    error = function(e) {
      data.table::data.table(
        estimativa_log = NA_real_,
        erro_padrao = NA_real_,
        ic95_inf_log = NA_real_,
        ic95_sup_log = NA_real_,
        t = NA_real_,
        p_valor = NA_real_,
        erro_contraste = conditionMessage(e)
      )
    }
  )
}

finalizar_contraste <- function(dt) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  dt[, medida := exp(estimativa_log)]
  dt[, ic95_inf := exp(ic95_inf_log)]
  dt[, ic95_sup := exp(ic95_sup_log)]

  dt[, variacao_relativa_pct := (medida - 1) * 100]
  dt[, medida_ic95 := paste0(
    fmt_num(medida, 2),
    " (",
    fmt_num(ic95_inf, 2),
    ", ",
    fmt_num(ic95_sup, 2),
    ")"
  )]
  dt[, p_formatado := fmt_p(p_valor)]

  dt[, interpretacao := data.table::fcase(
    !is.na(medida) & medida < 1 & !is.na(p_valor) & p_valor < 0.05,
    "More favorable in the compared group",
    !is.na(medida) & medida < 1,
    "More favorable direction, not statistically significant",
    !is.na(medida) & medida > 1 & !is.na(p_valor) & p_valor < 0.05,
    "Less favorable in the compared group",
    !is.na(medida) & medida > 1,
    "Less favorable direction, not statistically significant",
    default = "Inconclusive"
  )]

  dt
}

make_nd <- function(d, politica, aps, periodo, ano_ref) {
  data.table::data.table(
    politica_cat = factor(politica, levels = levels(d$politica_cat)),
    aps_cat = factor(aps, levels = levels(d$aps_cat)),
    periodo_estagnacao = factor(periodo, levels = levels(d$periodo_estagnacao)),
    ano_c = as.numeric(ano_ref - ano_inicio_estagnacao),
    z_log_taxa_lag = 0
  )
}

rodar_modelos_v10 <- function(base_long) {
  lista_res <- list()
  lista_log <- list()
  ir <- 1L
  il <- 1L

  chaves <- unique(base_long[, .(
    faixa_analise,
    faixa_id,
    marcador,
    marcador_id,
    marcador_label,
    marcador_label_longo,
    classe
  )])

  for (i in seq_len(nrow(chaves))) {
    fx <- chaves$faixa_analise[i]
    fxid <- chaves$faixa_id[i]
    mk <- chaves$marcador[i]
    mkid <- chaves$marcador_id[i]
    mklab <- chaves$marcador_label[i]
    mklab_long <- chaves$marcador_label_longo[i]
    classe_i <- chaves$classe[i]

    d <- data.table::copy(base_long[faixa_analise == fx & marcador == mk])

    d <- d[
      is.finite(log_razao_taxa) &
        is.finite(ano_c) &
        is.finite(z_log_taxa_lag) &
        is.finite(peso_modelo) &
        !is.na(politica_cat) &
        !is.na(aps_cat) &
        !is.na(periodo_estagnacao)
    ]

    d[, politica_cat := factor(as.character(politica_cat), levels = categorias_modelo)]
    d[, aps_cat := factor(as.character(aps_cat), levels = categorias_modelo)]
    d[, periodo_estagnacao := factor(
      as.character(periodo_estagnacao),
      levels = c("Pre_estagnacao", "Estagnacao")
    )]

    cond_ok <- nrow(d) >= min_linhas_modelo &&
      data.table::uniqueN(d$code) >= min_municipios_modelo &&
      data.table::uniqueN(d$ano) >= min_anos_modelo &&
      all(categorias_modelo %in% as.character(d$politica_cat)) &&
      all(categorias_modelo %in% as.character(d$aps_cat)) &&
      all(c("Pre_estagnacao", "Estagnacao") %in% as.character(d$periodo_estagnacao))

    if (!cond_ok) {
      lista_log[[il]] <- data.table::data.table(
        faixa_analise = fx,
        faixa_id = fxid,
        marcador = mk,
        marcador_id = mkid,
        marcador_label = mklab,
        status = "not_run",
        detalhe = "Insufficient sample size or missing categories/periods.",
        n_linhas = nrow(d),
        n_municipios = data.table::uniqueN(d$code),
        n_anos = data.table::uniqueN(d$ano),
        n_grupos4 = data.table::uniqueN(d$grupo4),
        n_grupos3 = data.table::uniqueN(d$grupo3_clinico, na.rm = TRUE)
      )
      il <- il + 1L
      next
    }

    fit_obj <- fit_lm_safe(d)

    if (is.null(fit_obj$fit)) {
      lista_log[[il]] <- data.table::data.table(
        faixa_analise = fx,
        faixa_id = fxid,
        marcador = mk,
        marcador_id = mkid,
        marcador_label = mklab,
        status = "model_error",
        detalhe = fit_obj$erro,
        n_linhas = nrow(d),
        n_municipios = data.table::uniqueN(d$code),
        n_anos = data.table::uniqueN(d$ano)
      )
      il <- il + 1L
      next
    }

    fit <- fit_obj$fit
    vc <- vcov_cluster_safe(fit, cluster = d$code)

    ano_ref_pre <- stats::median(d$ano[d$periodo_estagnacao == "Pre_estagnacao"], na.rm = TRUE)
    ano_ref_est <- stats::median(d$ano[d$periodo_estagnacao == "Estagnacao"], na.rm = TRUE)

    if (!is.finite(ano_ref_pre)) ano_ref_pre <- stats::median(d$ano, na.rm = TRUE)
    if (!is.finite(ano_ref_est)) ano_ref_est <- stats::median(d$ano, na.rm = TRUE)

    nd <- list(
      p0a0_est = make_nd(d, "Nao teve aumento", "Nao teve aumento", "Estagnacao", ano_ref_est),
      p1a0_est = make_nd(d, "Teve aumento", "Nao teve aumento", "Estagnacao", ano_ref_est),
      p0a1_est = make_nd(d, "Nao teve aumento", "Teve aumento", "Estagnacao", ano_ref_est),
      p1a1_est = make_nd(d, "Teve aumento", "Teve aumento", "Estagnacao", ano_ref_est),
      p0a0_pre = make_nd(d, "Nao teve aumento", "Nao teve aumento", "Pre_estagnacao", ano_ref_pre),
      p1a0_pre = make_nd(d, "Teve aumento", "Nao teve aumento", "Pre_estagnacao", ano_ref_pre),
      p0a1_pre = make_nd(d, "Nao teve aumento", "Teve aumento", "Pre_estagnacao", ano_ref_pre),
      p1a1_pre = make_nd(d, "Teve aumento", "Teve aumento", "Pre_estagnacao", ano_ref_pre)
    )

    contrastes <- list(
      efeito_politica_sem_aps_est = list(
        tipo = "policy_effect_without_phc_stagnation",
        medida_nome = "RCR",
        descricao = "Policy increase vs no policy increase among municipalities without PHC increase, stagnation period",
        nds = list(nd$p1a0_est, nd$p0a0_est),
        w = c(1, -1)
      ),
      efeito_politica_com_aps_est = list(
        tipo = "policy_effect_with_phc_stagnation",
        medida_nome = "RCR",
        descricao = "Policy increase vs no policy increase among municipalities with PHC increase, stagnation period",
        nds = list(nd$p1a1_est, nd$p0a1_est),
        w = c(1, -1)
      ),
      modificacao_aps_est = list(
        tipo = "phc_modification_stagnation",
        medida_nome = "Interaction ratio",
        descricao = "Difference in policy effect when PHC increased vs did not increase, stagnation period",
        nds = list(nd$p1a1_est, nd$p0a1_est, nd$p1a0_est, nd$p0a0_est),
        w = c(1, -1, -1, 1)
      ),
      modificacao_aps_pre = list(
        tipo = "phc_modification_pre_stagnation",
        medida_nome = "Interaction ratio",
        descricao = "Difference in policy effect when PHC increased vs did not increase, pre-stagnation period",
        nds = list(nd$p1a1_pre, nd$p0a1_pre, nd$p1a0_pre, nd$p0a0_pre),
        w = c(1, -1, -1, 1)
      ),
      diferenca_est_vs_pre = list(
        tipo = "phc_modification_stagnation_vs_pre",
        medida_nome = "Interaction ratio",
        descricao = "PHC modification during stagnation minus PHC modification before stagnation",
        nds = list(
          nd$p1a1_est,
          nd$p0a1_est,
          nd$p1a0_est,
          nd$p0a0_est,
          nd$p1a1_pre,
          nd$p0a1_pre,
          nd$p1a0_pre,
          nd$p0a0_pre
        ),
        w = c(1, -1, -1, 1, -1, 1, 1, -1)
      )
    )

    for (nm in names(contrastes)) {
      cc <- contrastes[[nm]]
      out <- linear_contrast(fit, vc, cc$nds, cc$w)

      out[, `:=`(
        faixa_analise = fx,
        faixa_id = fxid,
        marcador = mk,
        marcador_id = mkid,
        marcador_label = mklab,
        marcador_label_longo = mklab_long,
        classe = classe_i,
        tipo_contraste = cc$tipo,
        medida_nome = cc$medida_nome,
        descricao = cc$descricao,
        n_linhas = nrow(d),
        n_municipios = data.table::uniqueN(d$code),
        n_anos = data.table::uniqueN(d$ano),
        ano_ref_pre = ano_ref_pre,
        ano_ref_estagnacao = ano_ref_est,
        formula = paste(deparse(formula_v10), collapse = " ")
      )]

      lista_res[[ir]] <- finalizar_contraste(out)
      ir <- ir + 1L
    }

    lista_log[[il]] <- data.table::data.table(
      faixa_analise = fx,
      faixa_id = fxid,
      marcador = mk,
      marcador_id = mkid,
      marcador_label = mklab,
      status = "ok",
      detalhe = NA_character_,
      n_linhas = nrow(d),
      n_municipios = data.table::uniqueN(d$code),
      n_anos = data.table::uniqueN(d$ano),
      n_grupos4 = data.table::uniqueN(d$grupo4),
      n_grupos3 = data.table::uniqueN(d$grupo3_clinico, na.rm = TRUE)
    )
    il <- il + 1L
  }

  list(
    resultados = rbindlist_seguro(lista_res),
    log = rbindlist_seguro(lista_log)
  )
}

res_modelos <- rodar_modelos_v10(base_v10)
resultados_v10 <- res_modelos$resultados
log_modelos_v10 <- res_modelos$log

fwrite_safe(resultados_v10, file.path(dir_tabelas, "03_model_results.csv"))
fwrite_safe(log_modelos_v10, file.path(dir_logs, "03_model_log.csv"))


# ============================================================
# 9. OBSERVED TEMPORAL TABLE WITH TWO DISPLAY GROUPS
# ============================================================

calcular_temporal3 <- function(base_long) {
  obs3 <- base_long[
    !is.na(grupo3_clinico),
    .(
      obitos = sum(obitos, na.rm = TRUE),
      risco = sum(risco, na.rm = TRUE),
      n_municipios = data.table::uniqueN(code),
      n_linhas = .N
    ),
    by = .(
      faixa_analise,
      faixa_id,
      marcador,
      marcador_id,
      marcador_label,
      marcador_label_longo,
      classe,
      ano,
      grupo3_clinico
    )
  ]

  obs3[, taxa_1000 := data.table::fifelse(risco > 0, obitos / risco * 1000, NA_real_)]
  obs3[, periodo := data.table::fifelse(ano >= ano_inicio_estagnacao, "Estagnacao", "Pre-estagnacao")]

  # Primeiro ano observado de cada serie.
  data.table::setorder(
    obs3,
    faixa_analise,
    marcador,
    grupo3_clinico,
    ano
  )

  ref <- obs3[
    is.finite(taxa_1000) & taxa_1000 > 0,
    .SD[1],
    by = .(faixa_analise, marcador, grupo3_clinico)
  ][
    ,
    .(
      faixa_analise,
      marcador,
      grupo3_clinico,
      ano_referencia_serie = ano,
      taxa_referencia_serie = taxa_1000
    )
  ]

  obs3 <- merge(
    obs3,
    ref,
    by = c("faixa_analise", "marcador", "grupo3_clinico"),
    all.x = TRUE
  )

  obs3[, variacao_pct_primeiro_ano := data.table::fifelse(
    is.finite(taxa_referencia_serie) & taxa_referencia_serie > 0,
    (taxa_1000 / taxa_referencia_serie - 1) * 100,
    NA_real_
  )]

  obs3[, reducao_pct_primeiro_ano := data.table::fifelse(
    is.finite(taxa_referencia_serie) & taxa_referencia_serie > 0,
    (1 - taxa_1000 / taxa_referencia_serie) * 100,
    NA_real_
  )]

  obs3[]
}

temporal3 <- calcular_temporal3(base_v10)

fwrite_safe(temporal3, file.path(dir_tabelas, "04_time_2lines_change.csv"))


# ============================================================
# 10. COMMON AXES FOR TEMPORAL FIGURES
# ============================================================

make_percent_limits <- function(x, round_to = y_axis_round_to) {
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(c(-40, 20))
  }

  lo <- min(x, 0, na.rm = TRUE)
  hi <- max(x, 0, na.rm = TRUE)

  lo <- floor(lo / round_to) * round_to
  hi <- ceiling(hi / round_to) * round_to

  if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
    return(c(-40, 20))
  }

  c(lo, hi)
}

make_year_breaks <- function(x, step = x_break_step) {
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(NULL)
  }

  lo <- min(x, na.rm = TRUE)
  hi <- max(x, na.rm = TRUE)
  breaks <- seq(lo, hi, by = step)

  if (length(breaks) == 0 || tail(breaks, 1) != hi) {
    breaks <- sort(unique(c(breaks, hi)))
  }

  breaks
}

axis_x_limits <- range(temporal3$ano[is.finite(temporal3$ano)], na.rm = TRUE)
axis_x_breaks <- make_year_breaks(temporal3$ano, step = x_break_step)

axis_y_limits <- make_percent_limits(
  temporal3[
    !is.na(grupo3_clinico) &
      is.finite(variacao_pct_primeiro_ano)
  ]$variacao_pct_primeiro_ano
)

axis_y_breaks <- seq(axis_y_limits[1], axis_y_limits[2], by = y_break_step)

temporal_axis_config <- data.table::data.table(
  x_min = axis_x_limits[1],
  x_max = axis_x_limits[2],
  x_breaks = paste(axis_x_breaks, collapse = "; "),
  y_min_pct = axis_y_limits[1],
  y_max_pct = axis_y_limits[2],
  y_breaks_pct = paste(axis_y_breaks, collapse = "; ")
)

fwrite_safe(
  data.table::data.table(
    x_min = axis_x_limits[1],
    x_max = axis_x_limits[2],
    x_breaks = paste(axis_x_breaks, collapse = "; "),
    y_min_pct = axis_y_limits[1],
    y_max_pct = axis_y_limits[2],
    y_breaks_pct = paste(axis_y_breaks, collapse = "; "),
    note = "Common y-axis and x-axis settings applied to all temporal figures and panels."
  ),
  file.path(dir_logs, "05_temporal_axes.csv")
)


# ============================================================
# 11. PLOTTING FUNCTIONS
# ============================================================

tema_clinico <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, lineheight = 1.05),
      plot.caption = ggplot2::element_text(size = base_size - 3, hjust = 0, color = "grey35"),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

adicionar_smooth_ou_linha <- function(p, d, grupo_col = "grupo3_clinico", y_col = "variacao_pct_primeiro_ano") {
  min_n <- d[
    is.finite(get(y_col)),
    .N,
    by = grupo_col
  ][, min(N, na.rm = TRUE)]

  if (is.finite(min_n) && min_n >= 5) {
    p + ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      span = loess_span,
      linewidth = 1.35
    )
  } else {
    p + ggplot2::geom_line(linewidth = 1.10, alpha = 0.95)
  }
}

plot_temporal_3linhas_variacao <- function(dt, faixa_id_i, marcador_id_i) {
  d <- data.table::copy(dt[faixa_id == faixa_id_i & marcador_id == marcador_id_i])

  arq <- arquivo_curto(
    dir_fig_temporal3,
    "ts",
    paste0(faixa_id_i, "_", marcador_id_i),
    ".png"
  )

  d <- d[!is.na(grupo3_clinico) & is.finite(variacao_pct_primeiro_ano)]

  if (nrow(d) == 0 || data.table::uniqueN(d$grupo3_clinico) < 2) {
    criar_png_sem_resultado(
      arq,
      "No data",
      paste(faixa_id_i, marcador_id_i)
    )
    return(invisible(arq))
  }

  d[, grupo3_clinico := factor(as.character(grupo3_clinico), levels = ordem_grupo3)]

  lab_fx <- age_label_en(unique(d$faixa_analise)[1])
  lab_mk <- unique(d$marcador_label_longo)[1]

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = ano,
      y = variacao_pct_primeiro_ano,
      color = grupo3_clinico,
      group = grupo3_clinico
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = ano_inicio_estagnacao - 0.5,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey85",
      alpha = 0.35
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.40, color = "grey35") +
    ggplot2::geom_vline(
      xintercept = ano_inicio_estagnacao,
      linetype = "dashed",
      linewidth = 0.45,
      color = "grey35"
    )

  if (isTRUE(mostrar_linha_bruta_fina)) {
    p <- p + ggplot2::geom_line(linewidth = 0.35, alpha = 0.25)
  }

  if (isTRUE(mostrar_pontos_temporal)) {
    p <- p + ggplot2::geom_point(size = 1.45, alpha = 0.62)
  }

  p <- adicionar_smooth_ou_linha(p, d)

  p <- p +
    ggplot2::scale_color_manual(values = cores_grupo3, drop = FALSE) +
    ggplot2::scale_x_continuous(
      limits = axis_x_limits,
      breaks = axis_x_breaks
    ) +
    ggplot2::scale_y_continuous(
      limits = axis_y_limits,
      breaks = axis_y_breaks,
      labels = function(x) paste0(fmt_num(x, 0), "%")
    ) +
    ggplot2::labs(
      title = paste0(lab_mk, " and PHC"),
      subtitle = lab_fx,
      x = "Year",
      y = "Change from first year (%)",
      caption = NULL
    ) +
    tema_clinico(12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )

  ggplot2::ggsave(
    filename = arq,
    plot = p,
    width = fig_width,
    height = fig_height,
    dpi = fig_dpi,
    bg = "white",
    limitsize = FALSE
  )

  invisible(arq)
}

plot_painel_3linhas_faixa <- function(dt, faixa_id_i) {
  d <- data.table::copy(dt[faixa_id == faixa_id_i])
  d <- d[!is.na(grupo3_clinico) & is.finite(variacao_pct_primeiro_ano)]

  arq <- arquivo_curto(dir_fig_panel, "panel", faixa_id_i, ".png")

  if (nrow(d) == 0 || data.table::uniqueN(d$marcador_id) < 1) {
    criar_png_sem_resultado(arq, "No data", faixa_id_i)
    return(invisible(arq))
  }

  d[, grupo3_clinico := factor(as.character(grupo3_clinico), levels = ordem_grupo3)]

  lab_fx <- age_label_en(unique(d$faixa_analise)[1])

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = ano,
      y = variacao_pct_primeiro_ano,
      color = grupo3_clinico,
      group = grupo3_clinico
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = ano_inicio_estagnacao - 0.5,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey85",
      alpha = 0.35
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "grey35") +
    ggplot2::geom_vline(
      xintercept = ano_inicio_estagnacao,
      linetype = "dashed",
      linewidth = 0.35,
      color = "grey35"
    ) +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      span = loess_span,
      linewidth = 1.05
    ) +
    ggplot2::scale_color_manual(values = cores_grupo3, drop = FALSE) +
    ggplot2::scale_x_continuous(
      limits = axis_x_limits,
      breaks = axis_x_breaks
    ) +
    ggplot2::scale_y_continuous(
      limits = axis_y_limits,
      breaks = axis_y_breaks,
      labels = function(x) paste0(fmt_num(x, 0), "%")
    ) +
    ggplot2::facet_wrap(~ marcador_label, scales = "fixed", ncol = 2) +
    ggplot2::labs(
      title = "Policy expansion and PHC",
      subtitle = lab_fx,
      x = "Year",
      y = "Change from first year (%)",
      caption = NULL
    ) +
    tema_clinico(11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )

  ggplot2::ggsave(
    filename = arq,
    plot = p,
    width = fig_width_painel,
    height = fig_height_painel,
    dpi = fig_dpi,
    bg = "white",
    limitsize = FALSE
  )

  invisible(arq)
}

plot_forest_modificacao <- function(res, faixa_id_i) {
  d <- data.table::copy(res[
    faixa_id == faixa_id_i &
      tipo_contraste == "phc_modification_stagnation" &
      is.finite(medida) &
      is.finite(ic95_inf) &
      is.finite(ic95_sup)
  ])

  arq <- arquivo_curto(dir_fig_forest, "forest", faixa_id_i, ".png")

  if (nrow(d) == 0) {
    criar_png_sem_resultado(arq, "No model results", faixa_id_i)
    return(invisible(arq))
  }

  data.table::setorder(d, classe, marcador_label)
  d[, marcador_label := factor(marcador_label, levels = rev(unique(marcador_label)))]
  d[, texto := paste0(medida_ic95, "   p=", p_formatado)]
  d[, sig := data.table::fifelse(!is.na(p_valor) & p_valor < 0.05, "p < 0.05", "p >= 0.05")]

  x_min <- 0.40
  x_max <- 2.50

  d[, med_plot := pmin(pmax(medida, x_min), x_max)]
  d[, lo_plot := pmin(pmax(ic95_inf, x_min), x_max)]
  d[, hi_plot := pmin(pmax(ic95_sup, x_min), x_max)]

  lab_fx <- age_label_en(unique(d$faixa_analise)[1])
  altura <- max(fig_height_forest_min, 0.55 * nrow(d) + 2.6)

  p <- ggplot2::ggplot(d, ggplot2::aes(y = marcador_label)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.45,
      color = "grey35"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = lo_plot,
        xend = hi_plot,
        yend = marcador_label
      ),
      linewidth = 0.9,
      color = "#1D3557"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = med_plot, shape = sig),
      size = 3.0,
      color = "#1D3557"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_max * 1.08, label = texto),
      hjust = 0,
      size = 3.5,
      color = "grey10"
    ) +
    ggplot2::scale_x_log10(
      limits = c(x_min, x_max * 1.9),
      breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2.0, 2.5),
      labels = scales::number_format(decimal.mark = ".", accuracy = 0.01)
    ) +
    ggplot2::scale_shape_manual(
      values = c("p < 0.05" = 15, "p >= 0.05" = 16),
      drop = FALSE
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Policy-PHC interaction",
      subtitle = paste0(lab_fx, ". Ratio < 1 favors policy + PHC."),
      x = "Interaction ratio",
      y = NULL,
      caption = NULL
    ) +
    tema_clinico(12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 155, 10, 10)
    ) +
    ggplot2::annotate(
      "text",
      x = x_max * 1.08,
      y = Inf,
      label = "Ratio (95% CI), p",
      hjust = 0,
      vjust = 1.4,
      size = 3.6,
      fontface = "bold"
    )

  ggplot2::ggsave(
    filename = arq,
    plot = p,
    width = fig_width_forest,
    height = altura,
    dpi = fig_dpi,
    bg = "white",
    limitsize = FALSE
  )

  invisible(arq)
}


# ============================================================
# 11. GERAR FIGURAS
# ============================================================

arquivos_figuras <- list()
cont <- 1L

pares <- unique(temporal3[, .(faixa_id, marcador_id)])

for (i in seq_len(nrow(pares))) {
  fxid <- pares$faixa_id[i]
  mkid <- pares$marcador_id[i]

  arquivos_figuras[[cont]] <- data.table::data.table(
    tipo = "time_2lines_change",
    faixa_id = fxid,
    marcador_id = mkid,
    arquivo = plot_temporal_3linhas_variacao(temporal3, fxid, mkid)
  )
  cont <- cont + 1L
}

for (fxid in unique(base_v10$faixa_id)) {
  arquivos_figuras[[cont]] <- data.table::data.table(
    tipo = "panel_2lines",
    faixa_id = fxid,
    marcador_id = NA_character_,
    arquivo = plot_painel_3linhas_faixa(temporal3, fxid)
  )
  cont <- cont + 1L

  arquivos_figuras[[cont]] <- data.table::data.table(
    tipo = "forest_phc_interaction",
    faixa_id = fxid,
    marcador_id = NA_character_,
    arquivo = plot_forest_modificacao(resultados_v10, fxid)
  )
  cont <- cont + 1L
}

figuras_geradas <- rbindlist_seguro(arquivos_figuras)

fwrite_safe(figuras_geradas, file.path(dir_logs, "04_figures.csv"))


# ============================================================
# 12. EXCEL E README
# ============================================================

wb <- openxlsx::createWorkbook()

add_sheet_safe(
  wb,
  "input",
  data.table::data.table(
    origem_base = origem_base,
    dir_v9 = dir_v9,
    out_dir_v10 = out_dir_v10,
    aps_col = aps_col
  )
)

add_sheet_safe(wb, "policies", politicas_disponiveis)
add_sheet_safe(wb, "groups_4cells", resumo_grupos4)
add_sheet_safe(wb, "groups_2lines", resumo_grupos3)
add_sheet_safe(wb, "models", resultados_v10)
add_sheet_safe(wb, "time_2lines", temporal3)
add_sheet_safe(wb, "axes", temporal_axis_config)
add_sheet_safe(wb, "model_log", log_modelos_v10)
add_sheet_safe(wb, "figures", figuras_geradas)

openxlsx::saveWorkbook(
  wb,
  file.path(dir_tabelas, "v10_phc_results.xlsx"),
  overwrite = TRUE
)

readme <- c(
  "MODEL V10 - CLINICAL ENGLISH VERSION",
  "",
  "Objective:",
  "To assess whether PHC expansion modifies the association between policy expansion and annual change in under-5 mortality.",
  "",
  "Main temporal figures:",
  "Only two groups are shown: Policy only and Policy + PHC.",
  "The no-increase group is not plotted, but remains in the model structure.",
  "All temporal plots and panel figures use the same y-axis scale.",
  "The x-axis uses fixed two-year breaks to improve temporal readability.",
  "",
  "Y-axis:",
  "Percent change in mortality rate from the first observed year of each series.",
  "",
  "Model:",
  "log annual rate change ~ policy expansion * PHC expansion * period + year + lagged mortality.",
  "",
  "Main contrast:",
  "PHC modification during the stagnation period.",
  "",
  "Interpretation:",
  "Interaction ratio < 1 favors joint policy + PHC expansion.",
  "Interaction ratio > 1 favors policy expansion without PHC expansion.",
  "",
  "Figures:",
  "figures/time_2lines: individual temporal figures.",
  "figures/panels_2lines: age-specific multipanel figures.",
  "figures/forest: model-based interaction plots.",
  "",
  "Caution:",
  "LOESS curves are descriptive. Inference should rely on model estimates and forest plots."
)

writeLines(readme, file.path(out_dir_v10, "README_v10_phc_en.txt"))


# ============================================================
# 13. FINAL SUMMARY
# ============================================================

message("V10 English clinical version completed.")
message("Output folder: ", out_dir_v10)
message("Excel: ", file.path(dir_tabelas, "v10_phc_results.xlsx"))
message("Temporal figures: ", dir_fig_temporal3)
message("Panels: ", dir_fig_panel)
message("Forest plots: ", dir_fig_forest)
message("")
message("Main contrast: phc_modification_stagnation")
message("Interaction ratio < 1 favors joint policy + PHC expansion.")
message("Interaction ratio > 1 favors policy expansion without PHC expansion.")

if (nrow(log_modelos_v10) > 0) {
  message("")
  message("Model summary:")
  print(log_modelos_v10[, .N, by = .(status, detalhe)][order(status, detalhe)])
}

if (nrow(figuras_geradas) > 0) {
  message("")
  message("Registered figures: ", nrow(figuras_geradas))
  message("See: ", file.path(dir_logs, "04_figures.csv"))
}
