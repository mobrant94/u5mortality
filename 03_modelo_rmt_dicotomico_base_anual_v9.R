# ============================================================
# MODELO V9 - EXPANSAO DICOTOMICA USANDO BASE ANUAL JA ACHATADA
# Razao das mudancas anuais da taxa (RMT)
#
# Entrada esperada no ambiente:
#   base_anual_mortalidade_contexto
#
# Unidade esperada:
#   municipio x ano x faixa_etaria
#
# Esta versao NAO reprocessa bases mensais.
# Usa diretamente a base anual criada no passo anterior.
#
# Pergunta:
#   Municipios que tiveram aumento da politica apresentaram mudanca anual
#   da mortalidade mais favoravel do que municipios sem aumento?
#
# Medida:
#   RMT = razao das mudancas anuais da taxa.
#
# Interpretacao:
#   RMT < 1: maior reducao anual ou menor estagnacao.
#   RMT = 1: mudanca anual semelhante a referencia.
#   RMT > 1: menor reducao anual ou maior estagnacao.
#
# Desfecho:
#   log_razao_taxa =
#     log((obitos_t + 0.5) / crianca_ano_risco_t) -
#     log((obitos_t-1 + 0.5) / crianca_ano_risco_t-1)
#
# Exposicao:
#   Expansao acumulada do marcador em relacao ao baseline municipal.
#
# Categorias:
#   Nao teve aumento = referencia
#   Teve aumento
#
# Observacao:
#   "Nao teve aumento" inclui reducao e estabilidade.
#   "Teve aumento" exige expansao acima da banda neutra do marcador
#   quando usar_banda_neutra = TRUE.
#
# Contrastes:
#   1) durante_estagnacao:
#      Teve aumento vs Nao teve aumento durante a estagnacao.
#
#   2) diferenca_estagnacao_vs_pre:
#      [Teve aumento - Nao teve aumento] na estagnacao
#      menos [Teve aumento - Nao teve aumento] antes da estagnacao.
#
# Saidas:
#   modelo_rmt_dicotomico_base_anual_v9/
#     tabelas/
#     logs/
#     graficos/forest_por_faixa/
# ============================================================


# ============================================================
# 1. CONFIGURACAO
# ============================================================

if (!exists("base_dir")) {
  base_dir <- getwd()
}

out_dir <- file.path(base_dir, "modelo_rmt_dicotomico_base_anual_v9")

ano_inicio_estagnacao <- 2015L
anos_baseline_expansao <- 2001:2005

correcao_obitos_log <- 0.5

# Se TRUE, pequenas variacoes positivas abaixo da banda neutra contam como
# "Nao teve aumento". Se quiser qualquer valor > 0 como aumento, use FALSE.
usar_banda_neutra <- TRUE
quantil_banda_neutra <- 0.10

min_linhas_modelo <- 150L
min_municipios_modelo <- 50L
min_anos_modelo <- 5L

incluir_ano_e_taxa_lag <- TRUE
incluir_efeito_fixo_municipio <- FALSE

# Limites visuais do forest plot. Tabelas mantem valores completos.
clip_x_min <- 0.25
clip_x_max <- 4.00

dpi_png <- 300
largura_png <- 17
base_altura_por_linha <- 0.45
altura_minima_png <- 7.5

rotulo_geral_menor5 <- "Geral <5 anos"

# Politicas avaliadas. Favelas e Mais Medicos ficam fora desta versao
# por nao serem a pergunta atual de expansao dicotomica das politicas principais.
marcadores_para_rodar <- c(
  "aps_esf_pct",
  "vac_bcg_pct",
  "vac_penta_pct",
  "vac_pneumococica_pct",
  "vac_rotavirus_pct",
  "agua_pct",
  "esgoto_pct",
  "bf_beneficios_por_1000_crianca"
)

categorias_modelo <- c("Nao teve aumento", "Teve aumento")

pacotes <- c(
  "data.table",
  "ggplot2",
  "sandwich",
  "openxlsx",
  "scales",
  "MASS"
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
# 2. PACOTES E DIRETORIOS
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

dir_tabelas <- file.path(out_dir, "tabelas")
dir_logs <- file.path(out_dir, "logs")
dir_graficos <- file.path(out_dir, "graficos")
dir_forest <- file.path(dir_graficos, "forest_por_faixa")

for (d in c(out_dir, dir_tabelas, dir_logs, dir_graficos, dir_forest)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}


# ============================================================
# 3. FUNCOES AUXILIARES
# ============================================================

rbindlist_seguro <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) return(data.table::data.table())
  data.table::rbindlist(x, fill = TRUE)
}

safe_file <- function(x, max_len = 90L) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  substr(tolower(x), 1, max_len)
}

pad_code6 <- function(x) {
  x <- as.character(x)
  x <- gsub("\\D", "", x)
  x[x == ""] <- NA_character_

  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)

  out[ok & nchar(x) >= 7] <- substr(x[ok & nchar(x) >= 7], 1, 6)
  out[ok & nchar(x) == 6] <- x[ok & nchar(x) == 6]
  out[ok & nchar(x) < 6]  <- sprintf("%06s", x[ok & nchar(x) < 6])

  out
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

mean_na <- function(x) {
  x <- to_num(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

z_score <- function(x) {
  x <- to_num(x)
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(m) || !is.finite(s) || s == 0) {
    return(rep(0, length(x)))
  }

  as.numeric((x - m) / s)
}

formatar_num <- function(x, digits = 2) {
  out <- rep("", length(x))
  ok <- !is.na(x) & is.finite(x)

  out[ok] <- formatC(
    x[ok],
    format = "f",
    digits = digits,
    decimal.mark = ",",
    big.mark = "."
  )

  out
}

formatar_p <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.001] <- "<0,001"
  out[!is.na(p) & p >= 0.001] <- formatC(
    p[!is.na(p) & p >= 0.001],
    format = "f",
    digits = 3,
    decimal.mark = ","
  )
  out
}

primeira_coluna_existente <- function(dt, alternativas) {
  out <- alternativas[alternativas %in% names(dt)]
  if (length(out) == 0) return(NA_character_)
  out[1]
}

sanitize_for_export <- function(dt) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  if (nrow(dt) == 0 && ncol(dt) == 0) {
    return(data.table::data.table(info = "sem_resultados"))
  }

  for (nm in names(dt)) {
    if (is.list(dt[[nm]]) && !is.data.frame(dt[[nm]])) {
      dt[[nm]] <- vapply(dt[[nm]], function(z) paste(as.character(z), collapse = " | "), character(1))
    }

    if (inherits(dt[[nm]], c("POSIXct", "POSIXt", "Date"))) {
      dt[[nm]] <- as.character(dt[[nm]])
    }
  }

  dt
}

fwrite_safe <- function(dt, file) {
  dt <- sanitize_for_export(dt)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(dt, file = file)
  invisible(file)
}

add_sheet_safe <- function(wb, sheet, dt) {
  dt <- sanitize_for_export(dt)
  sheet <- substr(gsub("[^A-Za-z0-9_]+", "_", sheet), 1, 31)

  if (sheet %in% openxlsx::sheets(wb)) {
    sheet <- substr(paste0(substr(sheet, 1, 25), "_", length(openxlsx::sheets(wb)) + 1), 1, 31)
  }

  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, as.data.frame(dt))
  invisible(wb)
}

interpretar_rmt <- function(rmt, p_valor) {
  data.table::fcase(
    !is.na(rmt) & rmt < 1 & !is.na(p_valor) & p_valor < 0.05,
    "Mais favoravel: maior reducao anual ou menor estagnacao",
    !is.na(rmt) & rmt < 1,
    "Direcao mais favoravel, sem significancia a 5%",
    !is.na(rmt) & rmt > 1 & !is.na(p_valor) & p_valor < 0.05,
    "Menos favoravel: menor reducao anual ou maior estagnacao",
    !is.na(rmt) & rmt > 1,
    "Direcao menos favoravel, sem significancia a 5%",
    default = "Nao conclusivo"
  )
}

criar_png_sem_resultado <- function(file, titulo, subtitulo) {
  p <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0.15, label = titulo, size = 6, fontface = "bold") +
    ggplot2::annotate("text", x = 0, y = -0.10, label = subtitulo, size = 4.2) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()

  ggplot2::ggsave(
    filename = file,
    plot = p,
    width = 12,
    height = 7,
    dpi = dpi_png,
    bg = "white",
    limitsize = FALSE
  )
}


# ============================================================
# 4. CATALOGO DE MARCADORES
# ============================================================

fonte_variaveis <- list(
  aps_esf_pct = c(
    "aps_esf_pct",
    "cobertura_esf_pct",
    "aps_proporcao_cobertura_estrategia_saude_familia",
    "proporcao_cobertura_estrategia_saude_familia",
    "cobertura_aps",
    "cobertura_aps_pct"
  ),
  agua_pct = c(
    "agua_pct",
    "cobertura_agua_pct",
    "cobertura_urbana_agua_pct"
  ),
  esgoto_pct = c(
    "esgoto_pct",
    "cobertura_esgoto_pct",
    "cobertura_urbana_esgoto_pct"
  ),
  bf_beneficios_por_1000_crianca = c(
    "bf_beneficios_por_1000_crianca",
    "bolsa_familia_por_1000_crianca"
  ),
  total_beneficios_bf = c(
    "total_beneficios_bf",
    "bf_beneficios",
    "bolsa_familia_beneficios"
  ),
  vac_bcg_pct = c("vac_bcg_pct", "bcg_pct", "cobertura_bcg"),
  vac_penta_pct = c("vac_penta_pct", "penta_pct", "cobertura_penta", "cobertura_pentavalente"),
  vac_pneumococica_pct = c("vac_pneumococica_pct", "pneumococica_pct", "cobertura_pneumococica"),
  vac_rotavirus_pct = c("vac_rotavirus_pct", "rotavirus_pct", "cobertura_rotavirus")
)

catalogo_marcadores <- data.table::data.table(
  marcador = c(
    "aps_esf_pct",
    "vac_bcg_pct",
    "vac_penta_pct",
    "vac_pneumococica_pct",
    "vac_rotavirus_pct",
    "agua_pct",
    "esgoto_pct",
    "bf_beneficios_por_1000_crianca"
  ),
  marcador_label = c(
    "APS/ESF",
    "Cobertura vacinal BCG",
    "Cobertura vacinal pentavalente",
    "Cobertura vacinal pneumococica",
    "Cobertura vacinal rotavirus",
    "Cobertura de agua encanada",
    "Cobertura de esgoto",
    "Bolsa Familia por 1.000 criancas-ano"
  ),
  classe = c(
    "aps",
    "vacina",
    "vacina",
    "vacina",
    "vacina",
    "saneamento",
    "saneamento",
    "protecao_social"
  )
)


# ============================================================
# 5. CARREGAR BASE ANUAL
# ============================================================

if (!exists("base_anual_mortalidade_contexto")) {
  stop(
    "Objeto 'base_anual_mortalidade_contexto' nao encontrado no ambiente. ",
    "Rode primeiro o script que achata a base para municipio x ano x faixa_etaria."
  )
}

base_anual <- data.table::as.data.table(data.table::copy(base_anual_mortalidade_contexto))

if (!"code" %in% names(base_anual) && "code_muni" %in% names(base_anual)) {
  base_anual[, code := pad_code6(code_muni)]
}

if (!"code" %in% names(base_anual)) stop("A base anual precisa ter coluna 'code' ou 'code_muni'.")
if (!"ano" %in% names(base_anual)) stop("A base anual precisa ter coluna 'ano'.")
if (!"faixa_etaria" %in% names(base_anual)) stop("A base anual precisa ter coluna 'faixa_etaria'.")
if (!"obitos" %in% names(base_anual)) stop("A base anual precisa ter coluna 'obitos'.")

base_anual[, code := pad_code6(code)]
base_anual[, ano := as.integer(to_num(ano))]
base_anual[, faixa_etaria := as.character(faixa_etaria)]
base_anual[, obitos := to_num(obitos)]

# Denominador preferencial para taxa anual.
if ("crianca_ano_risco" %in% names(base_anual)) {
  base_anual[, risco := to_num(crianca_ano_risco)]
} else if ("denominador_soma_meses" %in% names(base_anual)) {
  base_anual[, risco := to_num(denominador_soma_meses) / 12]
} else if ("denominador" %in% names(base_anual)) {
  # Na base anual criada antes, denominador = soma dos meses.
  base_anual[, risco := to_num(denominador) / 12]
} else {
  stop("A base anual precisa ter 'crianca_ano_risco', 'denominador_soma_meses' ou 'denominador'.")
}

base_anual <- base_anual[
  !is.na(code) &
    !is.na(ano) &
    !is.na(faixa_etaria) &
    !is.na(obitos) &
    !is.na(risco) &
    risco > 0
]

base_anual[, faixa_analise := faixa_etaria]

# Garante que Geral <5 anos venha primeiro, se existir.
faixa_levels <- c(
  rotulo_geral_menor5,
  sort(setdiff(unique(base_anual$faixa_analise), rotulo_geral_menor5))
)
faixa_levels <- faixa_levels[faixa_levels %in% unique(base_anual$faixa_analise)]

base_anual[, faixa_analise := factor(faixa_analise, levels = faixa_levels)]

# Checagem de unicidade da base anual.
dup_base <- nrow(base_anual) - data.table::uniqueN(base_anual[, .(code, ano, faixa_analise)])
if (dup_base > 0) {
  warning(
    "A base_anual_mortalidade_contexto possui duplicidade em code + ano + faixa_analise: ",
    dup_base,
    " linhas duplicadas. O script vai agregar para corrigir."
  )

  vars_num <- names(base_anual)[vapply(base_anual, is.numeric, logical(1))]
  vars_num <- setdiff(vars_num, c("ano", "obitos", "risco"))

  base_anual <- base_anual[
    ,
    c(
      .(
        obitos = sum(obitos, na.rm = TRUE),
        risco = sum(risco, na.rm = TRUE)
      ),
      lapply(.SD, function(x) {
        if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
      })
    ),
    by = .(code, ano, faixa_analise),
    .SDcols = vars_num
  ]

  base_anual[, faixa_etaria := as.character(faixa_analise)]
}

fwrite_safe(
  data.table::data.table(
    n_linhas = nrow(base_anual),
    n_municipios = uniqueN(base_anual$code),
    ano_min = min(base_anual$ano, na.rm = TRUE),
    ano_max = max(base_anual$ano, na.rm = TRUE),
    n_faixas = uniqueN(base_anual$faixa_analise),
    obitos_total = sum(base_anual$obitos, na.rm = TRUE),
    risco_total = sum(base_anual$risco, na.rm = TRUE)
  ),
  file.path(dir_logs, "01_resumo_base_anual_entrada.csv")
)


# ============================================================
# 6. PADRONIZAR MARCADORES NA BASE ANUAL
# ============================================================

mapa_fontes <- rbindlist_seguro(
  lapply(names(fonte_variaveis), function(std) {
    data.table::data.table(
      variavel_padrao = std,
      coluna_origem = primeira_coluna_existente(base_anual, fonte_variaveis[[std]])
    )
  })
)

mapa_fontes <- mapa_fontes[!is.na(coluna_origem)]

# Cria colunas padronizadas sem apagar as originais.
for (i in seq_len(nrow(mapa_fontes))) {
  origem <- mapa_fontes$coluna_origem[i]
  destino <- mapa_fontes$variavel_padrao[i]

  if (origem %in% names(base_anual)) {
    base_anual[, (destino) := to_num(get(origem))]
  }
}

# Bolsa Familia por crianca-ano, se ainda nao existir.
if (!"bf_beneficios_por_1000_crianca" %in% names(base_anual) &&
    "total_beneficios_bf" %in% names(base_anual)) {
  base_anual[, bf_beneficios_por_1000_crianca := data.table::fifelse(
    risco > 0,
    total_beneficios_bf / risco * 1000,
    NA_real_
  )]
}

marcadores_disponiveis <- catalogo_marcadores[
  marcador %in% names(base_anual) &
    marcador %in% marcadores_para_rodar
]

if (nrow(marcadores_disponiveis) == 0) {
  stop("Nenhum marcador da lista foi encontrado na base anual.")
}

fwrite_safe(mapa_fontes, file.path(dir_logs, "02_mapa_colunas_utilizadas.csv"))


# ============================================================
# 7. BASE MUNICIPIO x ANO DOS MARCADORES
# ============================================================

# Evita repetir calculo por faixa etaria.
cols_markers <- marcadores_disponiveis$marcador

pol_ano <- unique(
  base_anual[
    ,
    c("code", "ano", cols_markers),
    with = FALSE
  ]
)

# Se houver divergencia de valor entre faixas, toma o maior valor anual.
pol_ano <- pol_ano[
  ,
  lapply(.SD, function(x) {
    x <- to_num(x)
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }),
  by = .(code, ano),
  .SDcols = cols_markers
]

fwrite_safe(pol_ano, file.path(dir_tabelas, "base_marcadores_anuais_padronizados.csv"))


# ============================================================
# 8. EXPANSAO DICOTOMICA DOS MARCADORES
# ============================================================

adicionar_expansao <- function(pol, var) {
  pol <- data.table::copy(pol)

  dbase <- pol[
    ano %in% anos_baseline_expansao &
      !is.na(get(var)) &
      is.finite(to_num(get(var))),
    .(baseline = mean_na(get(var))),
    by = code
  ]

  dfallback_pre <- pol[
    ano < ano_inicio_estagnacao &
      !is.na(get(var)) &
      is.finite(to_num(get(var)))
  ]
  data.table::setorder(dfallback_pre, code, ano)
  dfallback_pre <- dfallback_pre[, .(baseline_pre = get(var)[1]), by = code]

  dfallback_any <- pol[
    !is.na(get(var)) &
      is.finite(to_num(get(var)))
  ]
  data.table::setorder(dfallback_any, code, ano)
  dfallback_any <- dfallback_any[, .(baseline_any = get(var)[1]), by = code]

  b <- merge(dbase, dfallback_pre, by = "code", all = TRUE)
  b <- merge(b, dfallback_any, by = "code", all = TRUE)

  b[, baseline_final := data.table::fifelse(
    !is.na(baseline),
    baseline,
    data.table::fifelse(!is.na(baseline_pre), baseline_pre, baseline_any)
  )]

  base_name <- paste0(var, "_baseline")
  exp_name  <- paste0(var, "_expansao")

  b2 <- b[, .(code, baseline_final)]
  data.table::setnames(b2, "baseline_final", base_name)

  pol <- merge(pol, b2, by = "code", all.x = TRUE)
  pol[, (exp_name) := to_num(get(var)) - to_num(get(base_name))]

  pol
}

categorizar_aumento_dicotomico <- function(x) {
  x <- to_num(x)
  out <- rep(NA_character_, length(x))

  x_abs <- abs(x[!is.na(x) & is.finite(x) & x != 0])

  banda <- if (isTRUE(usar_banda_neutra) && length(x_abs) > 0) {
    as.numeric(stats::quantile(
      x_abs,
      probs = quantil_banda_neutra,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    ))
  } else {
    0
  }

  if (!is.finite(banda) || is.na(banda)) banda <- 0
  banda <- max(banda, 0)

  out[!is.na(x) & is.finite(x) & x <= banda] <- "Nao teve aumento"
  out[!is.na(x) & is.finite(x) & x > banda] <- "Teve aumento"

  factor(out, levels = categorias_modelo)
}

lista_cortes <- list()

for (i in seq_len(nrow(marcadores_disponiveis))) {
  var_i <- marcadores_disponiveis$marcador[i]
  label_i <- marcadores_disponiveis$marcador_label[i]
  classe_i <- marcadores_disponiveis$classe[i]

  pol_ano <- adicionar_expansao(pol_ano, var_i)

  exp_name <- paste0(var_i, "_expansao")
  cat_name <- paste0(var_i, "_aumento_cat")

  pol_ano[, (cat_name) := categorizar_aumento_dicotomico(get(exp_name))]

  x <- to_num(pol_ano[[exp_name]])
  catx <- as.character(pol_ano[[cat_name]])
  x_abs <- abs(x[!is.na(x) & is.finite(x) & x != 0])

  banda <- if (isTRUE(usar_banda_neutra) && length(x_abs) > 0) {
    as.numeric(stats::quantile(x_abs, probs = quantil_banda_neutra, na.rm = TRUE, names = FALSE, type = 7))
  } else {
    0
  }

  if (!is.finite(banda) || is.na(banda)) banda <- 0

  lista_cortes[[i]] <- data.table::data.table(
    marcador = var_i,
    marcador_label = label_i,
    classe = classe_i,
    variavel_expansao = exp_name,
    variavel_categoria = cat_name,
    banda_neutra_abs = banda,
    regra_nao_teve_aumento = paste0("expansao <= ", formatar_num(banda, 4)),
    regra_teve_aumento = paste0("expansao > ", formatar_num(banda, 4)),
    n_total = sum(!is.na(x) & is.finite(x)),
    n_sem_aumento_ref = sum(catx == "Nao teve aumento", na.rm = TRUE),
    n_com_aumento = sum(catx == "Teve aumento", na.rm = TRUE),
    pct_com_aumento = ifelse(
      sum(!is.na(catx)) > 0,
      mean(catx == "Teve aumento", na.rm = TRUE) * 100,
      NA_real_
    )
  )
}

pontos_corte_aumento <- rbindlist_seguro(lista_cortes)

fwrite_safe(pol_ano, file.path(dir_tabelas, "base_marcadores_anuais_expansao_dicotomica.csv"))
fwrite_safe(pontos_corte_aumento, file.path(dir_tabelas, "01_pontos_corte_aumento_dicotomico.csv"))


# ============================================================
# 9. JUNTAR CATEGORIAS DE EXPANSAO NA BASE ANUAL
# ============================================================

cols_exp <- grep("(_baseline$|_expansao$|_aumento_cat$)", names(pol_ano), value = TRUE)

base_modelo <- merge(
  base_anual,
  pol_ano[, c("code", "ano", cols_exp), with = FALSE],
  by = c("code", "ano"),
  all.x = TRUE,
  sort = FALSE
)

base_modelo[, taxa_1000 := data.table::fifelse(
  risco > 0,
  obitos / risco * 1000,
  NA_real_
)]

fwrite_safe(
  base_modelo[, .(
    n_linhas = .N,
    n_municipios = uniqueN(code),
    n_anos = uniqueN(ano),
    n_faixas = uniqueN(faixa_analise),
    obitos_total = sum(obitos, na.rm = TRUE),
    risco_total = sum(risco, na.rm = TRUE)
  )],
  file.path(dir_logs, "03_resumo_base_modelo.csv")
)


# ============================================================
# 10. BASE DE MUDANCA ANUAL
# ============================================================

criar_base_mudanca <- function(dt, marcadores) {
  dt <- data.table::copy(dt)
  data.table::setorder(dt, code, faixa_analise, ano)

  dt[, obitos_lag := data.table::shift(obitos, 1L), by = .(code, faixa_analise)]
  dt[, risco_lag := data.table::shift(risco, 1L), by = .(code, faixa_analise)]
  dt[, ano_lag := data.table::shift(ano, 1L), by = .(code, faixa_analise)]
  dt[, intervalo_ano := ano - ano_lag]

  for (var in marcadores) {
    cols_lag <- intersect(
      c(var, paste0(var, "_expansao"), paste0(var, "_aumento_cat")),
      names(dt)
    )

    for (cn in cols_lag) {
      dt[, (paste0(cn, "_lag")) := data.table::shift(get(cn), 1L), by = .(code, faixa_analise)]
    }
  }

  dt <- dt[
    intervalo_ano == 1 &
      !is.na(obitos_lag) &
      !is.na(risco_lag) &
      risco_lag > 0 &
      !is.na(risco) &
      risco > 0
  ]

  dt[, log_taxa_atual := log((obitos + correcao_obitos_log) / risco)]
  dt[, log_taxa_lag := log((obitos_lag + correcao_obitos_log) / risco_lag)]
  dt[, log_razao_taxa := log_taxa_atual - log_taxa_lag]
  dt[, mudanca_pct_anual := (exp(log_razao_taxa) - 1) * 100]

  dt[, var_log_razao_aprox := 1 / (obitos + correcao_obitos_log) + 1 / (obitos_lag + correcao_obitos_log)]

  dt[, peso_modelo := data.table::fifelse(
    is.finite(var_log_razao_aprox) & var_log_razao_aprox > 0,
    1 / var_log_razao_aprox,
    NA_real_
  )]

  dt[, peso_modelo := peso_modelo / mean(peso_modelo, na.rm = TRUE)]
  dt[!is.finite(peso_modelo) | is.na(peso_modelo) | peso_modelo <= 0, peso_modelo := 1]

  dt[, periodo_estagnacao := factor(
    ifelse(ano >= ano_inicio_estagnacao, "Estagnacao", "Pre_estagnacao"),
    levels = c("Pre_estagnacao", "Estagnacao")
  )]

  dt[, ano_c := ano - ano_inicio_estagnacao]
  dt[, z_log_taxa_lag := z_score(log_taxa_lag)]
  dt[, code_f := factor(code)]

  dt
}

base_mudanca <- criar_base_mudanca(
  base_modelo,
  marcadores = marcadores_disponiveis$marcador
)

if (nrow(base_mudanca) == 0) {
  stop("Base de mudanca anual ficou vazia. Verifique anos consecutivos por municipio/faixa.")
}

fwrite_safe(base_mudanca, file.path(dir_tabelas, "base_mudanca_anual_log_razao_taxa.csv"))


# ============================================================
# 11. MODELOS E CONTRASTES
# ============================================================

montar_formula_marcador <- function() {
  rhs <- "aumento_cat * periodo_estagnacao"

  if (isTRUE(incluir_ano_e_taxa_lag)) {
    rhs <- paste(rhs, "+ ano_c + z_log_taxa_lag")
  }

  if (isTRUE(incluir_efeito_fixo_municipio)) {
    rhs <- paste(rhs, "+ code_f")
  }

  stats::as.formula(paste("log_razao_taxa ~", rhs))
}

fit_lm_ponderado_safe <- function(formula_modelo, dados_modelo) {
  tryCatch(
    {
      fit <- stats::lm(
        formula_modelo,
        data = dados_modelo,
        weights = peso_modelo
      )
      list(fit = fit, erro = NA_character_)
    },
    error = function(e) {
      list(fit = NULL, erro = conditionMessage(e))
    }
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

linear_contrast_newdata <- function(fit, vc, newdata_list, weights) {
  tryCatch(
    {
      termos <- stats::delete.response(stats::terms(fit))
      beta <- stats::coef(fit)
      beta <- beta[is.finite(beta)]

      coef_validos <- intersect(names(beta), rownames(vc))
      coef_validos <- intersect(coef_validos, colnames(vc))

      if (length(coef_validos) == 0) {
        stop("Nenhum coeficiente valido encontrado para contraste.")
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

      t_valor <- est / se
      gl <- stats::df.residual(fit)
      p <- 2 * stats::pt(abs(t_valor), df = gl, lower.tail = FALSE)

      data.table::data.table(
        estimativa_log = est,
        erro_padrao = se,
        ic95_inf_log = est - 1.96 * se,
        ic95_sup_log = est + 1.96 * se,
        t = t_valor,
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

  dt[, rmt := exp(estimativa_log)]
  dt[, ic95_inf := exp(ic95_inf_log)]
  dt[, ic95_sup := exp(ic95_sup_log)]

  dt[, mudanca_relativa_pct := (rmt - 1) * 100]
  dt[, ic95_inf_pct := (ic95_inf - 1) * 100]
  dt[, ic95_sup_pct := (ic95_sup - 1) * 100]

  dt[, p_valor_formatado := formatar_p(p_valor)]
  dt[, rmt_ic95 := paste0(
    formatar_num(rmt, 2),
    " (",
    formatar_num(ic95_inf, 2),
    "; ",
    formatar_num(ic95_sup, 2),
    ")"
  )]

  dt[, interpretacao := interpretar_rmt(rmt, p_valor)]

  dt
}

rodar_modelos <- function(base_input, analise_nome, faixas_para_rodar) {
  lista_resultados <- list()
  lista_log <- list()
  contador_resultados <- 1L
  contador_log <- 1L

  formula_modelo <- montar_formula_marcador()

  for (i_marc in seq_len(nrow(marcadores_disponiveis))) {
    marcador <- marcadores_disponiveis$marcador[i_marc]
    marcador_label <- marcadores_disponiveis$marcador_label[i_marc]
    classe <- marcadores_disponiveis$classe[i_marc]

    cat_col_lag <- paste0(marcador, "_aumento_cat_lag")

    for (faixa_i in faixas_para_rodar) {
      d <- data.table::copy(base_input[as.character(faixa_analise) == as.character(faixa_i)])

      if (!cat_col_lag %in% names(d)) {
        lista_log[[contador_log]] <- data.table::data.table(
          analise = analise_nome,
          faixa_analise = as.character(faixa_i),
          marcador = marcador,
          marcador_label = marcador_label,
          status = "nao_rodado",
          detalhe = paste0("Coluna ausente: ", cat_col_lag)
        )
        contador_log <- contador_log + 1L
        next
      }

      d[, aumento_cat_raw := as.character(get(cat_col_lag))]
      d <- d[aumento_cat_raw %in% categorias_modelo]

      d[, aumento_cat := factor(aumento_cat_raw, levels = categorias_modelo)]
      d[, aumento_cat := stats::relevel(aumento_cat, ref = "Nao teve aumento")]
      d[, code_f := factor(code)]

      d <- d[
        !is.na(aumento_cat) &
          !is.na(periodo_estagnacao) &
          is.finite(log_razao_taxa) &
          is.finite(z_log_taxa_lag) &
          is.finite(ano_c) &
          is.finite(peso_modelo)
      ]

      cond_minima <- nrow(d) >= min_linhas_modelo &&
        data.table::uniqueN(d$code) >= min_municipios_modelo &&
        data.table::uniqueN(d$ano) >= min_anos_modelo &&
        all(c("Nao teve aumento", "Teve aumento") %in% as.character(d$aumento_cat)) &&
        all(c("Pre_estagnacao", "Estagnacao") %in% as.character(d$periodo_estagnacao))

      if (!cond_minima) {
        lista_log[[contador_log]] <- data.table::data.table(
          analise = analise_nome,
          faixa_analise = as.character(faixa_i),
          marcador = marcador,
          marcador_label = marcador_label,
          status = "nao_rodado",
          detalhe = "Amostra insuficiente, referencia/comparador ausente ou periodo ausente.",
          n_linhas = nrow(d),
          n_municipios = data.table::uniqueN(d$code),
          n_anos = data.table::uniqueN(d$ano),
          tem_ref = "Nao teve aumento" %in% as.character(d$aumento_cat),
          tem_comparador = "Teve aumento" %in% as.character(d$aumento_cat),
          tem_pre = any(as.character(d$periodo_estagnacao) == "Pre_estagnacao"),
          tem_estagnacao = any(as.character(d$periodo_estagnacao) == "Estagnacao")
        )
        contador_log <- contador_log + 1L
        next
      }

      vars_modelo <- unique(c(all.vars(stats::terms(formula_modelo)), "code", "peso_modelo"))
      vars_modelo <- intersect(vars_modelo, names(d))

      dpol <- d[stats::complete.cases(d[, vars_modelo, with = FALSE])]

      if (nrow(dpol) < min_linhas_modelo ||
          data.table::uniqueN(dpol$code) < min_municipios_modelo ||
          !all(c("Nao teve aumento", "Teve aumento") %in% as.character(dpol$aumento_cat))) {
        lista_log[[contador_log]] <- data.table::data.table(
          analise = analise_nome,
          faixa_analise = as.character(faixa_i),
          marcador = marcador,
          marcador_label = marcador_label,
          status = "nao_rodado",
          detalhe = "Base completa insuficiente apos remocao de ausentes.",
          n_linhas = nrow(dpol),
          n_municipios = data.table::uniqueN(dpol$code),
          n_anos = data.table::uniqueN(dpol$ano)
        )
        contador_log <- contador_log + 1L
        next
      }

      dpol[, aumento_cat := droplevels(aumento_cat)]
      dpol[, aumento_cat := stats::relevel(aumento_cat, ref = "Nao teve aumento")]

      fit <- fit_lm_ponderado_safe(formula_modelo, dpol)

      if (is.null(fit$fit)) {
        lista_log[[contador_log]] <- data.table::data.table(
          analise = analise_nome,
          faixa_analise = as.character(faixa_i),
          marcador = marcador,
          marcador_label = marcador_label,
          status = "erro_modelo",
          detalhe = fit$erro
        )
        contador_log <- contador_log + 1L
        next
      }

      vc_cl <- vcov_cluster_safe(fit$fit, cluster = dpol$code)

      ano_ref_pre <- stats::median(dpol$ano[dpol$periodo_estagnacao == "Pre_estagnacao"], na.rm = TRUE)
      ano_ref_est <- stats::median(dpol$ano[dpol$periodo_estagnacao == "Estagnacao"], na.rm = TRUE)

      if (!is.finite(ano_ref_pre)) ano_ref_pre <- stats::median(dpol$ano, na.rm = TRUE)
      if (!is.finite(ano_ref_est)) ano_ref_est <- stats::median(dpol$ano, na.rm = TRUE)

      nd_ref_est <- data.table::data.table(
        aumento_cat = factor("Nao teve aumento", levels = levels(dpol$aumento_cat)),
        periodo_estagnacao = factor("Estagnacao", levels = levels(dpol$periodo_estagnacao)),
        ano_c = as.numeric(ano_ref_est - ano_inicio_estagnacao),
        z_log_taxa_lag = 0,
        code_f = dpol$code_f[1]
      )

      nd_cat_est <- data.table::copy(nd_ref_est)
      nd_cat_est[, aumento_cat := factor("Teve aumento", levels = levels(dpol$aumento_cat))]

      # 1) Teve aumento vs nao teve aumento durante estagnacao.
      contr_est <- linear_contrast_newdata(
        fit = fit$fit,
        vc = vc_cl,
        newdata_list = list(nd_cat_est, nd_ref_est),
        weights = c(1, -1)
      )

      contr_est[, `:=`(
        analise = analise_nome,
        faixa_analise = as.character(faixa_i),
        marcador = marcador,
        marcador_label = marcador_label,
        classe = classe,
        categoria = "Teve aumento",
        referencia = "Nao teve aumento",
        tipo_contraste = "durante_estagnacao",
        contraste = "Teve aumento vs Nao teve aumento",
        n_linhas = nrow(dpol),
        n_municipios = data.table::uniqueN(dpol$code),
        n_anos = data.table::uniqueN(dpol$ano),
        ano_ref = as.character(ano_ref_est),
        formula = paste(deparse(formula_modelo), collapse = " ")
      )]

      lista_resultados[[contador_resultados]] <- finalizar_contraste(contr_est)
      contador_resultados <- contador_resultados + 1L

      # 2) Diferenca-em-diferencas.
      nd_ref_pre <- data.table::copy(nd_ref_est)
      nd_ref_pre[, periodo_estagnacao := factor("Pre_estagnacao", levels = levels(dpol$periodo_estagnacao))]
      nd_ref_pre[, ano_c := as.numeric(ano_ref_pre - ano_inicio_estagnacao)]

      nd_cat_pre <- data.table::copy(nd_ref_pre)
      nd_cat_pre[, aumento_cat := factor("Teve aumento", levels = levels(dpol$aumento_cat))]

      contr_diff <- linear_contrast_newdata(
        fit = fit$fit,
        vc = vc_cl,
        newdata_list = list(nd_cat_est, nd_ref_est, nd_cat_pre, nd_ref_pre),
        weights = c(1, -1, -1, 1)
      )

      contr_diff[, `:=`(
        analise = analise_nome,
        faixa_analise = as.character(faixa_i),
        marcador = marcador,
        marcador_label = marcador_label,
        classe = classe,
        categoria = "Teve aumento",
        referencia = "Nao teve aumento",
        tipo_contraste = "diferenca_estagnacao_vs_pre",
        contraste = "Teve aumento vs Nao teve aumento: Estagnacao - Pre",
        n_linhas = nrow(dpol),
        n_municipios = data.table::uniqueN(dpol$code),
        n_anos = data.table::uniqueN(dpol$ano),
        ano_ref = paste0(ano_ref_est, " vs ", ano_ref_pre),
        formula = paste(deparse(formula_modelo), collapse = " ")
      )]

      lista_resultados[[contador_resultados]] <- finalizar_contraste(contr_diff)
      contador_resultados <- contador_resultados + 1L

      lista_log[[contador_log]] <- data.table::data.table(
        analise = analise_nome,
        faixa_analise = as.character(faixa_i),
        marcador = marcador,
        marcador_label = marcador_label,
        status = "ok",
        detalhe = NA_character_,
        n_linhas = nrow(dpol),
        n_municipios = data.table::uniqueN(dpol$code),
        n_anos = data.table::uniqueN(dpol$ano)
      )
      contador_log <- contador_log + 1L
    }
  }

  list(
    resultados = rbindlist_seguro(lista_resultados),
    log = rbindlist_seguro(lista_log)
  )
}

res_principal <- rodar_modelos(
  base_input = base_mudanca,
  analise_nome = "Principal",
  faixas_para_rodar = faixa_levels
)

resultados_principal <- res_principal$resultados
log_modelos <- res_principal$log

fwrite_safe(resultados_principal, file.path(dir_tabelas, "02_resultados_principal_rmt_dicotomico.csv"))
fwrite_safe(log_modelos, file.path(dir_logs, "04_log_modelos.csv"))


# ============================================================
# 12. FOREST PLOTS EM GGPLOT2
# ============================================================

preparar_forest_data <- function(dt) {
  if (nrow(dt) == 0) return(data.table::data.table())

  dt <- data.table::copy(dt)

  dt <- dt[
    is.finite(rmt) &
      is.finite(ic95_inf) &
      is.finite(ic95_sup) &
      rmt > 0 &
      ic95_inf > 0 &
      ic95_sup > 0
  ]

  if (nrow(dt) == 0) return(data.table::data.table())

  data.table::setorder(dt, classe, marcador_label)

  dt[, label_y := marcador_label]
  dt[, y := rev(seq_len(.N))]

  dt[, rmt_plot := pmin(pmax(rmt, clip_x_min), clip_x_max)]
  dt[, ic95_inf_plot := pmin(pmax(ic95_inf, clip_x_min), clip_x_max)]
  dt[, ic95_sup_plot := pmin(pmax(ic95_sup, clip_x_min), clip_x_max)]

  dt[, ci_truncado := (!is.na(ic95_inf) & ic95_inf < clip_x_min) | (!is.na(ic95_sup) & ic95_sup > clip_x_max)]

  dt[, texto_valor := paste0(
    formatar_num(rmt, 2),
    " (",
    formatar_num(ic95_inf, 2),
    "; ",
    formatar_num(ic95_sup, 2),
    ")   p=",
    formatar_p(p_valor)
  )]

  dt[ci_truncado == TRUE & !is.na(texto_valor), texto_valor := paste0(texto_valor, " *")]

  dt
}

plotar_forest_gg <- function(dt, tipo_contraste_i, faixa_i, dir_saida) {
  d <- data.table::copy(dt[
    tipo_contraste == tipo_contraste_i &
      as.character(faixa_analise) == as.character(faixa_i)
  ])

  subtipo <- if (tipo_contraste_i == "durante_estagnacao") {
    "Teve aumento vs Nao teve aumento durante a estagnacao"
  } else {
    "Diferenca-em-diferencas: contraste na estagnacao menos contraste pre-estagnacao"
  }

  arquivo <- file.path(
    dir_saida,
    paste0(
      "forest_base_anual_dicotomico_",
      safe_file(tipo_contraste_i, 40),
      "_",
      safe_file(faixa_i, 40),
      ".png"
    )
  )

  if (nrow(d) == 0) {
    criar_png_sem_resultado(
      file = arquivo,
      titulo = paste0("Sem resultados - ", faixa_i),
      subtitulo = paste0("Tipo de contraste: ", tipo_contraste_i, ". Verifique logs/04_log_modelos.csv.")
    )
    return(invisible(arquivo))
  }

  plot_dt <- preparar_forest_data(d)

  if (nrow(plot_dt) == 0) {
    criar_png_sem_resultado(
      file = arquivo,
      titulo = paste0("Sem estimativas validas - ", faixa_i),
      subtitulo = paste0("Tipo de contraste: ", tipo_contraste_i, ". Verifique ICs, RMT e logs.")
    )
    return(invisible(arquivo))
  }

  n_linhas <- nrow(plot_dt)
  altura <- max(altura_minima_png, base_altura_por_linha * n_linhas + 2.6)

  x_texto <- clip_x_max * 1.18
  x_lim_sup <- clip_x_max * 2.35

  breaks_x <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4)

  p <- ggplot2::ggplot(plot_dt, ggplot2::aes(y = y)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.45,
      color = "grey35"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = ic95_inf_plot,
        xend = ic95_sup_plot,
        yend = y
      ),
      linewidth = 0.85,
      color = "#1D3557",
      lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = rmt_plot),
      size = 2.8,
      color = "#1D3557"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = x_texto,
        label = texto_valor
      ),
      hjust = 0,
      size = 3.6,
      color = "grey10"
    ) +
    ggplot2::scale_x_log10(
      limits = c(clip_x_min, x_lim_sup),
      breaks = breaks_x,
      labels = scales::number_format(decimal.mark = ",", accuracy = 0.01)
    ) +
    ggplot2::scale_y_continuous(
      breaks = plot_dt$y,
      labels = plot_dt$label_y,
      expand = ggplot2::expansion(mult = c(0.04, 0.07))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = paste0("Expansao dicotomica das politicas | ", faixa_i),
      subtitle = paste0(
        subtipo,
        "\nRMT < 1 indica maior reducao anual ou menor estagnacao; referencia = Nao teve aumento. ",
        "Asterisco indica IC truncado no limite visual."
      ),
      x = "Razao das mudancas anuais da taxa (RMT)",
      y = NULL,
      caption = "Modelo linear ponderado para log_razao_taxa, com erro-padrao robusto clusterizado por municipio."
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 17),
      plot.subtitle = ggplot2::element_text(size = 11.5, lineheight = 1.05),
      plot.caption = ggplot2::element_text(size = 9.5, hjust = 0),
      axis.text.y = ggplot2::element_text(size = 11.2, color = "grey10"),
      axis.text.x = ggplot2::element_text(size = 10.5),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 230, 10, 10)
    ) +
    ggplot2::annotate(
      "text",
      x = x_texto,
      y = max(plot_dt$y) + 0.75,
      label = "RMT (IC95%)   p",
      hjust = 0,
      size = 3.7,
      fontface = "bold",
      color = "grey10"
    )

  ggplot2::ggsave(
    filename = arquivo,
    plot = p,
    width = largura_png,
    height = altura,
    dpi = dpi_png,
    bg = "white",
    limitsize = FALSE
  )

  invisible(arquivo)
}

for (fx in faixa_levels) {
  for (tc in c("durante_estagnacao", "diferenca_estagnacao_vs_pre")) {
    plotar_forest_gg(
      dt = resultados_principal,
      tipo_contraste_i = tc,
      faixa_i = as.character(fx),
      dir_saida = dir_forest
    )
  }
}


# ============================================================
# 13. EXCEL E README
# ============================================================

wb <- openxlsx::createWorkbook()
add_sheet_safe(wb, "pontos_corte", pontos_corte_aumento)
add_sheet_safe(wb, "resultados", resultados_principal)
add_sheet_safe(wb, "log_modelos", log_modelos)
add_sheet_safe(wb, "mapa_colunas", mapa_fontes)

openxlsx::saveWorkbook(
  wb,
  file.path(dir_tabelas, "resultados_rmt_dicotomico_base_anual_v9.xlsx"),
  overwrite = TRUE
)

readme_txt <- c(
  "MODELO V9 - EXPANSAO DICOTOMICA USANDO BASE ANUAL JA ACHATADA",
  "",
  "Entrada esperada:",
  "base_anual_mortalidade_contexto",
  "",
  "Esta versao nao reprocessa bases mensais.",
  "Usa diretamente a base anual municipio x ano x faixa_etaria.",
  "",
  "Desfecho:",
  "log_razao_taxa = log((obitos_t + 0.5) / risco_t) - log((obitos_t-1 + 0.5) / risco_t-1).",
  "",
  "Medida:",
  "RMT = razao das mudancas anuais da taxa.",
  "Nao e IRR de mortalidade absoluta.",
  "",
  "Exposicao:",
  "Expansao acumulada do marcador em relacao ao baseline municipal.",
  "",
  "Categorias:",
  "Nao teve aumento = referencia.",
  "Teve aumento = expansao acima da banda neutra do marcador.",
  "",
  "Interpretacao:",
  "RMT < 1: maior reducao anual ou menor estagnacao.",
  "RMT = 1: mudanca anual semelhante a referencia.",
  "RMT > 1: menor reducao anual ou maior estagnacao.",
  "",
  "Contrastes:",
  "1. durante_estagnacao: Teve aumento vs Nao teve aumento durante a estagnacao.",
  "2. diferenca_estagnacao_vs_pre: [Teve aumento vs Nao teve aumento] na estagnacao menos [Teve aumento vs Nao teve aumento] antes da estagnacao.",
  "",
  "Arquivos principais:",
  "01_pontos_corte_aumento_dicotomico.csv",
  "02_resultados_principal_rmt_dicotomico.csv",
  "base_mudanca_anual_log_razao_taxa.csv",
  "resultados_rmt_dicotomico_base_anual_v9.xlsx",
  "",
  "Graficos:",
  "graficos/forest_por_faixa",
  "",
  "Se alguma figura mostrar mensagem de sem resultados, consulte logs/04_log_modelos.csv."
)

writeLines(readme_txt, con = file.path(out_dir, "README_modelo_v9.txt"))


# ============================================================
# 14. RESUMO FINAL
# ============================================================

message("Analise concluida.")
message("Diretorio de saida: ", out_dir)
message("Excel principal: ", file.path(dir_tabelas, "resultados_rmt_dicotomico_base_anual_v9.xlsx"))
message("Resultados: ", file.path(dir_tabelas, "02_resultados_principal_rmt_dicotomico.csv"))
message("")
message("Interpretacao da RMT:")
message("  RMT < 1: maior reducao anual ou menor estagnacao.")
message("  RMT > 1: menor reducao anual ou maior estagnacao.")
message("")
message("Resumo dos modelos:")
if (exists("log_modelos") && nrow(log_modelos) > 0) {
  print(log_modelos[, .N, by = .(status, detalhe)][order(status, detalhe)])
}
