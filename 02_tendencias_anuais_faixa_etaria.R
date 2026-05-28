# ============================================================
# 05 - TENDENCIAS ANUAIS DE MORTALIDADE <5 ANOS POR FAIXA ETARIA
# Base anual leve criada pelo join novo
# Unidade de entrada esperada: municipio x ano x faixa_etaria
# Unidade do grafico: Brasil x ano x faixa_etaria
#
# Objeto principal esperado:
#   base_anual_mortalidade_contexto
#
# Desfecho:
#   obitos
#
# Denominador preferencial:
#   crianca_ano_risco
#
# Taxa principal:
#   taxa anual por 1.000 crianca-ano
# ============================================================

# ============================================================
# 0. PACOTES
# ============================================================

required_packages <- c(
  "data.table",
  "ggplot2",
  "scales",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(ggplot2)
library(scales)
library(stringr)

# ============================================================
# 1. PARAMETROS
# ============================================================

if (!exists("base_dir")) {
  base_dir <- Sys.getenv("U5MORTALITY_BASE_DIR", unset = getwd())
}

objeto_base <- "base_anual_mortalidade_contexto"

arquivo_rds_base <- file.path(
  base_dir,
  "base_anual_leve",
  "base_anual_mortalidade_contexto.rds"
)

arquivo_csv_base <- file.path(
  base_dir,
  "base_anual_leve",
  "base_anual_mortalidade_contexto.csv"
)

dir_saida <- file.path(base_dir, "tendencias_anuais_faixa_etaria")
dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)

salvar_csv <- TRUE
salvar_rds <- TRUE
salvar_png <- TRUE
salvar_tiff <- TRUE

dpi_saida <- 600
largura_fig <- 10
altura_fig <- 6.5

multiplicador_taxa <- 1000
rotulo_geral <- "Geral <5 anos"

faixas_total_menor5 <- c(
  "Geral <5 anos",
  "00-59 meses",
  "0-59 meses",
  "00 a 59 meses",
  "0 a 59 meses",
  "<5 anos",
  "Menores de 5 anos"
)

ordem_faixas_desagregadas <- c(
  "00-11 meses",
  "12-23 meses",
  "24-35 meses",
  "36-47 meses",
  "48-59 meses"
)

rotulos_faixas_en <- c(
  "Geral <5 anos" = "Overall <5 years",
  "00-11 meses" = "0-11 months",
  "12-23 meses" = "12-23 months",
  "24-35 meses" = "24-35 months",
  "36-47 meses" = "36-47 months",
  "48-59 meses" = "48-59 months"
)

# Para artigo em inglês. Troque para FALSE se quiser manter rótulos originais.
usar_rotulos_ingles <- TRUE

# ============================================================
# 2. FUNCOES AUXILIARES
# ============================================================

as_num <- function(x) {
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

salvar_figura <- function(plot, nome_base, width = largura_fig, height = altura_fig) {
  arquivos <- character(0)

  if (isTRUE(salvar_png)) {
    arq_png <- file.path(dir_saida, paste0(nome_base, ".png"))
    ggplot2::ggsave(
      filename = arq_png,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi_saida,
      bg = "white"
    )
    arquivos <- c(arquivos, arq_png)
  }

  if (isTRUE(salvar_tiff)) {
    arq_tiff <- file.path(dir_saida, paste0(nome_base, ".tiff"))
    ggplot2::ggsave(
      filename = arq_tiff,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi_saida,
      compression = "lzw",
      bg = "white"
    )
    arquivos <- c(arquivos, arq_tiff)
  }

  invisible(arquivos)
}

# ============================================================
# 3. CARREGAR BASE ANUAL
# ============================================================

if (exists(objeto_base, envir = .GlobalEnv)) {
  base <- copy(get(objeto_base, envir = .GlobalEnv))
} else if (file.exists(arquivo_rds_base)) {
  base <- readRDS(arquivo_rds_base)
} else if (file.exists(arquivo_csv_base)) {
  base <- data.table::fread(arquivo_csv_base)
} else {
  stop(
    "Nao encontrei a base anual. Rode primeiro o join novo para criar ",
    "'base_anual_mortalidade_contexto' ou salve o RDS/CSV em: ",
    file.path(base_dir, "base_anual_leve")
  )
}

setDT(base)

# ============================================================
# 4. CHECAGENS E PADRONIZACAO
# ============================================================

cols_minimas <- c("code", "ano", "faixa_etaria", "obitos")
cols_faltantes <- setdiff(cols_minimas, names(base))

if (length(cols_faltantes) > 0) {
  stop(
    "Colunas obrigatorias ausentes na base anual: ",
    paste(cols_faltantes, collapse = ", ")
  )
}

base[
  ,
  `:=`(
    code = pad_code6(code),
    ano = as.integer(as_num(ano)),
    faixa_etaria = as.character(faixa_etaria),
    obitos = as_num(obitos)
  )
]

if (!"crianca_ano_risco" %in% names(base)) {
  if ("denominador_soma_meses" %in% names(base)) {
    base[, crianca_ano_risco := as_num(denominador_soma_meses) / 12]
  } else if ("denominador" %in% names(base)) {
    # No join novo, 'denominador' corresponde a soma dos meses.
    base[, crianca_ano_risco := as_num(denominador) / 12]
  } else {
    stop(
      "A base nao contem 'crianca_ano_risco', 'denominador_soma_meses' nem 'denominador'."
    )
  }
} else {
  base[, crianca_ano_risco := as_num(crianca_ano_risco)]
}

base <- base[
  !is.na(code) &
    !is.na(ano) &
    !is.na(faixa_etaria) &
    !is.na(obitos) &
    !is.na(crianca_ano_risco) &
    crianca_ano_risco > 0
]

if (nrow(base) == 0) {
  stop("A base ficou vazia apos filtro de crianca_ano_risco > 0.")
}

# Evita misturar mais de um rotulo total.
base[
  faixa_etaria %in% faixas_total_menor5,
  faixa_etaria := rotulo_geral
]

# Checagem de duplicidade estrutural na base anual.
duplicadas <- base[
  ,
  .N,
  by = .(code, ano, faixa_etaria)
][N > 1]

cat("\nDuplicidades por code + ano + faixa_etaria na base anual:\n")
print(duplicadas)

if (nrow(duplicadas) > 0) {
  warning(
    "Ha duplicidades por code + ano + faixa_etaria. ",
    "O script vai agregar antes de calcular as tendencias nacionais."
  )
}

# ============================================================
# 5. AGREGAR TENDENCIA NACIONAL ANUAL
# ============================================================

# Nao misturar 'Geral <5 anos' com faixas desagregadas.
tendencia_anual <- base[
  ,
  .(
    municipios = uniqueN(code),
    obitos = sum(obitos, na.rm = TRUE),
    crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
  ),
  by = .(ano, faixa_etaria)
]

tendencia_anual[
  ,
  taxa_1000_crianca_ano := data.table::fifelse(
    crianca_ano_risco > 0,
    obitos / crianca_ano_risco * multiplicador_taxa,
    NA_real_
  )
]

tendencia_anual <- tendencia_anual[
  is.finite(taxa_1000_crianca_ano)
]

# Versao desagregada para grafico por faixa.
faixas_presentes <- intersect(
  ordem_faixas_desagregadas,
  unique(tendencia_anual$faixa_etaria)
)

if (length(faixas_presentes) == 0) {
  faixas_presentes <- setdiff(
    sort(unique(tendencia_anual$faixa_etaria)),
    rotulo_geral
  )
}

tendencia_faixas <- tendencia_anual[
  faixa_etaria %in% faixas_presentes
]

tendencia_geral <- tendencia_anual[
  faixa_etaria == rotulo_geral
]

if (nrow(tendencia_geral) == 0) {
  # Se por algum motivo o total nao existir, reconstrói pela soma das faixas.
  tendencia_geral <- tendencia_faixas[
    ,
    .(
      municipios = max(municipios, na.rm = TRUE),
      obitos = sum(obitos, na.rm = TRUE),
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
    ),
    by = ano
  ]

  tendencia_geral[
    ,
    `:=`(
      faixa_etaria = rotulo_geral,
      taxa_1000_crianca_ano = obitos / crianca_ano_risco * multiplicador_taxa
    )
  ]
}

# Rotulo de exibicao.
for (dt in list(tendencia_anual, tendencia_faixas, tendencia_geral)) {
  dt[
    ,
    faixa_label := if (isTRUE(usar_rotulos_ingles)) {
      fifelse(
        faixa_etaria %in% names(rotulos_faixas_en),
        unname(rotulos_faixas_en[faixa_etaria]),
        faixa_etaria
      )
    } else {
      faixa_etaria
    }
  ]
}

ordem_labels <- if (isTRUE(usar_rotulos_ingles)) {
  unname(rotulos_faixas_en[c(rotulo_geral, ordem_faixas_desagregadas)])
} else {
  c(rotulo_geral, ordem_faixas_desagregadas)
}

ordem_labels <- ordem_labels[ordem_labels %in% tendencia_anual$faixa_label]

tendencia_anual[, faixa_label := factor(faixa_label, levels = ordem_labels)]
tendencia_faixas[, faixa_label := factor(faixa_label, levels = ordem_labels)]
tendencia_geral[, faixa_label := factor(faixa_label, levels = ordem_labels)]

setorder(tendencia_anual, faixa_etaria, ano)
setorder(tendencia_faixas, faixa_etaria, ano)
setorder(tendencia_geral, ano)

# ============================================================
# 6. SALVAR TABELAS
# ============================================================

if (isTRUE(salvar_csv)) {
  data.table::fwrite(
    tendencia_anual,
    file.path(dir_saida, "tendencia_anual_mortalidade_por_faixa.csv")
  )

  data.table::fwrite(
    tendencia_faixas,
    file.path(dir_saida, "tendencia_anual_mortalidade_faixas_desagregadas.csv")
  )

  data.table::fwrite(
    tendencia_geral,
    file.path(dir_saida, "tendencia_anual_mortalidade_geral_menor5.csv")
  )
}

if (isTRUE(salvar_rds)) {
  saveRDS(
    tendencia_anual,
    file.path(dir_saida, "tendencia_anual_mortalidade_por_faixa.rds")
  )
}

# ============================================================
# 7. GRAFICOS
# ============================================================

tema_linha <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

breaks_ano <- scales::pretty_breaks(n = 10)

# 7.1 Tendencia geral <5 anos
p_geral <- ggplot(
  tendencia_geral,
  aes(x = ano, y = taxa_1000_crianca_ano)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = breaks_ano) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Annual under-5 mortality trend in Brazil",
    subtitle = "Observed mortality rate based on reconstructed child-year risk denominator",
    x = "Year",
    y = "Deaths per 1,000 child-years at risk"
  ) +
  tema_linha

arquivos_geral <- salvar_figura(
  p_geral,
  "figura_tendencia_anual_geral_menor5",
  width = 9,
  height = 5.5
)

# 7.2 Tendencia por faixa etaria - linhas no mesmo painel
if (nrow(tendencia_faixas) > 0) {
  p_faixas <- ggplot(
    tendencia_faixas,
    aes(x = ano, y = taxa_1000_crianca_ano, group = faixa_label, linetype = faixa_label)
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.5) +
    scale_x_continuous(breaks = breaks_ano) +
    scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = "Annual under-5 mortality trends by age group in Brazil",
      subtitle = "Observed mortality rates; age-specific lines are not summed with the overall <5 group",
      x = "Year",
      y = "Deaths per 1,000 child-years at risk"
    ) +
    tema_linha

  arquivos_faixas <- salvar_figura(
    p_faixas,
    "figura_tendencia_anual_faixas_linhas",
    width = 10.5,
    height = 6.5
  )

  # 7.3 Tendencia por faixa etaria - facetado com mesma escala no eixo y
  p_facet <- ggplot(
    tendencia_faixas,
    aes(x = ano, y = taxa_1000_crianca_ano)
  ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 1.3) +
    facet_wrap(~ faixa_label, ncol = 2) +
    scale_x_continuous(breaks = breaks_ano) +
    scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = "Annual under-5 mortality trends by age group in Brazil",
      subtitle = "Same y-axis scale across panels",
      x = "Year",
      y = "Deaths per 1,000 child-years at risk"
    ) +
    tema_linha +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )

  arquivos_facet <- salvar_figura(
    p_facet,
    "figura_tendencia_anual_faixas_facetada_mesma_escala",
    width = 11,
    height = 8
  )
} else {
  arquivos_faixas <- character(0)
  arquivos_facet <- character(0)
  warning("Nao ha faixas desagregadas disponiveis para grafico por faixa etaria.")
}

# 7.4 Grafico combinado: geral + faixas, com alerta conceitual no subtitulo.
tendencia_comb <- rbindlist(
  list(tendencia_geral, tendencia_faixas),
  use.names = TRUE,
  fill = TRUE
)

p_comb <- ggplot(
  tendencia_comb,
  aes(x = ano, y = taxa_1000_crianca_ano, group = faixa_label, linetype = faixa_label)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.4) +
  scale_x_continuous(breaks = breaks_ano) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Annual under-5 mortality trends in Brazil",
    subtitle = "Overall <5 years is shown for reference; do not sum it with age-specific strata",
    x = "Year",
    y = "Deaths per 1,000 child-years at risk"
  ) +
  tema_linha

arquivos_comb <- salvar_figura(
  p_comb,
  "figura_tendencia_anual_geral_e_faixas_linhas",
  width = 11,
  height = 6.8
)

# ============================================================
# 8. OBJETO FINAL NO AMBIENTE E RESUMO
# ============================================================

trend_results <- list(
  tendencia_anual = tendencia_anual,
  tendencia_faixas = tendencia_faixas,
  tendencia_geral = tendencia_geral,
  grafico_geral = p_geral,
  grafico_faixas = if (exists("p_faixas")) p_faixas else NULL,
  grafico_facetado = if (exists("p_facet")) p_facet else NULL,
  grafico_combinado = p_comb,
  arquivos = c(
    arquivos_geral,
    arquivos_faixas,
    arquivos_facet,
    arquivos_comb
  )
)

cat("\nResumo da tendencia anual geral <5 anos:\n")
print(
  tendencia_geral[
    ,
    .(
      ano_min = min(ano, na.rm = TRUE),
      ano_max = max(ano, na.rm = TRUE),
      obitos_total = sum(obitos, na.rm = TRUE),
      crianca_ano_risco_total = sum(crianca_ano_risco, na.rm = TRUE),
      taxa_inicial = taxa_1000_crianca_ano[which.min(ano)],
      taxa_final = taxa_1000_crianca_ano[which.max(ano)],
      variacao_pct = (
        taxa_1000_crianca_ano[which.max(ano)] /
          taxa_1000_crianca_ano[which.min(ano)] - 1
      ) * 100
    )
  ]
)

cat("\nArquivos salvos em:\n")
cat(dir_saida, "\n")

cat("\nObjeto criado no ambiente: trend_results\n")
