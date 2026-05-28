
# ============================================================
# FUNCOES COMUNS - BASE ANUAL MORTALIDADE <5 ANOS
# Adaptado para o join anual:
#   objeto principal: base_anual_mortalidade_contexto
#   unidade: municipio x ano x faixa_etaria
#   denominador analitico: crianca_ano_risco
#   desfecho: obitos
# ============================================================

if (!exists("base_dir")) {
  base_dir <- Sys.getenv("U5MORTALITY_BASE_DIR", unset = getwd())
}

# Recorte temporal analítico definido pelo projeto.
ANO_FINAL <- 2021

instalar_carregar <- function(pacotes) {
  faltantes <- pacotes[
    !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(faltantes) > 0) {
    install.packages(faltantes)
  }
  invisible(lapply(pacotes, library, character.only = TRUE))
}

pad_code6 <- function(x) {
  x <- as.character(x)
  x <- gsub("\\D", "", x)
  x[x == ""] <- NA_character_

  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)

  out[ok & nchar(x) >= 7] <- substr(x[ok & nchar(x) >= 7], 1, 6)
  out[ok & nchar(x) == 6] <- x[ok & nchar(x) == 6]
  out[ok & nchar(x) < 6]  <- sprintf("%06d", as.integer(x[ok & nchar(x) < 6]))

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

nome_arquivo_seguro <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

formatar_p <- function(p) {
  out <- rep(NA_character_, length(p))
  out[is.na(p)] <- NA_character_
  out[!is.na(p) & p < 0.001] <- "<0,001"
  out[!is.na(p) & p >= 0.001] <- formatC(
    p[!is.na(p) & p >= 0.001],
    format = "f",
    digits = 3,
    decimal.mark = ","
  )
  out
}

formatar_num <- function(x, digits = 2) {
  out <- rep(NA_character_, length(x))
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

criar_regiao <- function(code) {
  uf <- substr(as.character(code), 1, 2)
  data.table::fcase(
    uf %in% c("11", "12", "13", "14", "15", "16", "17"), "Norte",
    uf %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29"), "Nordeste",
    uf %in% c("31", "32", "33", "35"), "Sudeste",
    uf %in% c("41", "42", "43"), "Sul",
    uf %in% c("50", "51", "52", "53"), "Centro-Oeste",
    default = NA_character_
  )
}

carregar_base_anual <- function(base_dir = base_dir) {

  if (exists("base_anual_mortalidade_contexto", envir = .GlobalEnv)) {
    base <- data.table::copy(get("base_anual_mortalidade_contexto", envir = .GlobalEnv))
  } else if (exists("df", envir = .GlobalEnv)) {
    base <- data.table::copy(get("df", envir = .GlobalEnv))
  } else {
    candidatos <- c(
      file.path(base_dir, "base_anual_leve", "base_anual_mortalidade_contexto.rds"),
      file.path(base_dir, "base_anual_mortalidade_contexto.rds"),
      file.path(base_dir, "base_anual_leve", "base_anual_mortalidade_contexto.csv"),
      file.path(base_dir, "base_anual_mortalidade_contexto.csv")
    )

    arq <- candidatos[file.exists(candidatos)][1]

    if (is.na(arq)) {
      stop(
        "Nao encontrei a base anual. Rode primeiro o join anual ou salve o objeto em: ",
        file.path(base_dir, "base_anual_leve", "base_anual_mortalidade_contexto.rds"),
        "\nAlternativa: deixe o objeto 'base_anual_mortalidade_contexto' no ambiente."
      )
    }

    if (grepl("\\.rds$", arq, ignore.case = TRUE)) {
      base <- readRDS(arq)
    } else {
      base <- data.table::fread(arq)
    }
  }

  data.table::setDT(base)

  if (!"code" %in% names(base) && "code_muni" %in% names(base)) {
    base[, code := code_muni]
  }

  cols_obrigatorias <- c("code", "ano", "faixa_etaria", "obitos")
  faltantes <- setdiff(cols_obrigatorias, names(base))
  if (length(faltantes) > 0) {
    stop("Colunas obrigatorias ausentes na base anual: ", paste(faltantes, collapse = ", "))
  }

  base[
    ,
    `:=`(
      code = pad_code6(code),
      ano = as.integer(to_num(ano)),
      faixa_etaria = as.character(faixa_etaria),
      obitos = to_num(obitos)
    )
  ]

  if (!"crianca_ano_risco" %in% names(base)) {
    if ("denominador_soma_meses" %in% names(base)) {
      base[, crianca_ano_risco := to_num(denominador_soma_meses) / 12]
    } else if ("denominador" %in% names(base)) {
      # No join anual, denominador = soma dos denominadores mensais.
      base[, crianca_ano_risco := to_num(denominador) / 12]
    } else {
      stop("Nao encontrei 'crianca_ano_risco', 'denominador_soma_meses' nem 'denominador'.")
    }
  } else {
    base[, crianca_ano_risco := to_num(crianca_ano_risco)]
  }

  if (!"denominador_soma_meses" %in% names(base)) {
    if ("denominador" %in% names(base)) {
      base[, denominador_soma_meses := to_num(denominador)]
    } else {
      base[, denominador_soma_meses := crianca_ano_risco * 12]
    }
  } else {
    base[, denominador_soma_meses := to_num(denominador_soma_meses)]
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
    stop("A base anual ficou vazia apos filtros minimos.")
  }

  # Alias padronizados para compatibilidade com scripts antigos.
  if (!"aps_proporcao_cobertura_estrategia_saude_familia" %in% names(base)) {
    if ("cobertura_esf_pct" %in% names(base)) {
      base[, aps_proporcao_cobertura_estrategia_saude_familia := to_num(cobertura_esf_pct)]
    }
  } else {
    base[, aps_proporcao_cobertura_estrategia_saude_familia :=
           to_num(aps_proporcao_cobertura_estrategia_saude_familia)]
  }

  if ("aps_proporcao_cobertura_estrategia_saude_familia" %in% names(base)) {
    base[, cobertura_esf_pct := aps_proporcao_cobertura_estrategia_saude_familia]
  }

  if (!"mais_medicos_prof_ativos" %in% names(base)) {
    if ("total_prof_ativos" %in% names(base)) {
      base[, mais_medicos_prof_ativos := to_num(total_prof_ativos)]
    }
  } else {
    base[, mais_medicos_prof_ativos := to_num(mais_medicos_prof_ativos)]
  }

  vars_num <- intersect(
    c(
      "total_beneficios_bf",
      "total_prof_ativos",
      "mais_medicos_prof_ativos",
      "cobertura_urbana_agua_pct",
      "cobertura_urbana_esgoto_pct",
      "cobertura_bcg",
      "cobertura_penta",
      "cobertura_pneumococica",
      "cobertura_rotavirus",
      "n_favelas",
      "bf_beneficios_por_1000_crianca",
      "mais_medicos_por_10000_crianca",
      "favelas_por_10000_crianca"
    ),
    names(base)
  )

  for (v in vars_num) base[, (v) := to_num(get(v))]

  if (!"taxa_1000_crianca_ano" %in% names(base)) {
    base[, taxa_1000_crianca_ano := obitos / crianca_ano_risco * 1000]
  } else {
    base[, taxa_1000_crianca_ano := to_num(taxa_1000_crianca_ano)]
  }

  if (!"regiao" %in% names(base)) {
    base[, regiao := criar_regiao(code)]
  }

  base[
    ,
    regiao := factor(
      regiao,
      levels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
    )
  ]

  # Normaliza rotulo do total quando vier de scripts antigos.
  rotulos_total <- c(
    "00-59 meses", "0-59 meses", "00 a 59 meses", "0 a 59 meses",
    "Geral <5 anos", "<5 anos", "Menores de 5 anos"
  )
  base[faixa_etaria %in% rotulos_total, faixa_etaria := "Geral <5 anos"]

  dup <- base[, .N, by = .(code, ano, faixa_etaria)][N > 1]
  if (nrow(dup) > 0) {
    warning(
      "Ha duplicidade em code + ano + faixa_etaria. ",
      "Agregando por soma de obitos/denominador e maior valor anual das politicas."
    )

    vars_contexto <- setdiff(
      names(base),
      c("obitos", "denominador_soma_meses", "crianca_ano_risco", "taxa_1000_crianca_ano")
    )

    base <- base[
      ,
      c(
        list(
          obitos = sum(obitos, na.rm = TRUE),
          denominador_soma_meses = sum(denominador_soma_meses, na.rm = TRUE),
          crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
        ),
        lapply(.SD, function(x) {
          if (is.numeric(x)) {
            if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
          } else {
            x <- x[!is.na(x)]
            if (length(x) == 0) NA_character_ else as.character(x[1])
          }
        })
      ),
      by = .(code, ano, faixa_etaria),
      .SDcols = setdiff(vars_contexto, c("code", "ano", "faixa_etaria"))
    ]

    base[, taxa_1000_crianca_ano := obitos / crianca_ano_risco * 1000]
  }

  data.table::setorder(base, code, ano, faixa_etaria)

  base
}

criar_tercil_anual <- function(dt, var, nome_saida = "exposicao_cat") {
  dt <- data.table::copy(dt)
  dt[, valor_exposicao := to_num(get(var))]

  dt[
    ,
    (nome_saida) := {
      x <- valor_exposicao
      ok <- !is.na(x)
      out <- rep(NA_character_, .N)

      if (sum(ok) >= 3 && data.table::uniqueN(x[ok]) >= 3) {
        r <- rank(x[ok], ties.method = "average", na.last = "keep")
        p <- r / max(r, na.rm = TRUE)
        out[ok] <- data.table::fcase(
          p <= 1/3, "Baixo",
          p <= 2/3, "Médio",
          default = "Alto"
        )
      }

      out
    },
    by = ano
  ]

  dt[
    ,
    (nome_saida) := factor(
      get(nome_saida),
      levels = c("Baixo", "Médio", "Alto")
    )
  ]

  dt[!is.na(get(nome_saida))]
}

rotulos_politicas <- c(
  aps_proporcao_cobertura_estrategia_saude_familia = "APS/ESF coverage",
  total_beneficios_bf = "Bolsa Familia benefits",
  bf_beneficios_por_1000_crianca = "Bolsa Familia benefits per 1,000 child-years",
  total_prof_ativos = "Mais Medicos active physicians",
  mais_medicos_prof_ativos = "Mais Medicos active physicians",
  mais_medicos_por_10000_crianca = "Mais Medicos physicians per 10,000 child-years",
  cobertura_urbana_agua_pct = "Urban water coverage",
  cobertura_urbana_esgoto_pct = "Urban sewerage coverage",
  cobertura_bcg = "BCG vaccine coverage",
  cobertura_penta = "Pentavalent vaccine coverage",
  cobertura_pneumococica = "Pneumococcal vaccine coverage",
  cobertura_rotavirus = "Rotavirus vaccine coverage",
  n_favelas = "Urban communities/favelas",
  favelas_por_10000_crianca = "Urban communities/favelas per 10,000 child-years"
)

rotulo_politica <- function(x) {
  if (x %in% names(rotulos_politicas)) return(unname(rotulos_politicas[x]))
  gsub("_", " ", x)
}

politicas_disponiveis <- function(base) {
  preferidas <- c(
    "aps_proporcao_cobertura_estrategia_saude_familia",
    "bf_beneficios_por_1000_crianca",
    "mais_medicos_por_10000_crianca",
    "cobertura_urbana_agua_pct",
    "cobertura_urbana_esgoto_pct",
    "cobertura_bcg",
    "cobertura_penta",
    "cobertura_pneumococica",
    "cobertura_rotavirus",
    "favelas_por_10000_crianca"
  )

  fallback <- c(
    "total_beneficios_bf",
    "mais_medicos_prof_ativos",
    "n_favelas"
  )

  out <- intersect(preferidas, names(base))

  if (!"bf_beneficios_por_1000_crianca" %in% out &&
      "total_beneficios_bf" %in% names(base)) {
    out <- c(out, "total_beneficios_bf")
  }

  if (!"mais_medicos_por_10000_crianca" %in% out &&
      "mais_medicos_prof_ativos" %in% names(base)) {
    out <- c(out, "mais_medicos_prof_ativos")
  }

  if (!"favelas_por_10000_crianca" %in% out &&
      "n_favelas" %in% names(base)) {
    out <- c(out, "n_favelas")
  }

  out <- unique(out)

  out[vapply(out, function(v) {
    x <- base[[v]]
    sum(!is.na(x)) > 0 && data.table::uniqueN(x, na.rm = TRUE) >= 3
  }, logical(1))]
}

# ============================================================
# 04 - DIAGNOSTICO ANUAL DO DENOMINADOR
# Origem: teste.R
# Adaptacao: usa diretamente a base anual do novo join.
# Remove checagens municipio-mes, pois a nova unidade e municipio x ano x faixa_etaria.
# ============================================================

instalar_carregar(c(
  "data.table", "ggplot2", "scales"
))

dir_saida <- file.path(base_dir, "resultados_base_anual_diagnostico_denominador")
dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "tabelas"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "figuras"), showWarnings = FALSE, recursive = TRUE)

base_anual <- carregar_base_anual(base_dir)

# Mantem apenas anos ate 2021.
base_anual <- base_anual[ano <= ANO_FINAL]

if (nrow(base_anual) == 0) {
  stop("A base anual ficou vazia apos aplicar o recorte ate ", ANO_FINAL, ".")
}

cat("\nRecorte temporal aplicado: ate ", ANO_FINAL, ".\n", sep = "")

cat("\nResumo da base anual para diagnostico:\n")
print(
  base_anual[
    ,
    .(
      linhas = .N,
      municipios = data.table::uniqueN(code),
      anos = data.table::uniqueN(ano),
      ano_min = min(ano, na.rm = TRUE),
      ano_max = max(ano, na.rm = TRUE),
      faixas = data.table::uniqueN(faixa_etaria),
      obitos = sum(obitos, na.rm = TRUE),
      denominador_soma_meses = sum(denominador_soma_meses, na.rm = TRUE),
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
    )
  ]
)

duplicadas <- base_anual[
  ,
  .N,
  by = .(code, ano, faixa_etaria)
][N > 1]

cat("\nDuplicidades por code + ano + faixa_etaria:\n")
print(duplicadas)

data.table::fwrite(
  duplicadas,
  file.path(dir_saida, "tabelas", "duplicidades_code_ano_faixa.csv")
)

den_total_brasil_ano <- base_anual[
  faixa_etaria == "Geral <5 anos",
  .(
    linhas = .N,
    municipios = data.table::uniqueN(code),
    n_denominador_na = sum(is.na(crianca_ano_risco)),
    n_denominador_zero = sum(crianca_ano_risco == 0, na.rm = TRUE),
    n_denominador_negativo = sum(crianca_ano_risco < 0, na.rm = TRUE),
    denominador_soma_meses = sum(denominador_soma_meses, na.rm = TRUE),
    crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE),
    obitos = sum(obitos, na.rm = TRUE),
    taxa_1000_crianca_ano = sum(obitos, na.rm = TRUE) /
      sum(crianca_ano_risco, na.rm = TRUE) * 1000
  ),
  by = ano
][order(ano)]

den_total_brasil_ano[
  ,
  variacao_pct_crianca_ano_vs_ano_anterior :=
    (crianca_ano_risco / data.table::shift(crianca_ano_risco) - 1) * 100
]

cat("\nDenominador anual - Brasil - Geral <5 anos:\n")
print(den_total_brasil_ano)

den_faixas_brasil_ano <- base_anual[
  faixa_etaria != "Geral <5 anos",
  .(
    linhas = .N,
    municipios = data.table::uniqueN(code),
    n_denominador_na = sum(is.na(crianca_ano_risco)),
    n_denominador_zero = sum(crianca_ano_risco == 0, na.rm = TRUE),
    n_denominador_negativo = sum(crianca_ano_risco < 0, na.rm = TRUE),
    denominador_soma_meses = sum(denominador_soma_meses, na.rm = TRUE),
    crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE),
    obitos = sum(obitos, na.rm = TRUE),
    taxa_1000_crianca_ano = sum(obitos, na.rm = TRUE) /
      sum(crianca_ano_risco, na.rm = TRUE) * 1000
  ),
  by = .(ano, faixa_etaria)
][order(faixa_etaria, ano)]

den_faixas_brasil_ano[
  ,
  variacao_pct_crianca_ano_vs_ano_anterior :=
    (crianca_ano_risco / data.table::shift(crianca_ano_risco) - 1) * 100,
  by = faixa_etaria
]

cat("\nDenominador anual - Brasil - por faixa etaria:\n")
print(den_faixas_brasil_ano)

total_check <- base_anual[
  faixa_etaria == "Geral <5 anos",
  .(
    denominador_total = sum(denominador_soma_meses, na.rm = TRUE),
    crianca_ano_total = sum(crianca_ano_risco, na.rm = TRUE),
    obitos_total = sum(obitos, na.rm = TRUE)
  ),
  by = ano
]

soma_faixas_check <- base_anual[
  faixa_etaria != "Geral <5 anos",
  .(
    denominador_soma_faixas = sum(denominador_soma_meses, na.rm = TRUE),
    crianca_ano_soma_faixas = sum(crianca_ano_risco, na.rm = TRUE),
    obitos_soma_faixas = sum(obitos, na.rm = TRUE)
  ),
  by = ano
]

comparacao_total_faixas <- merge(
  total_check,
  soma_faixas_check,
  by = "ano",
  all = TRUE
)

comparacao_total_faixas[
  ,
  `:=`(
    diferenca_denominador = denominador_total - denominador_soma_faixas,
    diferenca_denominador_pct = 100 * (denominador_total - denominador_soma_faixas) / denominador_total,
    diferenca_crianca_ano = crianca_ano_total - crianca_ano_soma_faixas,
    diferenca_crianca_ano_pct = 100 * (crianca_ano_total - crianca_ano_soma_faixas) / crianca_ano_total,
    diferenca_obitos = obitos_total - obitos_soma_faixas,
    diferenca_obitos_pct = 100 * (obitos_total - obitos_soma_faixas) / obitos_total
  )
]

data.table::setorder(comparacao_total_faixas, ano)

cat("\nComparacao: Geral <5 anos versus soma das faixas desagregadas:\n")
print(comparacao_total_faixas)

resumo_completude_contexto <- data.table::rbindlist(
  lapply(
    intersect(
      c(
        "aps_proporcao_cobertura_estrategia_saude_familia",
        "total_beneficios_bf",
        "bf_beneficios_por_1000_crianca",
        "total_prof_ativos",
        "mais_medicos_prof_ativos",
        "mais_medicos_por_10000_crianca",
        "cobertura_urbana_agua_pct",
        "cobertura_urbana_esgoto_pct",
        "cobertura_bcg",
        "cobertura_penta",
        "cobertura_pneumococica",
        "cobertura_rotavirus",
        "n_favelas",
        "favelas_por_10000_crianca"
      ),
      names(base_anual)
    ),
    function(v) {
      data.table::data.table(
        variavel = v,
        n_na = sum(is.na(base_anual[[v]])),
        pct_na = round(mean(is.na(base_anual[[v]])) * 100, 2),
        n_unicos = data.table::uniqueN(base_anual[[v]], na.rm = TRUE),
        minimo = suppressWarnings(min(base_anual[[v]], na.rm = TRUE)),
        maximo = suppressWarnings(max(base_anual[[v]], na.rm = TRUE))
      )
    }
  ),
  fill = TRUE
)

data.table::fwrite(
  den_total_brasil_ano,
  file.path(dir_saida, "tabelas", "denominador_brasil_geral_menor5_ano.csv")
)

data.table::fwrite(
  den_faixas_brasil_ano,
  file.path(dir_saida, "tabelas", "denominador_brasil_por_faixa_ano.csv")
)

data.table::fwrite(
  comparacao_total_faixas,
  file.path(dir_saida, "tabelas", "comparacao_geral_menor5_vs_soma_faixas.csv")
)

data.table::fwrite(
  resumo_completude_contexto,
  file.path(dir_saida, "tabelas", "completude_variaveis_contextuais.csv")
)

p_den_total <- ggplot2::ggplot(
  den_total_brasil_ano,
  ggplot2::aes(x = ano, y = crianca_ano_risco)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    x = "Year",
    y = "Children-years at risk",
    title = "Annual denominator - Brazil",
    subtitle = "General under-5 stratum"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(dir_saida, "figuras", "denominador_brasil_geral_menor5.png"),
  p_den_total,
  width = 8.5,
  height = 4.8,
  dpi = 320
)

p_taxa_total <- ggplot2::ggplot(
  den_total_brasil_ano,
  ggplot2::aes(x = ano, y = taxa_1000_crianca_ano)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  ggplot2::labs(
    x = "Year",
    y = "Deaths per 1,000 child-years",
    title = "Annual mortality rate - Brazil",
    subtitle = "General under-5 stratum"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(dir_saida, "figuras", "taxa_brasil_geral_menor5.png"),
  p_taxa_total,
  width = 8.5,
  height = 4.8,
  dpi = 320
)

p_den_faixas <- ggplot2::ggplot(
  den_faixas_brasil_ano,
  ggplot2::aes(x = ano, y = crianca_ano_risco, group = faixa_etaria)
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::facet_wrap(~ faixa_etaria, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    x = "Year",
    y = "Children-years at risk",
    title = "Annual denominator by age stratum - Brazil"
  ) +
  ggplot2::theme_minimal(base_size = 10)

ggplot2::ggsave(
  file.path(dir_saida, "figuras", "denominador_brasil_por_faixa.png"),
  p_den_faixas,
  width = 11,
  height = 7,
  dpi = 320
)

diagnostico_denominador_base_anual <- list(
  base_anual = base_anual,
  duplicadas = duplicadas,
  den_total_brasil_ano = den_total_brasil_ano,
  den_faixas_brasil_ano = den_faixas_brasil_ano,
  comparacao_total_faixas = comparacao_total_faixas,
  resumo_completude_contexto = resumo_completude_contexto
)

saveRDS(
  diagnostico_denominador_base_anual,
  file.path(dir_saida, "diagnostico_denominador_base_anual.rds")
)

cat("\nConcluido.\n")
cat("Objeto criado: diagnostico_denominador_base_anual\n")
cat("Saidas em: ", dir_saida, "\n", sep = "")
