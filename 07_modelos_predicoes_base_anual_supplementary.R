
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
# 01 - MODELOS COM PREDICOES TEMPORAIS
# Origem: analise_alt / analise_alt2 / analise_alt3
# Adaptacao: usa diretamente a base anual do novo join.
# ============================================================

instalar_carregar(c(
  "data.table", "ggplot2", "MASS", "sandwich", "splines", "scales"
))

dir_saida <- file.path(base_dir, "resultados_base_anual_modelos_predicoes")
dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "figuras_taxa_predita"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "figuras_variacao_pct"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "tabelas"), showWarnings = FALSE, recursive = TRUE)

min_observacoes_modelo <- 100
min_municipios_modelo <- 30
min_anos_modelo <- 6
df_spline_temporal <- 3
multiplicador_taxa <- 1000
levels_exposicao <- c("Baixo", "Médio", "Alto")

base_anual <- carregar_base_anual(base_dir)

# Mantem apenas anos ate 2021.
base_anual <- base_anual[ano <= ANO_FINAL]

if (nrow(base_anual) == 0) {
  stop("A base anual ficou vazia apos aplicar o recorte ate ", ANO_FINAL, ".")
}

cat("\nRecorte temporal aplicado: ate ", ANO_FINAL, ".\n", sep = "")

cat("\nBase anual carregada:\n")
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
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
    )
  ]
)

politicas <- politicas_disponiveis(base_anual)

if (length(politicas) == 0) {
  stop("Nenhuma politica/indicador com dados suficientes foi encontrada na base anual.")
}

faixas_analise <- c(
  "Geral <5 anos",
  sort(setdiff(unique(base_anual$faixa_etaria), "Geral <5 anos"))
)

covariaveis_candidatas <- intersect(
  c(
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
  ),
  names(base_anual)
)

vcov_cluster <- function(mod, cluster) {
  vc <- tryCatch(
    sandwich::vcovCL(mod, cluster = cluster, type = "HC1"),
    error = function(e) {
      warning("Falha no vcovCL; usando vcov padrao. Motivo: ", conditionMessage(e))
      stats::vcov(mod)
    }
  )
  vc
}

wald_global <- function(beta, vc, padrao) {
  idx <- grep(padrao, names(beta))
  if (length(idx) == 0) return(NA_real_)

  L <- diag(length(beta))[idx, , drop = FALSE]
  est <- as.numeric(L %*% beta)
  V <- L %*% vc %*% t(L)

  if (any(!is.finite(est)) || any(!is.finite(V))) return(NA_real_)

  rk <- qr(V)$rank
  if (rk < 1) return(NA_real_)

  stat <- tryCatch(
    as.numeric(t(est) %*% MASS::ginv(V) %*% est),
    error = function(e) NA_real_
  )

  if (!is.finite(stat)) return(NA_real_)
  stats::pchisq(stat, df = rk, lower.tail = FALSE)
}

extrair_coeficientes <- function(mod, vc, meta) {
  beta <- stats::coef(mod)
  se <- sqrt(diag(vc))
  termos <- names(beta)

  data.table::data.table(
    faixa_etaria = meta$faixa_etaria,
    politica = meta$politica,
    politica_label = rotulo_politica(meta$politica),
    tipo_modelo = meta$tipo_modelo,
    termo = termos,
    estimativa = as.numeric(beta),
    erro_padrao_robusto = as.numeric(se[termos]),
    IRR = exp(as.numeric(beta)),
    IC95_inf = exp(as.numeric(beta) - 1.96 * as.numeric(se[termos])),
    IC95_sup = exp(as.numeric(beta) + 1.96 * as.numeric(se[termos])),
    p_valor = 2 * stats::pnorm(abs(as.numeric(beta) / as.numeric(se[termos])), lower.tail = FALSE)
  )
}

criar_formula_modelo <- function(tipo_modelo, covariaveis_validas) {
  base_rhs <- "exposicao_cat * splines::ns(ano, df = df_spline_temporal)"

  if (tipo_modelo == "ajustada" && length(covariaveis_validas) > 0) {
    z_covars <- paste0("z__", covariaveis_validas)
    rhs <- paste(c(base_rhs, "regiao", z_covars), collapse = " + ")
  } else if (tipo_modelo == "ajustada") {
    rhs <- paste(c(base_rhs, "regiao"), collapse = " + ")
  } else {
    rhs <- base_rhs
  }

  stats::as.formula(
    paste0("obitos ~ ", rhs, " + offset(log(crianca_ano_risco))")
  )
}

preparar_dados_modelo <- function(base, faixa_i, politica_i, tipo_modelo) {
  d <- base[faixa_etaria == faixa_i]
  d <- criar_tercil_anual(d, politica_i, "exposicao_cat")

  d <- d[
    !is.na(obitos) &
      !is.na(crianca_ano_risco) &
      crianca_ano_risco > 0 &
      !is.na(ano) &
      !is.na(code) &
      !is.na(exposicao_cat)
  ]

  if (nrow(d) == 0) return(NULL)

  covars <- setdiff(covariaveis_candidatas, politica_i)

  covars_validas <- covars[
    vapply(covars, function(v) {
      x <- d[[v]]
      mean(!is.na(x)) >= 0.70 && data.table::uniqueN(x, na.rm = TRUE) >= 3
    }, logical(1))
  ]

  if (tipo_modelo == "ajustada" && length(covars_validas) > 0) {
    for (v in covars_validas) {
      z <- paste0("z__", v)
      m <- mean(d[[v]], na.rm = TRUE)
      s <- stats::sd(d[[v]], na.rm = TRUE)
      if (!is.finite(s) || s == 0) {
        d[, (z) := 0]
      } else {
        d[, (z) := (get(v) - m) / s]
        d[is.na(get(z)), (z) := 0]
      }
    }
  } else {
    covars_validas <- character(0)
  }

  d[, regiao := droplevels(regiao)]

  list(dados = d, covariaveis = covars_validas)
}

predizer_taxas <- function(mod, vc, d, meta, covariaveis_validas) {
  anos <- sort(unique(d$ano))
  out <- list()
  k <- 0

  regiao_ref <- names(sort(table(d$regiao), decreasing = TRUE))[1]
  if (is.na(regiao_ref) || length(regiao_ref) == 0) regiao_ref <- levels(d$regiao)[1]

  for (aa in anos) {
    for (lev in levels_exposicao) {
      nd <- data.table::data.table(
        ano = aa,
        exposicao_cat = factor(lev, levels = levels_exposicao),
        crianca_ano_risco = 1,
        regiao = factor(regiao_ref, levels = levels(d$regiao))
      )

      if (length(covariaveis_validas) > 0) {
        for (v in covariaveis_validas) {
          nd[, (paste0("z__", v)) := 0]
        }
      }

      pr <- tryCatch(
        stats::predict(mod, newdata = nd, type = "link", se.fit = TRUE),
        error = function(e) NULL
      )

      if (is.null(pr)) next

      taxa <- exp(as.numeric(pr$fit)) * multiplicador_taxa
      ic_inf <- exp(as.numeric(pr$fit) - 1.96 * as.numeric(pr$se.fit)) * multiplicador_taxa
      ic_sup <- exp(as.numeric(pr$fit) + 1.96 * as.numeric(pr$se.fit)) * multiplicador_taxa

      k <- k + 1
      out[[k]] <- data.table::data.table(
        faixa_etaria = meta$faixa_etaria,
        politica = meta$politica,
        politica_label = rotulo_politica(meta$politica),
        tipo_modelo = meta$tipo_modelo,
        ano = aa,
        exposicao_cat = lev,
        taxa_predita_1000 = taxa,
        IC95_inf = ic_inf,
        IC95_sup = ic_sup
      )
    }
  }

  pred <- data.table::rbindlist(out, fill = TRUE)

  if (nrow(pred) > 0) {
    pred[
      ,
      taxa_base := taxa_predita_1000[ano == min(ano, na.rm = TRUE)][1],
      by = .(faixa_etaria, politica, tipo_modelo, exposicao_cat)
    ]
    pred[
      ,
      `:=`(
        variacao_pct_acumulada = (taxa_predita_1000 / taxa_base - 1) * 100,
        reducao_pct_acumulada = (1 - taxa_predita_1000 / taxa_base) * 100
      )
    ]
  }

  pred
}

ajustar_um_modelo <- function(base, faixa_i, politica_i, tipo_modelo) {
  prep <- preparar_dados_modelo(base, faixa_i, politica_i, tipo_modelo)

  meta <- list(
    faixa_etaria = faixa_i,
    politica = politica_i,
    tipo_modelo = tipo_modelo
  )

  if (is.null(prep)) {
    return(list(status = "ignorado_sem_dados", meta = meta))
  }

  d <- prep$dados
  covars <- prep$covariaveis

  resumo <- d[
    ,
    .(
      n = .N,
      municipios = data.table::uniqueN(code),
      anos = data.table::uniqueN(ano),
      obitos = sum(obitos, na.rm = TRUE),
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE),
      grupos_exposicao = data.table::uniqueN(exposicao_cat)
    )
  ]

  if (
    resumo$n < min_observacoes_modelo ||
      resumo$municipios < min_municipios_modelo ||
      resumo$anos < min_anos_modelo ||
      resumo$grupos_exposicao < 3
  ) {
    return(
      list(
        status = "ignorado_criterios_minimos",
        meta = meta,
        resumo = resumo,
        covariaveis = covars
      )
    )
  }

  f <- criar_formula_modelo(tipo_modelo, covars)

  mod <- tryCatch(
    MASS::glm.nb(f, data = d, control = glm.control(maxit = 100)),
    error = function(e) e
  )

  if (inherits(mod, "error")) {
    return(
      list(
        status = "falhou_modelo",
        meta = meta,
        erro = conditionMessage(mod),
        resumo = resumo,
        covariaveis = covars
      )
    )
  }

  vc <- vcov_cluster(mod, d$code)
  beta <- stats::coef(mod)

  p_interacao_tempo <- wald_global(
    beta = beta,
    vc = vc,
    padrao = "exposicao_cat.*:splines::ns|splines::ns.*:exposicao_cat"
  )

  coef <- extrair_coeficientes(mod, vc, meta)

  pred <- predizer_taxas(
    mod = mod,
    vc = vc,
    d = d,
    meta = meta,
    covariaveis_validas = covars
  )

  resumo_modelo <- data.table::data.table(
    faixa_etaria = faixa_i,
    politica = politica_i,
    politica_label = rotulo_politica(politica_i),
    tipo_modelo = tipo_modelo,
    n = resumo$n,
    municipios = resumo$municipios,
    anos = resumo$anos,
    ano_min = min(d$ano, na.rm = TRUE),
    ano_max = max(d$ano, na.rm = TRUE),
    obitos = resumo$obitos,
    crianca_ano_risco = resumo$crianca_ano_risco,
    theta = mod$theta,
    AIC = stats::AIC(mod),
    p_interacao_exposicao_tempo = p_interacao_tempo,
    covariaveis_ajuste = paste(covars, collapse = "; ")
  )

  list(
    status = "ok",
    meta = meta,
    modelo = mod,
    vcov = vc,
    dados = d,
    coeficientes = coef,
    predicoes = pred,
    resumo_modelo = resumo_modelo,
    covariaveis = covars
  )
}

lista_resultados <- list()
log_modelos <- list()
contador <- 0
contador_log <- 0

for (faixa_i in faixas_analise) {
  for (politica_i in politicas) {
    for (tipo_i in c("bruta", "ajustada")) {
      contador <- contador + 1

      cat(
        "\nModelo ", contador,
        " | faixa = ", faixa_i,
        " | politica = ", politica_i,
        " | tipo = ", tipo_i,
        "\n",
        sep = ""
      )

      res <- ajustar_um_modelo(base_anual, faixa_i, politica_i, tipo_i)
      lista_resultados[[contador]] <- res

      contador_log <- contador_log + 1
      log_modelos[[contador_log]] <- data.table::data.table(
        faixa_etaria = faixa_i,
        politica = politica_i,
        politica_label = rotulo_politica(politica_i),
        tipo_modelo = tipo_i,
        status = res$status,
        erro = if (!is.null(res$erro)) res$erro else NA_character_
      )
    }
  }
}

coeficientes_modelos <- data.table::rbindlist(
  lapply(lista_resultados, function(x) if (identical(x$status, "ok")) x$coeficientes else NULL),
  fill = TRUE
)

predicoes_taxas <- data.table::rbindlist(
  lapply(lista_resultados, function(x) if (identical(x$status, "ok")) x$predicoes else NULL),
  fill = TRUE
)

resumo_modelos <- data.table::rbindlist(
  lapply(lista_resultados, function(x) if (identical(x$status, "ok")) x$resumo_modelo else NULL),
  fill = TRUE
)

log_modelos <- data.table::rbindlist(log_modelos, fill = TRUE)

data.table::fwrite(coeficientes_modelos, file.path(dir_saida, "tabelas", "coeficientes_modelos_nb_robustos.csv"))
data.table::fwrite(predicoes_taxas, file.path(dir_saida, "tabelas", "predicoes_taxas_tercis.csv"))
data.table::fwrite(resumo_modelos, file.path(dir_saida, "tabelas", "resumo_modelos.csv"))
data.table::fwrite(log_modelos, file.path(dir_saida, "tabelas", "log_modelos.csv"))

plotar_predicoes <- function(dt, yvar, ylab, subdir) {
  if (nrow(dt) == 0) return(character(0))

  arquivos <- character(0)
  combos <- unique(dt[, .(faixa_etaria, politica, politica_label, tipo_modelo)])

  for (i in seq_len(nrow(combos))) {
    cc <- combos[i]
    d <- dt[
      faixa_etaria == cc$faixa_etaria &
        politica == cc$politica &
        tipo_modelo == cc$tipo_modelo
    ]

    if (nrow(d) == 0) next

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(
        x = ano,
        y = .data[[yvar]],
        group = exposicao_cat,
        linetype = exposicao_cat,
        shape = exposicao_cat
      )
    ) +
      ggplot2::geom_hline(
        yintercept = if (yvar == "variacao_pct_acumulada") 0 else NA_real_,
        linewidth = 0.3,
        na.rm = TRUE
      ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::labs(
        x = "Year",
        y = ylab,
        linetype = "Annual tertile",
        shape = "Annual tertile",
        title = cc$politica_label,
        subtitle = paste(cc$faixa_etaria, "|", cc$tipo_modelo)
      ) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position = "top",
        panel.grid.minor = ggplot2::element_blank()
      )

    arq <- file.path(
      dir_saida,
      subdir,
      paste0(
        nome_arquivo_seguro(cc$tipo_modelo), "_",
        nome_arquivo_seguro(cc$faixa_etaria), "_",
        nome_arquivo_seguro(cc$politica),
        ".png"
      )
    )

    ggplot2::ggsave(arq, p, width = 9, height = 5.2, dpi = 320)
    arquivos <- c(arquivos, arq)
  }

  arquivos
}

arquivos_taxa <- plotar_predicoes(
  predicoes_taxas,
  yvar = "taxa_predita_1000",
  ylab = "Predicted mortality rate per 1,000 child-years",
  subdir = "figuras_taxa_predita"
)

arquivos_variacao <- plotar_predicoes(
  predicoes_taxas,
  yvar = "variacao_pct_acumulada",
  ylab = "Cumulative percent change from first modeled year",
  subdir = "figuras_variacao_pct"
)

novo_results_base_anual <- list(
  base_anual_modelo = base_anual,
  politicas = politicas,
  coeficientes_modelos = coeficientes_modelos,
  predicoes_taxas = predicoes_taxas,
  resumo_modelos = resumo_modelos,
  log_modelos = log_modelos,
  arquivos_graficos_taxa = arquivos_taxa,
  arquivos_graficos_variacao = arquivos_variacao
)

saveRDS(novo_results_base_anual, file.path(dir_saida, "novo_results_base_anual.rds"))

cat("\nConcluido.\n")
cat("Objeto criado: novo_results_base_anual\n")
cat("Saidas em: ", dir_saida, "\n", sep = "")
