
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
# 02 - FOREST PLOTS CIENTIFICOS
# Origem: analise_marcos
# Adaptacao: usa diretamente a base anual do novo join.
#
# Diferenca deliberada em relacao ao script antigo:
#   Com spline temporal, coeficientes isolados de interacao nao sao
#   interpretaveis como periodos cronologicos. Por isso o forest plot
#   abaixo estima contrastes preditos Medio vs Baixo e Alto vs Baixo
#   em anos sentinela: primeiro, mediano e ultimo ano do modelo.
# ============================================================

instalar_carregar(c(
  "data.table", "ggplot2", "MASS", "sandwich", "splines", "scales"
))

dir_saida <- file.path(base_dir, "resultados_base_anual_forestplots")
dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "forestplots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_saida, "tabelas"), showWarnings = FALSE, recursive = TRUE)

min_observacoes_modelo <- 100
min_municipios_modelo <- 30
min_anos_modelo <- 6
df_spline_temporal <- 3
levels_exposicao <- c("Baixo", "Médio", "Alto")

base_anual <- carregar_base_anual(base_dir)

# Mantem apenas anos ate 2021.
base_anual <- base_anual[ano <= ANO_FINAL]

if (nrow(base_anual) == 0) {
  stop("A base anual ficou vazia apos aplicar o recorte ate ", ANO_FINAL, ".")
}

cat("\nRecorte temporal aplicado: ate ", ANO_FINAL, ".\n", sep = "")
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
  tryCatch(
    sandwich::vcovCL(mod, cluster = cluster, type = "HC1"),
    error = function(e) stats::vcov(mod)
  )
}

criar_formula_modelo <- function(tipo_modelo, covariaveis_validas) {
  base_rhs <- "exposicao_cat * splines::ns(ano, df = df_spline_temporal)"

  if (tipo_modelo == "ajustada" && length(covariaveis_validas) > 0) {
    rhs <- paste(
      c(base_rhs, "regiao", paste0("z__", covariaveis_validas)),
      collapse = " + "
    )
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

matriz_modelo_alinhada <- function(mod, nd, beta_names) {
  X <- stats::model.matrix(stats::delete.response(stats::terms(mod)), data = nd)

  faltantes <- setdiff(beta_names, colnames(X))
  if (length(faltantes) > 0) {
    X2 <- matrix(0, nrow = nrow(X), ncol = length(faltantes))
    colnames(X2) <- faltantes
    X <- cbind(X, X2)
  }

  X[, beta_names, drop = FALSE]
}

contraste_predito <- function(mod, vc, d, ano_i, exposicao_a, exposicao_ref, covariaveis_validas) {
  beta <- stats::coef(mod)
  beta_names <- names(beta)

  regiao_ref <- names(sort(table(d$regiao), decreasing = TRUE))[1]
  if (is.na(regiao_ref) || length(regiao_ref) == 0) regiao_ref <- levels(d$regiao)[1]

  nd_a <- data.table::data.table(
    ano = ano_i,
    exposicao_cat = factor(exposicao_a, levels = levels_exposicao),
    crianca_ano_risco = 1,
    regiao = factor(regiao_ref, levels = levels(d$regiao))
  )

  nd_b <- data.table::copy(nd_a)
  nd_b[, exposicao_cat := factor(exposicao_ref, levels = levels_exposicao)]

  if (length(covariaveis_validas) > 0) {
    for (v in covariaveis_validas) {
      nd_a[, (paste0("z__", v)) := 0]
      nd_b[, (paste0("z__", v)) := 0]
    }
  }

  X_a <- matriz_modelo_alinhada(mod, nd_a, beta_names)
  X_b <- matriz_modelo_alinhada(mod, nd_b, beta_names)

  L <- X_a - X_b
  log_rr <- as.numeric(L %*% beta)
  se <- tryCatch(
    sqrt(as.numeric(L %*% vc[beta_names, beta_names, drop = FALSE] %*% t(L))),
    error = function(e) NA_real_
  )

  if (!is.finite(se)) {
    return(
      data.table::data.table(
        ano_contraste = ano_i,
        contraste = paste0(exposicao_a, " vs ", exposicao_ref),
        log_IRR = log_rr,
        erro_padrao = NA_real_,
        IRR = exp(log_rr),
        IC95_inf = NA_real_,
        IC95_sup = NA_real_,
        p_valor = NA_real_
      )
    )
  }

  data.table::data.table(
    ano_contraste = ano_i,
    contraste = paste0(exposicao_a, " vs ", exposicao_ref),
    log_IRR = log_rr,
    erro_padrao = se,
    IRR = exp(log_rr),
    IC95_inf = exp(log_rr - 1.96 * se),
    IC95_sup = exp(log_rr + 1.96 * se),
    p_valor = 2 * stats::pnorm(abs(log_rr / se), lower.tail = FALSE)
  )
}

ajustar_e_contrastar <- function(base, faixa_i, politica_i, tipo_modelo) {
  prep <- preparar_dados_modelo(base, faixa_i, politica_i, tipo_modelo)

  if (is.null(prep)) {
    return(
      list(
        status = "ignorado_sem_dados",
        tabela = NULL,
        log = data.table::data.table(
          faixa_etaria = faixa_i,
          politica = politica_i,
          politica_label = rotulo_politica(politica_i),
          tipo_modelo = tipo_modelo,
          status = "ignorado_sem_dados",
          erro = NA_character_
        )
      )
    )
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
        tabela = NULL,
        log = data.table::data.table(
          faixa_etaria = faixa_i,
          politica = politica_i,
          politica_label = rotulo_politica(politica_i),
          tipo_modelo = tipo_modelo,
          status = "ignorado_criterios_minimos",
          erro = NA_character_
        )
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
        tabela = NULL,
        log = data.table::data.table(
          faixa_etaria = faixa_i,
          politica = politica_i,
          politica_label = rotulo_politica(politica_i),
          tipo_modelo = tipo_modelo,
          status = "falhou_modelo",
          erro = conditionMessage(mod)
        )
      )
    )
  }

  vc <- vcov_cluster(mod, d$code)
  anos <- sort(unique(d$ano))
  anos_sentinela <- unique(c(min(anos), round(stats::median(anos)), max(anos)))

  tabs <- list()
  k <- 0

  for (aa in anos_sentinela) {
    for (contr in c("Médio", "Alto")) {
      k <- k + 1
      tabs[[k]] <- contraste_predito(
        mod = mod,
        vc = vc,
        d = d,
        ano_i = aa,
        exposicao_a = contr,
        exposicao_ref = "Baixo",
        covariaveis_validas = covars
      )
    }
  }

  tab <- data.table::rbindlist(tabs, fill = TRUE)
  tab[
    ,
    `:=`(
      faixa_etaria = faixa_i,
      politica = politica_i,
      politica_label = rotulo_politica(politica_i),
      tipo_modelo = tipo_modelo,
      n = resumo$n,
      municipios = resumo$municipios,
      anos_modelo = resumo$anos,
      obitos = resumo$obitos,
      crianca_ano_risco = resumo$crianca_ano_risco,
      covariaveis_ajuste = paste(covars, collapse = "; ")
    )
  ]

  data.table::setcolorder(
    tab,
    c(
      "faixa_etaria", "politica", "politica_label", "tipo_modelo",
      "ano_contraste", "contraste",
      "IRR", "IC95_inf", "IC95_sup", "p_valor",
      "n", "municipios", "anos_modelo", "obitos", "crianca_ano_risco",
      "covariaveis_ajuste"
    )
  )

  list(
    status = "ok",
    tabela = tab,
    log = data.table::data.table(
      faixa_etaria = faixa_i,
      politica = politica_i,
      politica_label = rotulo_politica(politica_i),
      tipo_modelo = tipo_modelo,
      status = "ok",
      erro = NA_character_
    )
  )
}

resultados <- list()
logs <- list()
contador <- 0

for (faixa_i in faixas_analise) {
  for (politica_i in politicas) {
    for (tipo_i in c("bruta", "ajustada")) {
      contador <- contador + 1
      cat(
        "\nForest modelo ", contador,
        " | faixa = ", faixa_i,
        " | politica = ", politica_i,
        " | tipo = ", tipo_i,
        "\n",
        sep = ""
      )

      res <- ajustar_e_contrastar(base_anual, faixa_i, politica_i, tipo_i)
      resultados[[contador]] <- res$tabela
      logs[[contador]] <- res$log
    }
  }
}

forest_tabela <- data.table::rbindlist(resultados, fill = TRUE)
log_forest <- data.table::rbindlist(logs, fill = TRUE)

if (nrow(forest_tabela) > 0) {
  forest_tabela[
    ,
    `:=`(
      IRR_IC95 = paste0(
        formatar_num(IRR, 2), " (",
        formatar_num(IC95_inf, 2), " - ",
        formatar_num(IC95_sup, 2), ")"
      ),
      p_formatado = formatar_p(p_valor)
    )
  ]
}

data.table::fwrite(forest_tabela, file.path(dir_saida, "tabelas", "forest_contrastes_preditos.csv"))
data.table::fwrite(log_forest, file.path(dir_saida, "tabelas", "log_forest_modelos.csv"))

plotar_forest <- function(dt, faixa_i, tipo_i) {
  d <- dt[faixa_etaria == faixa_i & tipo_modelo == tipo_i]
  if (nrow(d) == 0) return(NA_character_)

  d[
    ,
    label_linha := paste0(
      politica_label, " | ", ano_contraste, " | ", contraste
    )
  ]

  d[, label_linha := factor(label_linha, levels = rev(unique(label_linha)))]

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = IRR, y = label_linha)
  ) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = IC95_inf, xmax = IC95_sup),
      height = 0.18,
      linewidth = 0.45,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 2.0, shape = 15, na.rm = TRUE) +
    ggplot2::scale_x_log10(
      breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::labs(
      x = "Incidence rate ratio (log scale)",
      y = NULL,
      title = paste0("Predicted contrasts by annual tertile - ", faixa_i),
      subtitle = paste0("Model: ", tipo_i, ". Reference: low annual tertile.")
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 7)
    )

  arq <- file.path(
    dir_saida,
    "forestplots",
    paste0("forest_", nome_arquivo_seguro(tipo_i), "_", nome_arquivo_seguro(faixa_i), ".png")
  )

  ggplot2::ggsave(arq, p, width = 12, height = max(6, 0.23 * nrow(d)), dpi = 320)
  arq
}

arquivos <- character(0)
if (nrow(forest_tabela) > 0) {
  for (ff in unique(forest_tabela$faixa_etaria)) {
    for (tt in unique(forest_tabela$tipo_modelo)) {
      arquivos <- c(arquivos, plotar_forest(forest_tabela, ff, tt))
    }
  }
}

forest_results_base_anual <- list(
  base_anual = base_anual,
  politicas = politicas,
  forest_tabela = forest_tabela,
  log_forest = log_forest,
  arquivos_forest = arquivos
)

saveRDS(forest_results_base_anual, file.path(dir_saida, "forest_results_base_anual.rds"))

cat("\nConcluido.\n")
cat("Objeto criado: forest_results_base_anual\n")
cat("Saidas em: ", dir_saida, "\n", sep = "")
