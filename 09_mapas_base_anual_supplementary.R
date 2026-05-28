
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
# 03 - MAPAS ANUAIS
# Origem: mapas.R
# Adaptacao: usa diretamente a base anual do novo join.
# Nao agrega municipio-mes; a unidade de entrada ja e municipio x ano x faixa_etaria.
# ============================================================

instalar_carregar(c(
  "data.table", "dplyr", "sf", "geobr", "ggplot2", "viridis", "scales"
))

ANO_FINAL <- 2021
P_LIMITE_SUPERIOR <- 0.995

dir_saida <- file.path(base_dir, "resultados_base_anual_mapas")
out_pop <- file.path(dir_saida, "mapas_crianca_ano_risco")
out_taxa <- file.path(dir_saida, "mapas_taxa_mortalidade")
out_diag <- file.path(dir_saida, "diagnosticos")

dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
dir.create(out_pop, showWarnings = FALSE, recursive = TRUE)
dir.create(out_taxa, showWarnings = FALSE, recursive = TRUE)
dir.create(out_diag, showWarnings = FALSE, recursive = TRUE)

base_anual <- carregar_base_anual(base_dir)
base_anual <- base_anual[ano <= ANO_FINAL]

faixas_mapas <- c(
  "Geral <5 anos",
  sort(setdiff(unique(base_anual$faixa_etaria), "Geral <5 anos"))
)

cat("\nResumo da base anual para mapas:\n")
print(
  base_anual[
    ,
    .(
      linhas = .N,
      municipios = data.table::uniqueN(code),
      ano_min = min(ano, na.rm = TRUE),
      ano_max = max(ano, na.rm = TRUE),
      faixas = data.table::uniqueN(faixa_etaria),
      obitos = sum(obitos, na.rm = TRUE),
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
    )
  ]
)

ler_malha_municipal_robusta <- function() {
  tentativas <- list(
    list(year = 2020, simplified = TRUE),
    list(year = 2020, simplified = FALSE),
    list(year = 2022, simplified = TRUE),
    list(year = 2022, simplified = FALSE),
    list(year = 2010, simplified = TRUE),
    list(year = 2010, simplified = FALSE)
  )

  for (tt in tentativas) {
    cat(
      "\nTentando geobr::read_municipality(year = ",
      tt$year,
      ", simplified = ",
      tt$simplified,
      ")\n",
      sep = ""
    )

    obj <- tryCatch(
      geobr::read_municipality(
        code_muni = "all",
        year = tt$year,
        simplified = tt$simplified,
        showProgress = FALSE
      ),
      error = function(e) {
        message("Falhou: ", conditionMessage(e))
        NULL
      },
      warning = function(w) {
        message("Aviso: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    if (!is.null(obj) && nrow(obj) > 0) return(sf::st_as_sf(obj))
  }

  stop("Nao foi possivel carregar a malha municipal pelo geobr.")
}

muni_raw <- ler_malha_municipal_robusta()
muni_raw <- sf::st_make_valid(muni_raw)

muni <- muni_raw |>
  dplyr::mutate(code = substr(as.character(code_muni), 1, 6)) |>
  dplyr::select(code)

dup_codes <- muni |>
  sf::st_drop_geometry() |>
  dplyr::count(code) |>
  dplyr::filter(n > 1)

if (nrow(dup_codes) > 0) {
  muni <- muni |>
    dplyr::group_by(code) |>
    dplyr::summarise(.groups = "drop")
}

muni <- sf::st_make_valid(muni)

codigos_mapa <- unique(muni$code)
codigos_dados <- unique(base_anual$code)

codigos_sem_mapa <- setdiff(codigos_dados, codigos_mapa)
codigos_sem_dados <- setdiff(codigos_mapa, codigos_dados)

data.table::fwrite(
  data.table::data.table(code = codigos_sem_mapa),
  file.path(out_diag, "codigos_dados_sem_malha.csv")
)

data.table::fwrite(
  data.table::data.table(code = codigos_sem_dados),
  file.path(out_diag, "codigos_malha_sem_dados.csv")
)

cat("\nCodigos nos dados sem malha: ", length(codigos_sem_mapa), "\n", sep = "")
cat("Codigos na malha sem dados: ", length(codigos_sem_dados), "\n", sep = "")

anos_mapa <- sort(unique(base_anual$ano))

grade <- data.table::CJ(
  code = codigos_mapa,
  ano = anos_mapa,
  faixa_etaria = faixas_mapas,
  unique = TRUE
)

base_mapas <- merge(
  grade,
  base_anual[
    ,
    .(
      code,
      ano,
      faixa_etaria,
      obitos,
      crianca_ano_risco,
      taxa_1000_crianca_ano
    )
  ],
  by = c("code", "ano", "faixa_etaria"),
  all.x = TRUE
)

data.table::fwrite(base_mapas, file.path(out_diag, "base_mapas_anual_completa.csv"))

mapa_sf <- muni |>
  dplyr::left_join(as.data.frame(base_mapas), by = "code")

limite_robusto <- function(x, p = P_LIMITE_SUPERIOR) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  lim_sup <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
  if (!is.finite(lim_sup) || lim_sup <= 0) lim_sup <- max(x, na.rm = TRUE)
  c(0, lim_sup)
}

plotar_mapa_faixa <- function(mapa, faixa_i, var, label, outdir, prefixo) {
  d <- mapa |>
    dplyr::filter(faixa_etaria == faixa_i)

  if (nrow(d) == 0) return(NA_character_)

  lim <- limite_robusto(d[[var]])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[var]]),
      color = NA
    ) +
    viridis::scale_fill_viridis(
      option = "C",
      na.value = "grey92",
      limits = lim,
      oob = scales::squish,
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    ggplot2::facet_wrap(~ ano) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = label,
      subtitle = faixa_i,
      fill = NULL
    ) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      plot.title = ggplot2::element_text(face = "bold")
    )

  arq <- file.path(
    outdir,
    paste0(prefixo, "_", nome_arquivo_seguro(faixa_i), ".png")
  )

  ggplot2::ggsave(arq, p, width = 13, height = 9, dpi = 320)
  arq
}

arquivos_pop <- character(0)
arquivos_taxa <- character(0)

for (ff in faixas_mapas) {
  arquivos_pop <- c(
    arquivos_pop,
    plotar_mapa_faixa(
      mapa = mapa_sf,
      faixa_i = ff,
      var = "crianca_ano_risco",
      label = "Children-years at risk",
      outdir = out_pop,
      prefixo = "crianca_ano_risco"
    )
  )

  arquivos_taxa <- c(
    arquivos_taxa,
    plotar_mapa_faixa(
      mapa = mapa_sf,
      faixa_i = ff,
      var = "taxa_1000_crianca_ano",
      label = "Under-5 mortality rate per 1,000 child-years",
      outdir = out_taxa,
      prefixo = "taxa_mortalidade"
    )
  )
}

resumo_nacional <- base_anual[
  ,
  .(
    obitos = sum(obitos, na.rm = TRUE),
    crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE),
    taxa_1000_crianca_ano = sum(obitos, na.rm = TRUE) /
      sum(crianca_ano_risco, na.rm = TRUE) * 1000
  ),
  by = .(ano, faixa_etaria)
][order(faixa_etaria, ano)]

data.table::fwrite(
  resumo_nacional,
  file.path(out_diag, "resumo_nacional_anual_por_faixa.csv")
)

mapas_results_base_anual <- list(
  base_mapas = base_mapas,
  resumo_nacional = resumo_nacional,
  codigos_sem_mapa = codigos_sem_mapa,
  codigos_sem_dados = codigos_sem_dados,
  arquivos_pop = arquivos_pop,
  arquivos_taxa = arquivos_taxa
)

saveRDS(mapas_results_base_anual, file.path(dir_saida, "mapas_results_base_anual.rds"))

cat("\nConcluido.\n")
cat("Objeto criado: mapas_results_base_anual\n")
cat("Saidas em: ", dir_saida, "\n", sep = "")
