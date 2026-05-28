# ============================================================
# JOIN UNIFICADO - MORTALIDADE <5 ANOS + CONTEXTO/POLITICAS
#
# Objetivos em um unico fluxo:
#   1) Ler as bases originais da pasta do projeto.
#   2) Construir a base mensal final:
#        base_final_mortalidade_contexto
#        base_final_mortalidade_contexto_filtrada
#        df
#   3) Construir a base anual leve:
#        base_anual_mortalidade_contexto
#
# Unidades:
#   - Base mensal: municipio x mes x faixa_etaria
#   - Base anual : municipio x ano x faixa_etaria
#
# Regras principais:
#   - Obitos: soma por municipio-tempo-faixa.
#   - Denominador mensal: soma por municipio-mes-faixa.
#   - Denominador anual: soma dos denominadores mensais; crianca_ano_risco = soma_meses / 12.
#   - Politicas/contexto anuais: maior valor observado no ano, replicado para as faixas etarias.
#   - Codigo municipal: sempre padronizado para 6 digitos DATASUS.
#   - "Geral <5 anos" e faixas desagregadas sao mantidas separadas.
#
# Saidas principais:
#   base_final_mortalidade_contexto_com_aps_favelas.rds
#   base_final_mortalidade_contexto.rds
#   base_final_mortalidade_contexto_filtrada.rds
#   base_anual_leve/base_anual_mortalidade_contexto.rds
# ============================================================

options(stringsAsFactors = FALSE)

# ============================================================
# 0. PARAMETROS
# ============================================================

pasta_base <- "C:/Users/MARCOS.ANTUNES/OneDrive - PUCRS - BR/Documentações - Marcos/Área de Trabalho/Mortalidade Infantil/bases"

salvar_csv_mensal <- FALSE
salvar_csv_anual  <- TRUE
salvar_rds        <- TRUE

rotulo_geral_menor5 <- "Geral <5 anos"

faixas_total_menor5 <- c(
  "00-59 meses", "0-59 meses", "00 a 59 meses", "0 a 59 meses",
  "Geral <5 anos", "<5 anos", "Menores de 5 anos"
)

# ============================================================
# 1. PACOTES
# ============================================================

pacotes <- c("data.table", "lubridate", "readxl", "stringr")

faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) install.packages(faltantes)

library(data.table)
library(lubridate)
library(readxl)
library(stringr)

stopifnot(dir.exists(pasta_base))

pasta_auditoria <- file.path(pasta_base, "auditorias_join_unificado")
pasta_anual <- file.path(pasta_base, "base_anual_leve")
dir.create(pasta_auditoria, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_anual, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. FUNCOES AUXILIARES
# ============================================================

pad_code6 <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9]", "", x)
  x[x == ""] <- NA_character_
  
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  
  out[ok & nchar(x) >= 7] <- substr(x[ok & nchar(x) >= 7], 1, 6)
  out[ok & nchar(x) == 6] <- x[ok & nchar(x) == 6]
  out[ok & nchar(x) < 6] <- sprintf("%06d", suppressWarnings(as.integer(x[ok & nchar(x) < 6])))
  
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
  x[tem_virgula] <- gsub(".", "", x[tem_virgula], fixed = TRUE)
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  
  suppressWarnings(as.numeric(x))
}

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  
  x_chr <- as.character(x)
  out <- suppressWarnings(as.Date(x_chr))
  
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::ymd(x_chr))
  }
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::dmy(x_chr))
  }
  if (all(is.na(out))) {
    dig <- gsub("[^0-9]", "", x_chr)
    out <- rep(as.Date(NA), length(x_chr))
    ok6 <- !is.na(dig) & nchar(dig) == 6
    out[ok6] <- as.Date(paste0(substr(dig[ok6], 1, 4), "-", substr(dig[ok6], 5, 6), "-01"))
    ok8 <- !is.na(dig) & nchar(dig) == 8
    out[ok8] <- suppressWarnings(lubridate::ymd(dig[ok8]))
  }
  
  as.Date(out)
}

ano_from_data <- function(x) {
  as.integer(format(as_date_safe(x), "%Y"))
}

mes_from_data <- function(x) {
  as.integer(format(as_date_safe(x), "%m"))
}

primeira_existente <- function(nms, candidatos) {
  out <- candidatos[candidatos %in% nms]
  if (length(out) == 0) return(NA_character_)
  out[1]
}

max_na <- function(x) {
  x <- to_num(x)
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

mean_na <- function(x) {
  x <- to_num(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

sum_na <- function(x) {
  x <- to_num(x)
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

corrigir_nan <- function(dt) {
  setDT(dt)
  vars_num <- names(dt)[vapply(dt, is.numeric, logical(1))]
  for (v in vars_num) dt[is.nan(get(v)), (v) := NA_real_]
  invisible(dt)
}

checar_linhas <- function(base, n_esperado, etapa) {
  if (nrow(base) != n_esperado) {
    stop(
      "Erro no join de ", etapa,
      ": numero de linhas mudou. Esperado: ", n_esperado,
      "; obtido: ", nrow(base), "."
    )
  }
  invisible(TRUE)
}

checar_unicidade <- function(dt, chave, nome) {
  setDT(dt)
  n_dup <- nrow(dt) - uniqueN(dt[, ..chave])
  
  cat("\n", nome, "\n", sep = "")
  cat("Linhas: ", nrow(dt), "\n", sep = "")
  cat("Chave: ", paste(chave, collapse = " + "), "\n", sep = "")
  cat("Duplicadas pela chave: ", n_dup, "\n", sep = "")
  
  if (n_dup > 0) {
    fwrite(
      dt[, .N, by = chave][N > 1],
      file.path(pasta_auditoria, paste0("duplicidades_", gsub("[^A-Za-z0-9]+", "_", nome), ".csv"))
    )
    stop(nome, " tem duplicidade na chave: ", paste(chave, collapse = " + "))
  }
  
  invisible(n_dup)
}

preparar_code_data <- function(dt, code_candidates, exigir_data = TRUE) {
  dt <- as.data.table(dt)
  
  col_code <- primeira_existente(names(dt), code_candidates)
  if (is.na(col_code)) {
    stop("Nao encontrei coluna de codigo municipal. Candidatas: ", paste(code_candidates, collapse = ", "))
  }
  dt[, code := pad_code6(get(col_code))]
  
  if ("data" %in% names(dt)) {
    dt[, data := as_date_safe(data)]
  } else if (all(c("ano", "mes") %in% names(dt))) {
    dt[, data := as.Date(sprintf("%04d-%02d-01", as.integer(to_num(ano)), as.integer(to_num(mes))))]
  } else if ("competencia" %in% names(dt)) {
    comp <- gsub("[^0-9]", "", as.character(dt$competencia))
    ok6 <- !is.na(comp) & nchar(comp) == 6
    dt[, data := as.Date(NA)]
    dt[ok6, data := as.Date(paste0(substr(comp[ok6], 1, 4), "-", substr(comp[ok6], 5, 6), "-01"))]
  } else if (isTRUE(exigir_data)) {
    stop("Base sem coluna data, sem ano+mes e sem competencia.")
  }
  
  if ("data" %in% names(dt)) {
    dt[, data := as.Date(lubridate::floor_date(data, unit = "month"))]
    dt[, ano := as.integer(format(data, "%Y"))]
    dt[, mes := as.integer(format(data, "%m"))]
  } else if ("ano" %in% names(dt)) {
    dt[, ano := as.integer(to_num(ano))]
  }
  
  dt
}

safe_update_join <- function(base, add, by_cols, cols_add, etapa = "") {
  setDT(base)
  setDT(add)
  
  cols_add <- intersect(cols_add, names(add))
  if (nrow(add) == 0 || length(cols_add) == 0) return(invisible(base))
  
  checar_unicidade(add, by_cols, paste0("join_source_", etapa))
  n_antes <- nrow(base)
  
  setkeyv(base, by_cols)
  setkeyv(add, by_cols)
  
  for (cc in cols_add) {
    base[add, (cc) := get(paste0("i.", cc)), on = by_cols]
  }
  
  checar_linhas(base, n_antes, etapa)
  invisible(base)
}

# ============================================================
# 3. CARREGAR BASES
# ============================================================

cat("\nCarregando bases de: ", pasta_base, "\n", sep = "")

env_den <- new.env()
load(file.path(pasta_base, "denominador.RData"), envir = env_den)
objetos_den <- ls(env_den)

if ("denominador_final" %in% objetos_den) {
  denominador_final <- get("denominador_final", envir = env_den)
} else {
  candidatos_den <- objetos_den[
    vapply(
      objetos_den,
      function(obj) {
        x <- get(obj, envir = env_den)
        is.data.frame(x) && all(c("code_muni", "data", "faixa_etaria", "denominador") %in% names(x))
      },
      logical(1)
    )
  ]
  if (length(candidatos_den) == 0) stop("Nao encontrei denominador_final ou equivalente em denominador.RData.")
  denominador_final <- get(candidatos_den[1], envir = env_den)
}
rm(env_den)

sim_menores5_sum_municipio_mes_faixa <- readRDS(file.path(pasta_base, "sim_menores5_sum_municipio_mes_faixa.rds"))
bolsa_familia_beneficios_municipio_mes <- readRDS(file.path(pasta_base, "bolsa_familia_beneficios_municipio_mes.rds"))
atencao_basica_municipio_mes <- readRDS(file.path(pasta_base, "atencao_basica_municipio_mes.rds"))
snis_agua_esgoto_municipio_ano <- readRDS(file.path(pasta_base, "snis_agua_esgoto_municipio_ano.rds"))
vacinas_municipio_ano <- readRDS(file.path(pasta_base, "vacinas_municipio_ano.rds"))

arquivo_mais_medicos <- file.path(pasta_base, "mais_medicos.csv")
if (!file.exists(arquivo_mais_medicos)) stop("Arquivo nao encontrado: ", arquivo_mais_medicos)
mais_medicos <- read.csv2(arquivo_mais_medicos)

# ============================================================
# 4. PREPARAR DENOMINADOR MENSAL COM GERAL <5 ANOS
# ============================================================

setDT(denominador_final)

den <- copy(denominador_final)

den <- preparar_code_data(
  den,
  code_candidates = c("code", "code_muni", "code_muni6", "codmun", "codigo_municipio")
)

if (!"faixa_etaria" %in% names(den)) stop("denominador_final sem faixa_etaria.")
if (!"denominador" %in% names(den)) stop("denominador_final sem denominador.")

den[, faixa_etaria := as.character(faixa_etaria)]
den[, denominador := to_num(denominador)]

den <- den[!is.na(code) & !is.na(data) & !is.na(faixa_etaria) & !is.na(denominador)]

den_mes_raw <- den[
  ,
  .(denominador = sum(denominador, na.rm = TRUE)),
  by = .(code, data, ano, mes, faixa_etaria)
]

faixas_den <- sort(unique(den_mes_raw$faixa_etaria))
faixas_total_den <- intersect(faixas_total_menor5, faixas_den)
faixas_desag_den <- setdiff(faixas_den, faixas_total_den)

den_mes_faixas <- den_mes_raw[faixa_etaria %in% faixas_desag_den]

if (length(faixas_total_den) > 0) {
  den_mes_geral <- den_mes_raw[
    faixa_etaria == faixas_total_den[1],
    .(denominador = sum(denominador, na.rm = TRUE)),
    by = .(code, data, ano, mes)
  ]
} else {
  den_mes_geral <- den_mes_faixas[
    ,
    .(denominador = sum(denominador, na.rm = TRUE)),
    by = .(code, data, ano, mes)
  ]
}

den_mes_geral[, faixa_etaria := rotulo_geral_menor5]

den_mes <- rbindlist(list(den_mes_geral, den_mes_faixas), use.names = TRUE, fill = TRUE)
setcolorder(den_mes, c("code", "data", "ano", "mes", "faixa_etaria", "denominador"))
setorder(den_mes, code, data, faixa_etaria)
checar_unicidade(den_mes, c("code", "data", "faixa_etaria"), "denominador_mensal")

# ============================================================
# 5. PREPARAR OBITOS MENSAIS COM GERAL <5 ANOS
# ============================================================

setDT(sim_menores5_sum_municipio_mes_faixa)

ob <- copy(sim_menores5_sum_municipio_mes_faixa)
ob <- preparar_code_data(
  ob,
  code_candidates = c("code", "code_muni", "code_muni6", "codmun", "codigo_municipio")
)

if (!"faixa_etaria" %in% names(ob)) stop("Base SIM sem faixa_etaria.")
if (!"obitos" %in% names(ob)) stop("Base SIM sem obitos.")

ob[, faixa_etaria := as.character(faixa_etaria)]
ob[, obitos := to_num(obitos)]

ob <- ob[!is.na(code) & !is.na(data) & !is.na(faixa_etaria) & !is.na(obitos)]

ob_mes_raw <- ob[
  ,
  .(obitos = sum(obitos, na.rm = TRUE)),
  by = .(code, data, ano, mes, faixa_etaria)
]

faixas_ob <- sort(unique(ob_mes_raw$faixa_etaria))
faixas_total_ob <- intersect(faixas_total_menor5, faixas_ob)
faixas_desag_ob <- setdiff(faixas_ob, faixas_total_ob)

ob_mes_faixas <- ob_mes_raw[faixa_etaria %in% faixas_desag_ob]

if (length(faixas_total_ob) > 0) {
  ob_mes_geral <- ob_mes_raw[
    faixa_etaria == faixas_total_ob[1],
    .(obitos = sum(obitos, na.rm = TRUE)),
    by = .(code, data, ano, mes)
  ]
} else {
  ob_mes_geral <- ob_mes_faixas[
    ,
    .(obitos = sum(obitos, na.rm = TRUE)),
    by = .(code, data, ano, mes)
  ]
}

ob_mes_geral[, faixa_etaria := rotulo_geral_menor5]

ob_mes <- rbindlist(list(ob_mes_geral, ob_mes_faixas), use.names = TRUE, fill = TRUE)
setcolorder(ob_mes, c("code", "data", "ano", "mes", "faixa_etaria", "obitos"))
setorder(ob_mes, code, data, faixa_etaria)
checar_unicidade(ob_mes, c("code", "data", "faixa_etaria"), "obitos_mensais")

# ============================================================
# 6. PREPARAR POLITICAS E CONTEXTO
# ============================================================

# ----------------------------
# Bolsa Familia mensal
# ----------------------------
setDT(bolsa_familia_beneficios_municipio_mes)

bf_mes <- copy(bolsa_familia_beneficios_municipio_mes)
bf_mes <- preparar_code_data(
  bf_mes,
  code_candidates = c("code", "code_muni", "code_muni6", "codmun", "codigo_municipio")
)

if (!"total_beneficios_bf" %in% names(bf_mes)) stop("Base Bolsa Familia sem total_beneficios_bf.")

bf_mes[, total_beneficios_bf := to_num(total_beneficios_bf)]
bf_mes <- bf_mes[
  !is.na(code) & !is.na(data),
  .(total_beneficios_bf = sum(total_beneficios_bf, na.rm = TRUE)),
  by = .(code, data)
]
checar_unicidade(bf_mes, c("code", "data"), "bolsa_familia_mensal")

# ----------------------------
# Mais Medicos mensal
# ----------------------------
setDT(mais_medicos)

if (!"dt_referencia" %in% names(mais_medicos)) stop("mais_medicos.csv sem coluna dt_referencia.")
if (!"ibge" %in% names(mais_medicos)) stop("mais_medicos.csv sem coluna ibge.")
if (!"total_prof_ativos" %in% names(mais_medicos)) stop("mais_medicos.csv sem coluna total_prof_ativos.")

mais_medicos[, dt_referencia_raw := dt_referencia]
mais_medicos[, dt_referencia := suppressWarnings(lubridate::dmy(dt_referencia_raw))]
mais_medicos[is.na(dt_referencia), dt_referencia := as_date_safe(dt_referencia_raw)]
mais_medicos[, data := as.Date(lubridate::floor_date(dt_referencia, unit = "month"))]
mais_medicos[, code := pad_code6(ibge)]
mais_medicos[, total_prof_ativos := to_num(total_prof_ativos)]

mais_medicos_muni_mes <- mais_medicos[
  !is.na(code) & !is.na(data),
  .(total_prof_ativos = sum(total_prof_ativos, na.rm = TRUE)),
  by = .(code, data)
]
checar_unicidade(mais_medicos_muni_mes, c("code", "data"), "mais_medicos_mensal")

# ----------------------------
# APS mensal
# ----------------------------
setDT(atencao_basica_municipio_mes)

aps <- copy(atencao_basica_municipio_mes)
aps <- preparar_code_data(
  aps,
  code_candidates = c(
    "code", "code_muni", "code_muni6", "codigo_municipio", "cod_mun", "codmun",
    "ibge", "codigo_ibge", "co_municipio"
  )
)

cols_excluir_aps <- unique(c(
  "code", "data", "ano", "mes", "municipio", "municipio_nome", "nome_municipio",
  "uf", "UF", "competencia", "codigo_municipio", "cod_mun", "codmun", "ibge",
  "codigo_ibge", "co_municipio", "code_muni", "code_muni6"
))

vars_aps <- setdiff(names(aps), cols_excluir_aps)
vars_aps_num <- vars_aps[vapply(aps[, ..vars_aps], is.numeric, logical(1))]

if (length(vars_aps_num) == 0) {
  stop("Nenhuma variavel numerica encontrada na base APS.")
}

vars_aps_media <- grep(
  "pct|perc|percent|cobertura|proporcao|proporção|taxa|indice|índice",
  vars_aps_num,
  value = TRUE,
  ignore.case = TRUE
)
vars_aps_soma <- setdiff(vars_aps_num, vars_aps_media)

lista_aps <- list()

if (length(vars_aps_soma) > 0) {
  lista_aps[["soma"]] <- aps[
    !is.na(code) & !is.na(data),
    lapply(.SD, sum_na),
    by = .(code, data),
    .SDcols = vars_aps_soma
  ]
}

if (length(vars_aps_media) > 0) {
  lista_aps[["media"]] <- aps[
    !is.na(code) & !is.na(data),
    lapply(.SD, mean_na),
    by = .(code, data),
    .SDcols = vars_aps_media
  ]
}

aps_muni_mes <- Reduce(
  function(x, y) merge(x, y, by = c("code", "data"), all = TRUE),
  lista_aps
)

corrigir_nan(aps_muni_mes)

vars_aps_final <- setdiff(names(aps_muni_mes), c("code", "data"))
setnames(
  aps_muni_mes,
  old = vars_aps_final,
  new = ifelse(grepl("^aps_", vars_aps_final), vars_aps_final, paste0("aps_", vars_aps_final))
)

vars_aps_final <- setdiff(names(aps_muni_mes), c("code", "data"))
checar_unicidade(aps_muni_mes, c("code", "data"), "aps_mensal")

# ----------------------------
# SNIS anual
# ----------------------------
setDT(snis_agua_esgoto_municipio_ano)

snis <- copy(snis_agua_esgoto_municipio_ano)
snis <- preparar_code_data(
  snis,
  code_candidates = c("code", "code_muni", "code_muni6", "codmun", "codigo_municipio"),
  exigir_data = FALSE
)

if (!"ano" %in% names(snis)) {
  if ("data" %in% names(snis)) snis[, ano := ano_from_data(data)] else stop("SNIS sem ano ou data.")
}

snis[, ano := as.integer(to_num(ano))]

for (v in intersect(c("cobertura_urbana_agua_pct", "cobertura_urbana_esgoto_pct"), names(snis))) {
  snis[, (v) := to_num(get(v))]
}

snis_ano <- snis[
  !is.na(code) & !is.na(ano),
  .(
    cobertura_urbana_agua_pct = mean_na(cobertura_urbana_agua_pct),
    cobertura_urbana_esgoto_pct = mean_na(cobertura_urbana_esgoto_pct)
  ),
  by = .(code, ano)
]

corrigir_nan(snis_ano)
checar_unicidade(snis_ano, c("code", "ano"), "snis_anual")

# ----------------------------
# Vacinas anual
# ----------------------------
setDT(vacinas_municipio_ano)

vac <- copy(vacinas_municipio_ano)
vac <- preparar_code_data(
  vac,
  code_candidates = c("code", "code_muni", "code_muni6", "codmun", "codigo_municipio"),
  exigir_data = FALSE
)

if (!"ano" %in% names(vac)) {
  if ("data" %in% names(vac)) vac[, ano := ano_from_data(data)] else stop("Vacinas sem ano ou data.")
}

vac[, ano := as.integer(to_num(ano))]

vars_vac <- intersect(
  c("cobertura_bcg", "cobertura_penta", "cobertura_pneumococica", "cobertura_rotavirus"),
  names(vac)
)
if (length(vars_vac) == 0) stop("Base de vacinas sem variaveis de cobertura esperadas.")

for (v in vars_vac) vac[, (v) := to_num(get(v))]

vac_ano <- vac[
  !is.na(code) & !is.na(ano),
  lapply(.SD, mean_na),
  by = .(code, ano),
  .SDcols = vars_vac
]

corrigir_nan(vac_ano)
checar_unicidade(vac_ano, c("code", "ano"), "vacinas_anual")

# ----------------------------
# Favelas/comunidades urbanas anual
# ----------------------------
arquivo_2010 <- file.path(pasta_base, "Tabela 3379.xlsx")
arquivo_2022 <- file.path(pasta_base, "Tabela 9883.xlsx")

ler_tabela_favela <- function(arquivo, ano_ref) {
  if (!file.exists(arquivo)) stop("Arquivo nao encontrado: ", arquivo)
  
  dt <- readxl::read_excel(path = arquivo, sheet = "Tabela", col_types = c("text", "numeric"))
  dt <- as.data.table(dt)
  setnames(dt, old = names(dt)[1:2], new = c("code_muni7", "n_favelas"))
  
  dt[, code_muni7 := stringr::str_replace_all(as.character(code_muni7), "[^0-9]", "")]
  dt[, code := pad_code6(code_muni7)]
  dt[, ano := as.integer(ano_ref)]
  dt[, n_favelas := to_num(n_favelas)]
  
  dt[
    !is.na(code) & !is.na(n_favelas),
    .(n_favelas = sum(n_favelas, na.rm = TRUE)),
    by = .(code, ano)
  ]
}

favela_2010 <- ler_tabela_favela(arquivo_2010, 2010)
favela_2022 <- ler_tabela_favela(arquivo_2022, 2022)
favela_pontos <- rbindlist(list(favela_2010, favela_2022), fill = TRUE)

# ============================================================
# 7. CONSTRUIR BASE MENSAL FINAL
# ============================================================

base_final_mortalidade_contexto <- copy(den_mes)

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = ob_mes[, .(code, data, faixa_etaria, obitos)],
  by_cols = c("code", "data", "faixa_etaria"),
  cols_add = "obitos",
  etapa = "obitos"
)

base_final_mortalidade_contexto[is.na(obitos), obitos := 0]

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = bf_mes,
  by_cols = c("code", "data"),
  cols_add = "total_beneficios_bf",
  etapa = "bolsa_familia"
)

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = mais_medicos_muni_mes,
  by_cols = c("code", "data"),
  cols_add = "total_prof_ativos",
  etapa = "mais_medicos"
)

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = aps_muni_mes,
  by_cols = c("code", "data"),
  cols_add = vars_aps_final,
  etapa = "aps"
)

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = snis_ano,
  by_cols = c("code", "ano"),
  cols_add = c("cobertura_urbana_agua_pct", "cobertura_urbana_esgoto_pct"),
  etapa = "snis"
)

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = vac_ano,
  by_cols = c("code", "ano"),
  cols_add = vars_vac,
  etapa = "vacinas"
)

# Favelas: grade anual baseada nos codigos observados na base mensal.
codigos_base <- sort(unique(base_final_mortalidade_contexto$code))
favela_ano <- CJ(code = codigos_base, ano = 2010:2024, unique = TRUE)
favela_ano <- merge(favela_ano, favela_pontos, by = c("code", "ano"), all.x = TRUE)
favela_ano[ano %in% c(2010, 2022) & is.na(n_favelas), n_favelas := 0]

favela_ano[
  ,
  n_favelas_interpolado := {
    anos_obs <- ano[ano %in% c(2010, 2022)]
    vals_obs <- n_favelas[ano %in% c(2010, 2022)]
    if (length(vals_obs) == 2 && all(!is.na(vals_obs))) {
      approx(x = anos_obs, y = vals_obs, xout = ano, rule = 2)$y
    } else {
      rep(NA_real_, .N)
    }
  },
  by = code
]

favela_ano[ano > 2022, n_favelas_interpolado := n_favelas_interpolado[ano == 2022][1], by = code]
favela_ano[, n_favelas_interpolado := round(n_favelas_interpolado)]
favela_ano[
  ,
  metodo_n_favelas := fcase(
    ano %in% c(2010, 2022), "observado_censo",
    ano > 2010 & ano < 2022, "interpolado_linear",
    ano > 2022, "carregado_2022",
    default = NA_character_
  )
]

favela_ano_final <- favela_ano[
  ,
  .(code, ano, n_favelas = n_favelas_interpolado, metodo_n_favelas)
]
checar_unicidade(favela_ano_final, c("code", "ano"), "favela_anual")

safe_update_join(
  base = base_final_mortalidade_contexto,
  add = favela_ano_final,
  by_cols = c("code", "ano"),
  cols_add = c("n_favelas", "metodo_n_favelas"),
  etapa = "favelas"
)

setorder(base_final_mortalidade_contexto, code, data, faixa_etaria)
base_final_mortalidade_contexto[, code_muni := code]

# ============================================================
# 8. PADRONIZAR VARIAVEIS ANALITICAS NA BASE MENSAL
# ============================================================

# ESF principal: identifica a melhor coluna disponivel.
esf_candidates <- c(
  "aps_proporcao_cobertura_estrategia_saude_familia",
  "aps_cobertura_esf_pct",
  "cobertura_esf_pct",
  grep("aps_.*(estrategia|estratégia).*familia", names(base_final_mortalidade_contexto), value = TRUE, ignore.case = TRUE),
  grep("aps_.*esf", names(base_final_mortalidade_contexto), value = TRUE, ignore.case = TRUE)
)
col_esf <- primeira_existente(names(base_final_mortalidade_contexto), unique(esf_candidates))

if (is.na(col_esf)) {
  warning("Nao encontrei coluna clara de cobertura ESF. cobertura_esf_pct sera criada como NA.")
  base_final_mortalidade_contexto[, cobertura_esf_pct := NA_real_]
} else {
  base_final_mortalidade_contexto[, cobertura_esf_pct := to_num(get(col_esf))]
}

ab_candidates <- c(
  "aps_proporcao_cobertura_total_atencao_basica",
  "aps_cobertura_total_ab_pct",
  "cobertura_total_ab_pct",
  grep("aps_.*total.*(atencao|atenção|basica|básica)", names(base_final_mortalidade_contexto), value = TRUE, ignore.case = TRUE)
)
col_ab <- primeira_existente(names(base_final_mortalidade_contexto), unique(ab_candidates))

if (!is.na(col_ab)) {
  base_final_mortalidade_contexto[, cobertura_total_ab_pct := to_num(get(col_ab))]
}

if (!"total_prof_ativos" %in% names(base_final_mortalidade_contexto)) {
  base_final_mortalidade_contexto[, total_prof_ativos := NA_real_]
}

base_final_mortalidade_contexto[, mais_medicos_prof_ativos := to_num(total_prof_ativos)]
base_final_mortalidade_contexto[mais_medicos_prof_ativos < 0, mais_medicos_prof_ativos := NA_real_]

vars_contextuais_numericas <- intersect(
  c(
    "total_beneficios_bf", "total_prof_ativos", "mais_medicos_prof_ativos",
    "cobertura_esf_pct", "cobertura_total_ab_pct",
    "cobertura_urbana_agua_pct", "cobertura_urbana_esgoto_pct",
    "cobertura_bcg", "cobertura_penta", "cobertura_pneumococica", "cobertura_rotavirus",
    "n_favelas"
  ),
  names(base_final_mortalidade_contexto)
)

for (v in vars_contextuais_numericas) {
  base_final_mortalidade_contexto[, (v) := to_num(get(v))]
}

base_final_mortalidade_contexto[, taxa_1000_mensal := fifelse(
  denominador > 0,
  obitos / denominador * 1000,
  NA_real_
)]

checar_unicidade(
  base_final_mortalidade_contexto,
  c("code", "data", "faixa_etaria"),
  "base_final_mortalidade_contexto_mensal"
)

# ============================================================
# 9. BASE FILTRADA MENSAL
# ============================================================

n_antes_filtro <- nrow(base_final_mortalidade_contexto)

base_final_mortalidade_contexto_filtrada <- base_final_mortalidade_contexto[
  !is.na(code) & nchar(code) == 6 &
    !is.na(data) & !is.na(ano) & !is.na(mes) & mes >= 1 & mes <= 12 &
    !is.na(faixa_etaria) &
    !is.na(denominador) & denominador > 0 &
    !is.na(obitos) & obitos >= 0
]

if (nrow(base_final_mortalidade_contexto_filtrada) == 0) {
  stop("A base filtrada ficou vazia. Verifique denominador, obitos e chaves.")
}

setorder(base_final_mortalidade_contexto_filtrada, code, data, faixa_etaria)
df <- copy(base_final_mortalidade_contexto_filtrada)

checar_unicidade(
  base_final_mortalidade_contexto_filtrada,
  c("code", "data", "faixa_etaria"),
  "base_final_mortalidade_contexto_filtrada"
)

# ============================================================
# 10. CONSTRUIR BASE ANUAL LEVE A PARTIR DA BASE MENSAL
# ============================================================

base_m <- copy(base_final_mortalidade_contexto_filtrada)

base_anual_mortalidade_contexto <- base_m[
  ,
  .(
    denominador_soma_meses = sum(denominador, na.rm = TRUE),
    denominador = sum(denominador, na.rm = TRUE),
    crianca_ano_risco = sum(denominador, na.rm = TRUE) / 12,
    obitos = sum(obitos, na.rm = TRUE),
    meses_denominador = uniqueN(data[!is.na(denominador)]),
    meses_obitos = uniqueN(data[!is.na(obitos)])
  ),
  by = .(code, ano, faixa_etaria)
]

# Politicas/contexto: maior valor observado no ano por municipio.
vars_policy_num <- intersect(
  c(
    "total_beneficios_bf", "total_prof_ativos", "mais_medicos_prof_ativos",
    "cobertura_esf_pct", "cobertura_total_ab_pct",
    "aps_proporcao_cobertura_estrategia_saude_familia",
    "aps_proporcao_cobertura_total_atencao_basica",
    "cobertura_urbana_agua_pct", "cobertura_urbana_esgoto_pct",
    "cobertura_bcg", "cobertura_penta", "cobertura_pneumococica", "cobertura_rotavirus",
    "n_favelas"
  ),
  names(base_m)
)

policy_ano_num <- base_m[
  ,
  lapply(.SD, max_na),
  by = .(code, ano),
  .SDcols = vars_policy_num
]

safe_update_join(
  base = base_anual_mortalidade_contexto,
  add = policy_ano_num,
  by_cols = c("code", "ano"),
  cols_add = vars_policy_num,
  etapa = "politicas_contexto_anual"
)

if ("metodo_n_favelas" %in% names(base_m)) {
  policy_ano_chr <- base_m[
    ,
    .(metodo_n_favelas = first_non_na(metodo_n_favelas)),
    by = .(code, ano)
  ]
  safe_update_join(
    base = base_anual_mortalidade_contexto,
    add = policy_ano_chr,
    by_cols = c("code", "ano"),
    cols_add = "metodo_n_favelas",
    etapa = "metodo_favelas_anual"
  )
}

base_anual_mortalidade_contexto[, taxa_1000_soma_meses := fifelse(
  denominador_soma_meses > 0,
  obitos / denominador_soma_meses * 1000,
  NA_real_
)]

base_anual_mortalidade_contexto[, taxa_1000_crianca_ano := fifelse(
  crianca_ano_risco > 0,
  obitos / crianca_ano_risco * 1000,
  NA_real_
)]

if (!"total_beneficios_bf" %in% names(base_anual_mortalidade_contexto)) {
  base_anual_mortalidade_contexto[, total_beneficios_bf := NA_real_]
}
if (!"mais_medicos_prof_ativos" %in% names(base_anual_mortalidade_contexto)) {
  base_anual_mortalidade_contexto[, mais_medicos_prof_ativos := NA_real_]
}
if (!"n_favelas" %in% names(base_anual_mortalidade_contexto)) {
  base_anual_mortalidade_contexto[, n_favelas := NA_real_]
}

base_anual_mortalidade_contexto[, bf_beneficios_por_1000_crianca := fifelse(
  !is.na(total_beneficios_bf) & crianca_ano_risco > 0,
  total_beneficios_bf / crianca_ano_risco * 1000,
  NA_real_
)]

base_anual_mortalidade_contexto[, mais_medicos_por_10000_crianca := fifelse(
  !is.na(mais_medicos_prof_ativos) & crianca_ano_risco > 0,
  mais_medicos_prof_ativos / crianca_ano_risco * 10000,
  NA_real_
)]

base_anual_mortalidade_contexto[, favelas_por_10000_crianca := fifelse(
  !is.na(n_favelas) & crianca_ano_risco > 0,
  n_favelas / crianca_ano_risco * 10000,
  NA_real_
)]

base_anual_mortalidade_contexto[, code_muni := code]
setorder(base_anual_mortalidade_contexto, code, ano, faixa_etaria)

checar_unicidade(
  base_anual_mortalidade_contexto,
  c("code", "ano", "faixa_etaria"),
  "base_anual_mortalidade_contexto"
)

# ============================================================
# 11. AUDITORIAS
# ============================================================

auditoria_pre_filtro <- data.table(
  indicador = c(
    "linhas_mensal_antes_filtro",
    "linhas_mensal_depois_filtro",
    "linhas_removidas",
    "municipios_filtrada",
    "anos_filtrada",
    "ano_min_filtrada",
    "ano_max_filtrada",
    "pct_esf_preenchida_filtrada",
    "pct_mais_medicos_preenchida_filtrada"
  ),
  valor = c(
    n_antes_filtro,
    nrow(base_final_mortalidade_contexto_filtrada),
    n_antes_filtro - nrow(base_final_mortalidade_contexto_filtrada),
    uniqueN(base_final_mortalidade_contexto_filtrada$code),
    uniqueN(base_final_mortalidade_contexto_filtrada$ano),
    min(base_final_mortalidade_contexto_filtrada$ano, na.rm = TRUE),
    max(base_final_mortalidade_contexto_filtrada$ano, na.rm = TRUE),
    100 * mean(!is.na(base_final_mortalidade_contexto_filtrada$cobertura_esf_pct)),
    100 * mean(!is.na(base_final_mortalidade_contexto_filtrada$mais_medicos_prof_ativos))
  )
)

fwrite(auditoria_pre_filtro, file.path(pasta_auditoria, "auditoria_pre_filtro.csv"))

auditoria_base_filtrada_por_ano <- base_final_mortalidade_contexto_filtrada[
  ,
  .(
    linhas = .N,
    municipios = uniqueN(code),
    faixas = uniqueN(faixa_etaria),
    obitos = sum(obitos, na.rm = TRUE),
    denominador_mensal = sum(denominador, na.rm = TRUE),
    crianca_ano_risco_aprox = sum(denominador, na.rm = TRUE) / 12,
    taxa_1000_crianca_ano = sum(obitos, na.rm = TRUE) / (sum(denominador, na.rm = TRUE) / 12) * 1000,
    n_esf_preenchida = sum(!is.na(cobertura_esf_pct)),
    pct_esf_preenchida = 100 * mean(!is.na(cobertura_esf_pct)),
    media_esf = mean(cobertura_esf_pct, na.rm = TRUE),
    mediana_esf = median(cobertura_esf_pct, na.rm = TRUE),
    n_mais_medicos_preenchida = sum(!is.na(mais_medicos_prof_ativos)),
    pct_mais_medicos_preenchida = 100 * mean(!is.na(mais_medicos_prof_ativos)),
    media_mais_medicos = mean(mais_medicos_prof_ativos, na.rm = TRUE),
    mediana_mais_medicos = median(mais_medicos_prof_ativos, na.rm = TRUE)
  ),
  by = ano
][order(ano)]

fwrite(auditoria_base_filtrada_por_ano, file.path(pasta_auditoria, "auditoria_base_filtrada_por_ano.csv"))

auditoria_base_anual_por_ano <- base_anual_mortalidade_contexto[
  ,
  .(
    linhas = .N,
    municipios = uniqueN(code),
    faixas = uniqueN(faixa_etaria),
    obitos = sum(obitos, na.rm = TRUE),
    crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE),
    taxa_1000_crianca_ano = sum(obitos, na.rm = TRUE) / sum(crianca_ano_risco, na.rm = TRUE) * 1000
  ),
  by = ano
][order(ano)]

fwrite(auditoria_base_anual_por_ano, file.path(pasta_auditoria, "auditoria_base_anual_por_ano.csv"))

vars_auditoria_contexto <- intersect(
  c(
    "total_beneficios_bf", "mais_medicos_prof_ativos", "cobertura_esf_pct", "cobertura_total_ab_pct",
    "cobertura_urbana_agua_pct", "cobertura_urbana_esgoto_pct",
    "cobertura_bcg", "cobertura_penta", "cobertura_pneumococica", "cobertura_rotavirus",
    "n_favelas"
  ),
  names(base_anual_mortalidade_contexto)
)

auditoria_variaveis_contextuais_anual <- rbindlist(
  lapply(vars_auditoria_contexto, function(v) {
    base_anual_mortalidade_contexto[
      ,
      .(
        variavel = v,
        linhas = .N,
        n_preenchido = sum(!is.na(get(v))),
        pct_preenchido = 100 * mean(!is.na(get(v))),
        media = mean(get(v), na.rm = TRUE),
        mediana = median(get(v), na.rm = TRUE),
        p25 = as.numeric(quantile(get(v), 0.25, na.rm = TRUE, names = FALSE)),
        p75 = as.numeric(quantile(get(v), 0.75, na.rm = TRUE, names = FALSE)),
        minimo = suppressWarnings(min(get(v), na.rm = TRUE)),
        maximo = suppressWarnings(max(get(v), na.rm = TRUE)),
        n_unicos = uniqueN(get(v)[!is.na(get(v))])
      ),
      by = ano
    ]
  }),
  fill = TRUE
)[order(variavel, ano)]

auditoria_variaveis_contextuais_anual[is.infinite(minimo), minimo := NA_real_]
auditoria_variaveis_contextuais_anual[is.infinite(maximo), maximo := NA_real_]
fwrite(auditoria_variaveis_contextuais_anual, file.path(pasta_auditoria, "auditoria_variaveis_contextuais_anual.csv"))

# ============================================================
# 12. SALVAR
# ============================================================

if (isTRUE(salvar_rds)) {
  saveRDS(favela_pontos, file.path(pasta_base, "favela_pontos_2010_2022.rds"))
  saveRDS(favela_ano_final, file.path(pasta_base, "favela_municipio_ano_2010_2024_interpolado.rds"))
  
  saveRDS(base_final_mortalidade_contexto, file.path(pasta_base, "base_final_mortalidade_contexto_com_aps_favelas.rds"))
  saveRDS(base_final_mortalidade_contexto, file.path(pasta_base, "base_final_mortalidade_contexto.rds"))
  saveRDS(base_final_mortalidade_contexto_filtrada, file.path(pasta_base, "base_final_mortalidade_contexto_filtrada.rds"))
  saveRDS(base_anual_mortalidade_contexto, file.path(pasta_anual, "base_anual_mortalidade_contexto.rds"))
}

if (isTRUE(salvar_csv_mensal)) {
  fwrite(base_final_mortalidade_contexto, file.path(pasta_base, "base_final_mortalidade_contexto.csv"))
  fwrite(base_final_mortalidade_contexto_filtrada, file.path(pasta_base, "base_final_mortalidade_contexto_filtrada.csv"))
}

if (isTRUE(salvar_csv_anual)) {
  fwrite(base_anual_mortalidade_contexto, file.path(pasta_anual, "base_anual_mortalidade_contexto.csv"))
}

# ============================================================
# 13. RESUMO FINAL
# ============================================================

cat("\n============================================================\n")
cat("JOIN UNIFICADO FINALIZADO\n")
cat("============================================================\n")

cat("\nObjetos criados no ambiente:\n")
cat("- base_final_mortalidade_contexto\n")
cat("- base_final_mortalidade_contexto_filtrada\n")
cat("- base_anual_mortalidade_contexto\n")
cat("- df\n")

cat("\nResumo da base mensal filtrada:\n")
print(
  base_final_mortalidade_contexto_filtrada[
    ,
    .(
      linhas = .N,
      municipios = uniqueN(code),
      ano_min = min(ano, na.rm = TRUE),
      ano_max = max(ano, na.rm = TRUE),
      faixas = uniqueN(faixa_etaria),
      obitos = sum(obitos, na.rm = TRUE),
      denominador = sum(denominador, na.rm = TRUE)
    )
  ]
)

cat("\nResumo da base anual:\n")
print(
  base_anual_mortalidade_contexto[
    ,
    .(
      linhas = .N,
      municipios = uniqueN(code),
      ano_min = min(ano, na.rm = TRUE),
      ano_max = max(ano, na.rm = TRUE),
      faixas = uniqueN(faixa_etaria),
      obitos = sum(obitos, na.rm = TRUE),
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE)
    )
  ]
)

cat("\nResumo anual por faixa etaria:\n")
print(
  base_anual_mortalidade_contexto[
    ,
    .(
      linhas = .N,
      municipios = uniqueN(code),
      obitos = sum(obitos, na.rm = TRUE),
      crianca_ano_risco = sum(crianca_ano_risco, na.rm = TRUE),
      taxa_1000_crianca_ano = sum(obitos, na.rm = TRUE) / sum(crianca_ano_risco, na.rm = TRUE) * 1000
    ),
    by = faixa_etaria
  ][order(faixa_etaria)]
)

cat("\nArquivos principais salvos em:\n")
cat("- ", file.path(pasta_base, "base_final_mortalidade_contexto.rds"), "\n", sep = "")
cat("- ", file.path(pasta_base, "base_final_mortalidade_contexto_filtrada.rds"), "\n", sep = "")
cat("- ", file.path(pasta_anual, "base_anual_mortalidade_contexto.rds"), "\n", sep = "")
cat("- ", file.path(pasta_anual, "base_anual_mortalidade_contexto.csv"), "\n", sep = "")
cat("- Auditorias: ", pasta_auditoria, "\n", sep = "")

cat("\nRegras preservadas:\n")
cat("\n- code municipal em 6 digitos DATASUS.")
cat("\n- Geral <5 anos analisado separadamente das faixas desagregadas.")
cat("\n- df e apenas copia da base mensal filtrada.")
cat("\n- Base anual criada no mesmo fluxo, sem precisar rodar outro join.\n")
