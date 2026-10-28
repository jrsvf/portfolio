# =============================================================================
# Extras das páginas de cadeia: cursos ligados à cadeia e escoamento das
# exportações (via de transporte e ponto de saída).
# =============================================================================

normalizar_txt <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT"))

# ---------------------------------------------------------------------------
# 1. Cursos superiores presenciais por nome de curso x município (último ano)
# ---------------------------------------------------------------------------
extrair_cursos_nome <- function(projeto, uf) {
  sql <- function(com_grau) glue::glue("
    SELECT ano, id_municipio, nome_curso, {if (com_grau) 'CAST(tipo_grau_academico AS STRING)' else 'NULL'} AS grau,
           CAST(tipo_organizacao_administrativa AS STRING) AS adm,
           COUNT(*) AS cursos, SUM(quantidade_vagas) AS vagas, SUM(quantidade_matriculas) AS matriculas,
           SUM(quantidade_concluintes) AS concluintes
    FROM `basedosdados.br_inep_censo_educacao_superior.curso`
    WHERE sigla_uf = '{uf}' AND CAST(tipo_modalidade_ensino AS STRING) = '1'
      AND ano = (SELECT MAX(ano) FROM `basedosdados.br_inep_censo_educacao_superior.curso` WHERE sigla_uf = '{uf}')
    GROUP BY 1, 2, 3, 4, 5")
  df <- tryCatch(consultar(sql(TRUE), projeto), error = function(e) {
    msg("Cursos com grau acadêmico falhou (", conditionMessage(e), "); tentando sem o grau.")
    consultar(sql(FALSE), projeto)
  })
  df %>% mutate(across(c(ano, cursos, vagas, matriculas, concluintes), as.numeric), id_municipio = as.character(id_municipio))
}

ler_cadeia_cursos <- function(arq) readr::read_csv(arq, col_types = "ccc") %>% mutate(excluir = coalesce(excluir, ""))

cursos_cadeia <- function(cursos, mapa, dic, municipios) {
  if (!NROW(cursos) || !NROW(mapa)) return(tibble())
  rot <- function(x, col) rotular(x, dic, "br_inep_censo_educacao_superior", "curso", col)
  cs <- cursos %>% mutate(
    curso_n = normalizar_txt(nome_curso),
    grau = rot(grau, "tipo_grau_academico"),
    rede = ifelse(grepl("p[úu]blica|federal|estadual|municipal|especial", rot(adm, "tipo_organizacao_administrativa"),
                        ignore.case = TRUE) | adm %in% c("1", "2", "3", "7"), "Pública", "Privada"))
  purrr::map_dfr(seq_len(nrow(mapa)), function(i) {
    m <- mapa[i, ]
    ok <- grepl(m$padrao, cs$curso_n, perl = TRUE)
    if (nzchar(m$excluir)) ok <- ok & !grepl(m$excluir, cs$curso_n, perl = TRUE)
    cs[ok, ] %>% mutate(cadeia_id = m$cadeia_id)
  }) %>%
    left_join(municipios %>% select(id_municipio, municipio), by = "id_municipio") %>%
    select(cadeia_id, ano, id_municipio, municipio, nome_curso, grau, rede, cursos, vagas, matriculas, concluintes)
}

# ---------------------------------------------------------------------------
# 2. Escoamento: exportações por UF de produção x SH4 x via x URF de saída
# ---------------------------------------------------------------------------
extrair_comex_escoamento <- function(projeto, uf, anos = 3) {
  fim <- as.numeric(format(Sys.Date(), "%Y")) - 1
  consultar(glue::glue("
    SELECT ano, SUBSTR(LPAD(CAST(id_ncm AS STRING), 8, '0'), 1, 4) AS sh4,
           CAST(id_via AS STRING) AS via, CAST(id_urf AS STRING) AS urf,
           SUM(SAFE_CAST(valor_fob_dolar AS FLOAT64)) AS valor_fob,
           SUM(SAFE_CAST(peso_liquido_kg AS FLOAT64)) AS kg
    FROM `basedosdados.br_me_comex_stat.ncm_exportacao`
    WHERE sigla_uf_ncm = '{uf}' AND ano BETWEEN {fim - anos + 1} AND {fim}
    GROUP BY 1, 2, 3, 4"), projeto) %>%
    mutate(ano = as.numeric(ano), via = as.character(as.integer(via)), urf = as.character(as.integer(urf)))
}

VIAS_COMEX <- c("0" = "Não declarada", "1" = "Marítima", "2" = "Fluvial", "3" = "Lacustre", "4" = "Aérea", "5" = "Postal",
                "6" = "Ferroviária", "7" = "Rodoviária", "8" = "Conduto ou rede de transmissão", "9" = "Meios próprios",
                "10" = "Entrada ou saída ficta", "11" = "Courier", "12" = "Em mãos", "13" = "Por reboque", "14" = "Dutos",
                "15" = "Vicinal fronteiriço")

# Nomes das unidades da Receita (URF) e das vias: dicionário do ComexStat na
# Base dos Dados; se faltar, tabelas auxiliares do MDIC na web.
obter_nomes_comex <- function(projeto) {
  d <- tentar("dicionário ComexStat", consultar("
    SELECT DISTINCT nome_coluna, chave, valor FROM `basedosdados.br_me_comex_stat.dicionario`
    WHERE nome_coluna IN ('id_urf', 'id_via')", projeto))
  urf <- if (NROW(d)) d %>% filter(nome_coluna == "id_urf") %>% transmute(urf = as.character(as.integer(chave)), nome = valor) else tibble()
  if (NROW(urf) < 10) {
    urf <- tryCatch({
      x <- utils::read.csv2("https://balanca.economia.gov.br/balanca/bd/tabelas/URF.csv", fileEncoding = "latin1", colClasses = "character")
      tibble(urf = as.character(as.integer(x$CO_URF)), nome = x$NO_URF)
    }, error = function(e) { msg("Nomes das URF indisponíveis: ", conditionMessage(e)); tibble(urf = character(), nome = character()) })
  }
  urf %>% distinct(urf, .keep_all = TRUE) %>% mutate(nome = stringr::str_to_title(stringr::str_squish(nome)))
}

escoamento_cadeia <- function(esc, sh4_map, nomes_urf) {
  if (!NROW(esc)) return(list(via = tibble(), urf = tibble()))
  ult <- max(esc$ano)
  base <- esc %>% filter(ano == ult) %>% inner_join(sh4_map %>% distinct(cadeia_id, sh4), by = "sh4", relationship = "many-to-many")
  via <- base %>% group_by(cadeia_id, ano, via) %>% summarise(valor_fob = sum(valor_fob, na.rm = TRUE), kg = sum(kg, na.rm = TRUE), .groups = "drop") %>%
    mutate(via_nome = coalesce(unname(VIAS_COMEX[via]), paste("Via", via))) %>%
    group_by(cadeia_id) %>% mutate(part = valor_fob / sum(valor_fob)) %>% ungroup()
  urf <- base %>% group_by(cadeia_id, ano, urf) %>% summarise(via = via[which.max(valor_fob)], valor_fob = sum(valor_fob, na.rm = TRUE),
                                                             kg = sum(kg, na.rm = TRUE), .groups = "drop") %>%
    left_join(nomes_urf, by = "urf") %>%
    mutate(nome = coalesce(nome, paste("URF", urf)), via_nome = coalesce(unname(VIAS_COMEX[via]), "")) %>%
    group_by(cadeia_id) %>% mutate(part = valor_fob / sum(valor_fob)) %>% arrange(desc(valor_fob), .by_group = TRUE) %>% ungroup()
  list(via = via, urf = urf)
}
