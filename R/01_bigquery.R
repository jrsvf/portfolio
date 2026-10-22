# =============================================================================
# Extração via BigQuery (Base dos Dados), faturada no projeto `analises1`
# =============================================================================
# Fontes:
#   RAIS estabelecimentos  basedosdados.br_me_rais.microdados_estabelecimentos
#   RAIS vínculos          basedosdados.br_me_rais.microdados_vinculos
#   Novo CAGED             basedosdados.br_me_caged.microdados_movimentacao
#   ComexStat municipal    basedosdados.br_me_comex_stat.municipio_exportacao / _importacao
#   PIB municipal          basedosdados.br_ibge_pib.municipio
#   População              basedosdados.br_ibge_populacao.municipio
#   Diretórios             basedosdados.br_bd_diretorios_brasil.municipio / cnae_2
#
# Cuidados (já mapeados em trabalhos anteriores):
#   - quantidade_vinculos_ativos e vinculo_ativo_3112 são STRING -> SAFE_CAST / = '1'
#   - COUNT retorna integer64 -> as.numeric()
#   - CNAE é texto com zeros à esquerda -> nunca converter para número
# =============================================================================

suppressPackageStartupMessages(library(bigrquery))

autenticar_bq <- function() {
  cred <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if (nzchar(cred) && file.exists(cred)) {
    bigrquery::bq_auth(path = cred)          # GitHub Actions (conta de serviço)
  } else {
    bigrquery::bq_auth()                     # Uso local: abre o navegador
  }
  invisible(TRUE)
}

consultar <- function(sql, projeto) {
  msg("BigQuery: ", str_squish(substr(sql, 1, 90)), "...")
  tb <- bigrquery::bq_project_query(projeto, sql)
  df <- bigrquery::bq_table_download(tb, bigint = "numeric", page_size = 50000)
  tibble::as_tibble(df)
}

# Expressão SQL que devolve a SUBCLASSE CNAE (7 dígitos). Quando a subclasse
# vem vazia, usa a classe (cnae_2) completada com "00". A classe (5 dígitos)
# é derivada em R com substr(), assim as cadeias podem usar os dois níveis.
SQL_SUBCLASSE <- "LPAD(REGEXP_REPLACE(COALESCE(CAST(cnae_2_subclasse AS STRING), CONCAT(CAST(cnae_2 AS STRING), '00')), r'[^0-9]', ''), 7, '0')"
SQL_SUBCLASSE_CAGED <- "LPAD(REGEXP_REPLACE(CAST(cnae_2_subclasse AS STRING), r'[^0-9]', ''), 7, '0')"

com_classe <- function(df) df %>% mutate(classe = substr(subclasse, 1, 5))

# ---------------------------------------------------------------------------
# 0. Conferência de esquema: falha cedo e com mensagem clara se a Base dos
#    Dados renomear alguma coluna.
# ---------------------------------------------------------------------------
COLUNAS_ESPERADAS <- list(
  "br_me_rais.microdados_estabelecimentos" = c("ano", "sigla_uf", "id_municipio", "quantidade_vinculos_ativos", "cnae_2", "cnae_2_subclasse"),
  "br_me_rais.microdados_vinculos"         = c("ano", "sigla_uf", "id_municipio", "vinculo_ativo_3112", "valor_remuneracao_media", "sexo", "grau_instrucao_apos_2005", "cnae_2", "cnae_2_subclasse"),
  "br_me_caged.microdados_movimentacao"    = c("ano", "mes", "sigla_uf", "id_municipio", "cnae_2_subclasse", "saldo_movimentacao", "salario_mensal"),
  "br_me_comex_stat.municipio_exportacao"  = c("ano", "id_municipio", "sigla_uf", "id_sh4", "id_pais", "valor_fob_dolar", "peso_liquido_kg"),
  "br_me_comex_stat.municipio_importacao"  = c("ano", "id_municipio", "sigla_uf", "id_sh4", "id_pais", "valor_fob_dolar", "peso_liquido_kg"),
  "br_ibge_pib.municipio"                  = c("ano", "id_municipio", "pib", "va_agropecuaria", "va_industria", "va_servicos", "va_adespss"),
  "br_ibge_populacao.municipio"            = c("ano", "id_municipio", "populacao"),
  "br_bd_diretorios_brasil.cnae_2"         = c("subclasse", "descricao_subclasse", "classe", "descricao_classe", "divisao", "secao", "descricao_secao")
)

verificar_esquema <- function(projeto) {
  problemas <- character()
  for (tab in names(COLUNAS_ESPERADAS)) {
    ds <- strsplit(tab, ".", fixed = TRUE)[[1]]
    sql <- sprintf(
      "SELECT column_name FROM `basedosdados.%s.INFORMATION_SCHEMA.COLUMNS` WHERE table_name = '%s'",
      ds[1], ds[2])
    cols <- consultar(sql, projeto)$column_name
    falta <- setdiff(COLUNAS_ESPERADAS[[tab]], cols)
    if (length(falta) > 0) problemas <- c(problemas, sprintf("%s: faltam %s", tab, paste(falta, collapse = ", ")))
  }
  if (length(problemas) > 0) {
    stop("O esquema da Base dos Dados mudou. Ajuste R/01_bigquery.R:\n  ",
         paste(problemas, collapse = "\n  "), call. = FALSE)
  }
  msg("Esquema conferido: todas as colunas esperadas existem.")
  TRUE
}

# ---------------------------------------------------------------------------
# 1. RAIS: emprego e estabelecimentos por município x classe x ano (PE)
# ---------------------------------------------------------------------------
extrair_rais_estab_uf <- function(projeto, uf, ano_ini) {
  sql <- glue::glue("
    SELECT ano, id_municipio, {SQL_SUBCLASSE} AS subclasse,
           SUM(SAFE_CAST(quantidade_vinculos_ativos AS INT64)) AS vinculos,
           COUNTIF(SAFE_CAST(quantidade_vinculos_ativos AS INT64) > 0) AS estabelecimentos
    FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini}
    GROUP BY 1, 2, 3")
  consultar(sql, projeto) %>%
    mutate(across(c(ano, vinculos, estabelecimentos), as.numeric),
           id_municipio = as.character(id_municipio)) %>% com_classe()
}

# Todas as UFs agregadas por classe (referência Brasil/Nordeste e shift-share)
extrair_rais_estab_brasil <- function(projeto, ano_ini) {
  sql <- glue::glue("
    SELECT ano, sigla_uf, {SQL_SUBCLASSE} AS subclasse,
           SUM(SAFE_CAST(quantidade_vinculos_ativos AS INT64)) AS vinculos,
           COUNTIF(SAFE_CAST(quantidade_vinculos_ativos AS INT64) > 0) AS estabelecimentos
    FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
    WHERE ano >= {ano_ini}
    GROUP BY 1, 2, 3")
  consultar(sql, projeto) %>%
    mutate(across(c(ano, vinculos, estabelecimentos), as.numeric)) %>% com_classe()
}

# Perfil dos vínculos ativos em 31/12: remuneração, sexo, escolaridade
extrair_rais_perfil_uf <- function(projeto, uf, ano_ini) {
  sql <- glue::glue("
    SELECT ano, id_municipio, {SQL_SUBCLASSE} AS subclasse,
           COUNT(*) AS vinculos_ativos,
           AVG(SAFE_CAST(valor_remuneracao_media AS FLOAT64)) AS remuneracao_media,
           AVG(IF(CAST(sexo AS STRING) = '2', 1, 0)) AS pct_mulheres,
           AVG(IF(SAFE_CAST(grau_instrucao_apos_2005 AS INT64) >= 9, 1, 0)) AS pct_superior
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini} AND CAST(vinculo_ativo_3112 AS STRING) = '1'
    GROUP BY 1, 2, 3")
  consultar(sql, projeto) %>%
    mutate(across(c(ano, vinculos_ativos, remuneracao_media, pct_mulheres, pct_superior), as.numeric),
           id_municipio = as.character(id_municipio)) %>% com_classe()
}

# ---------------------------------------------------------------------------
# 2. Novo CAGED: saldo mensal por município x classe
# ---------------------------------------------------------------------------
extrair_caged_uf <- function(projeto, uf, ano_ini) {
  sql <- glue::glue("
    SELECT ano, mes, id_municipio, {SQL_SUBCLASSE_CAGED} AS subclasse,
           SUM(SAFE_CAST(saldo_movimentacao AS INT64)) AS saldo,
           COUNTIF(SAFE_CAST(saldo_movimentacao AS INT64) > 0) AS admissoes,
           COUNTIF(SAFE_CAST(saldo_movimentacao AS INT64) < 0) AS desligamentos,
           AVG(IF(SAFE_CAST(saldo_movimentacao AS INT64) > 0, SAFE_CAST(salario_mensal AS FLOAT64), NULL)) AS salario_admissao
    FROM `basedosdados.br_me_caged.microdados_movimentacao`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini}
    GROUP BY 1, 2, 3, 4")
  consultar(sql, projeto) %>%
    mutate(across(c(ano, mes, saldo, admissoes, desligamentos, salario_admissao), as.numeric),
           id_municipio = as.character(id_municipio)) %>% com_classe()
}

# ---------------------------------------------------------------------------
# 3. ComexStat por município (domicílio fiscal da empresa) x SH4 x país
# ---------------------------------------------------------------------------
extrair_comex_uf <- function(projeto, uf, ano_ini) {
  um_fluxo <- function(tabela, fluxo) {
    sql <- glue::glue("
      SELECT ano, id_municipio, LPAD(CAST(id_sh4 AS STRING), 4, '0') AS sh4,
             CAST(id_pais AS STRING) AS id_pais,
             SUM(SAFE_CAST(valor_fob_dolar AS FLOAT64)) AS valor_fob,
             SUM(SAFE_CAST(peso_liquido_kg AS FLOAT64)) AS kg
      FROM `basedosdados.br_me_comex_stat.{tabela}`
      WHERE sigla_uf = '{uf}' AND ano >= {ano_ini}
      GROUP BY 1, 2, 3, 4")
    consultar(sql, projeto) %>% mutate(fluxo = fluxo)
  }
  bind_rows(um_fluxo("municipio_exportacao", "Exportação"),
            um_fluxo("municipio_importacao", "Importação")) %>%
    mutate(ano = as.numeric(ano), id_municipio = as.character(id_municipio))
}

# ---------------------------------------------------------------------------
# 4. PIB municipal, população e diretórios
# ---------------------------------------------------------------------------
extrair_pib_pop_uf <- function(projeto, uf) {
  sql <- glue::glue("
    SELECT p.ano, p.id_municipio, p.pib, p.va_agropecuaria, p.va_industria,
           p.va_servicos, p.va_adespss, pop.populacao
    FROM `basedosdados.br_ibge_pib.municipio` p
    JOIN `basedosdados.br_bd_diretorios_brasil.municipio` d USING (id_municipio)
    LEFT JOIN `basedosdados.br_ibge_populacao.municipio` pop
      ON pop.id_municipio = p.id_municipio AND pop.ano = p.ano
    WHERE d.sigla_uf = '{uf}'")
  consultar(sql, projeto) %>%
    mutate(across(-id_municipio, as.numeric), id_municipio = as.character(id_municipio))
}

extrair_municipios <- function(projeto, uf) {
  sql <- glue::glue("
    SELECT id_municipio, nome AS municipio, sigla_uf
    FROM `basedosdados.br_bd_diretorios_brasil.municipio`
    WHERE sigla_uf = '{uf}'")
  consultar(sql, projeto) %>% mutate(id_municipio = as.character(id_municipio))
}

extrair_cnae <- function(projeto) {
  sql <- "
    SELECT DISTINCT subclasse, descricao_subclasse, classe, descricao_classe,
           divisao, descricao_divisao, secao, descricao_secao
    FROM `basedosdados.br_bd_diretorios_brasil.cnae_2`"
  consultar(sql, projeto) %>%
    mutate(subclasse = normalizar_cnae(subclasse, 7),
           classe = normalizar_cnae(classe, 5),
           divisao = normalizar_cnae(divisao, 2))
}

extrair_paises <- function(projeto) {
  sql <- "
    SELECT CAST(id_pais AS STRING) AS id_pais, nome AS pais
    FROM `basedosdados.br_bd_diretorios_mundo.pais`"
  tryCatch(consultar(sql, projeto),
           error = function(e) {
             msg("Aviso: diretório de países indisponível (", conditionMessage(e), "). Usando códigos.")
             tibble(id_pais = character(), pais = character())
           })
}

# Nomes dos países no padrão do ComexStat (CO_PAIS). Fonte principal: o
# dicionário do próprio ComexStat na Base dos Dados (mesma autenticação do
# resto do pipeline). Se falhar, tenta a tabela auxiliar do MDIC na web.
obter_paises <- function(projeto = NULL) {
  if (!is.null(projeto)) {
    df <- tryCatch(consultar("
      SELECT DISTINCT chave AS id_pais, valor AS pais
      FROM `basedosdados.br_me_comex_stat.dicionario`
      WHERE nome_coluna = 'id_pais' AND id_tabela = 'municipio_exportacao'", projeto),
      error = function(e) { msg("Dicionário de países indisponível: ", conditionMessage(e)); NULL })
    if (!is.null(df) && nrow(df) > 100) {
      msg("Países: ", nrow(df), " nomes carregados do dicionário do ComexStat.")
      df <- df %>% mutate(id_pais = as.character(as.integer(id_pais))) %>% distinct(id_pais, .keep_all = TRUE)
      return(adicionar_sigla(df, projeto))
    }
  }
  urls <- c("https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv",
            "https://api-comexstat.mdic.gov.br/tables/countries")
  for (u in urls) {
    df <- tryCatch({
      if (grepl("\\.csv$", u)) {
        x <- utils::read.csv2(u, fileEncoding = "latin1", colClasses = "character")
        tibble(id_pais = as.character(as.integer(x$CO_PAIS)), pais = x$NO_PAIS)
      } else {
        x <- jsonlite::fromJSON(u)$data
        tibble(id_pais = as.character(as.integer(x$id)), pais = x$text)
      }
    }, error = function(e) { msg("Tabela de países indisponível em ", u, ": ", conditionMessage(e)); NULL })
    if (!is.null(df) && nrow(df) > 100) { msg("Países: ", nrow(df), " nomes carregados."); return(df) }
  }
  msg("Aviso: nomes de países indisponíveis; os gráficos mostrarão o código do país.")
  tibble(id_pais = character(), pais = character())
}

# Sigla ISO de 3 letras (CHN, USA, NLD...) para os rótulos dos gráficos. O
# dicionário do ComexStat só traz o nome (sem acento), então o casamento com o
# diretório de países da Base dos Dados é feito pelo nome normalizado.
normalizar_nome <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^A-Za-z ]", " ", x))
  gsub("\\s+", " ", trimws(x))
}

adicionar_sigla <- function(df, projeto) {
  dir <- tryCatch(consultar("
    SELECT nome_pt, nome_en, sigla_iso3 FROM `basedosdados.br_bd_diretorios_mundo.pais`
    WHERE sigla_iso3 IS NOT NULL", projeto), error = function(e) NULL)
  if (is.null(dir)) return(df %>% mutate(sigla = toupper(substr(normalizar_nome(pais), 1, 3))))
  chaves <- bind_rows(dir %>% transmute(chave = normalizar_nome(nome_pt), sigla_iso3),
                      dir %>% transmute(chave = normalizar_nome(nome_en), sigla_iso3)) %>%
    filter(!is.na(chave)) %>% distinct(chave, .keep_all = TRUE)
  df %>%
    mutate(chave = normalizar_nome(pais)) %>%
    left_join(chaves, by = "chave") %>%
    mutate(sigla = coalesce(sigla_iso3, toupper(substr(chave, 1, 3)))) %>%
    select(id_pais, pais, sigla)
}
