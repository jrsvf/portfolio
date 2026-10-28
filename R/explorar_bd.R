# Exploração (fora do pipeline): lista tabelas e colunas da Base dos Dados
# para os temas novos e grava em saidas/explorar_bd_*.csv
# Uso: Rscript R/explorar_bd.R   (na raiz do projeto)
suppressPackageStartupMessages({ library(bigrquery); library(dplyr) })
if (nzchar(Sys.getenv("GARGLE_EMAIL"))) options(gargle_oauth_email = Sys.getenv("GARGLE_EMAIL"))
bq_auth()
proj <- "analises1"
dir.create("saidas", showWarnings = FALSE)
q <- function(sql) tryCatch(bq_table_download(bq_project_query(proj, sql), bigint = "numeric"),
                            error = function(e) { message("ERRO: ", conditionMessage(e)); tibble(erro = conditionMessage(e)) })

# 1-2. colunas dos conjuntos candidatos (a listagem do projeto inteiro é negada)
dsets <- c("br_me_cnpj", "br_inep_censo_educacao_superior", "br_me_siconfi", "br_bcb_sicor", "br_ibge_censo_agropecuario",
           "br_rf_arrecadacao", "br_confaz", "br_inep_censo_escolar", "br_ibge_censo_2022", "br_me_rais", "br_anac_voos",
           "br_anac_aerodromos", "br_antaq_estatistico", "br_aneel_siga", "br_aneel_geracao", "br_ana_atlas_esgotos",
           "br_mme_energia", "br_bd_diretorios_brasil", "br_sefaz_pe", "br_stn_finbra", "br_mdic_cnae")
cols <- bind_rows(lapply(dsets, function(s)
  q(sprintf("SELECT table_schema, table_name, column_name, data_type FROM `basedosdados.%s.INFORMATION_SCHEMA.COLUMNS`", s)) %>%
    mutate(dataset = s, across(everything(), as.character))))
write.csv(cols, "saidas/explorar_bd_colunas.csv", row.names = FALSE)

# 3. amostras pequenas úteis
amostras <- list(
  cnpj_part = "SELECT MAX(data) AS ultima FROM `basedosdados.br_me_cnpj.estabelecimentos` WHERE data >= '2025-01-01'",
  siconfi_contas = "SELECT DISTINCT conta, portaria FROM `basedosdados.br_me_siconfi.municipio_receitas_orcamentarias` WHERE ano = 2023 AND sigla_uf = 'PE' AND LOWER(conta) LIKE '%icms%' LIMIT 50",
  siconfi_uf_icms = "SELECT DISTINCT conta FROM `basedosdados.br_me_siconfi.uf_receitas_orcamentarias` WHERE ano = 2023 AND sigla_uf = 'PE' AND LOWER(conta) LIKE '%icms%' LIMIT 50"
)
for (n in names(amostras)) write.csv(q(amostras[[n]]), sprintf("saidas/explorar_bd_%s.csv", n), row.names = FALSE)
message("pronto")
