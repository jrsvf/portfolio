# =============================================================================
# Quociente locacional por mais de um indicador
#
# O QL do emprego formal (RAIS) deixa de fora a informalidade e não mede
# produção. Aqui ele ganha companhia, sempre na mesma fórmula
#   QL = (x da cadeia no município / x total do município) /
#        (x da cadeia em PE        / x total de PE)
# com x sendo:
#   - empresas ativas no CNPJ, contando MEI (capta o pequeno negócio, como na
#     confecção do Agreste);
#   - exportações (ComexStat por município do exportador);
#   - valor da produção agropecuária do IBGE (PAM, PPM, PEVS), só nas cadeias
#     do campo;
#   - valor adicionado dos 4 grandes setores do PIB dos Municípios (por município).
# Na comparação de PE com o Brasil, o QL da produção vem da matriz NEREUS.
# =============================================================================

ql_formula <- function(x_mc, x_m, x_c, x_t) ifelse(x_m > 0 & x_c > 0 & x_t > 0, (x_mc / x_m) / (x_c / x_t), NA_real_)

# Empresas ativas por cadeia e município, com e sem MEI
extrair_cnpj_cadeias <- function(projeto, uf, elos) {
  cods <- elos %>% distinct(cadeia_id, codigo)
  linhas <- paste0("STRUCT('", cods$cadeia_id, "' AS cadeia_id, '", cods$codigo, "' AS cod)", collapse = ", ")
  sql <- glue::glue("
    WITH cods AS (SELECT * FROM UNNEST([{linhas}])),
    e AS (
      SELECT cnpj_basico, id_municipio, cnae_fiscal_principal AS cnae
      FROM `basedosdados.br_me_cnpj.estabelecimentos`
      WHERE data = {SQL_CNPJ_ULT} AND sigla_uf = '{uf}' AND situacao_cadastral IN ('2', '02')),
    s AS (SELECT cnpj_basico, MAX(opcao_mei) AS mei FROM `basedosdados.br_me_cnpj.simples` GROUP BY 1),
    es AS (SELECT e.*, IFNULL(s.mei, 0) AS mei FROM e LEFT JOIN s USING (cnpj_basico))
    SELECT 'cadeia' AS nivel, c.cadeia_id, es.id_municipio, es.mei, COUNT(DISTINCT CONCAT(es.cnpj_basico, es.cnae)) AS n
    FROM es JOIN cods c ON (LENGTH(c.cod) = 7 AND es.cnae = c.cod) OR (LENGTH(c.cod) = 5 AND SUBSTR(es.cnae, 1, 5) = c.cod)
    GROUP BY 1, 2, 3, 4
    UNION ALL
    SELECT 'total' AS nivel, CAST(NULL AS STRING) AS cadeia_id, id_municipio, mei, COUNT(*) AS n FROM es GROUP BY 1, 2, 3, 4")
  consultar(sql, projeto) %>% mutate(n = as.numeric(n), mei = as.numeric(mei) == 1, id_municipio = as.character(id_municipio))
}

ql_empresas <- function(cnpj) {
  if (!NROW(cnpj)) return(tibble())
  cad <- cnpj %>% filter(nivel == "cadeia") %>% group_by(cadeia_id, id_municipio) %>%
    summarise(empresas = sum(n), mei = sum(n[mei]), .groups = "drop")
  tot <- cnpj %>% filter(nivel == "total") %>% group_by(id_municipio) %>% summarise(tot_m = sum(n), .groups = "drop")
  cad %>% left_join(tot, by = "id_municipio") %>% group_by(cadeia_id) %>% mutate(tot_c = sum(empresas)) %>% ungroup() %>%
    mutate(ql_empresas = ifelse(empresas >= 5, ql_formula(empresas, tot_m, tot_c, sum(tot$tot_m)), NA), pct_mei = ifelse(empresas > 0, mei / empresas, NA)) %>%
    select(cadeia_id, id_municipio, empresas, mei, pct_mei, ql_empresas)
}

ql_exportacoes <- function(comex_pe, comex_cad) {
  if (!NROW(comex_cad)) return(tibble())
  ult <- max(comex_cad$ano)
  tot <- comex_pe %>% filter(ano == ult, fluxo == "Exportação") %>% group_by(id_municipio) %>% summarise(tot_m = sum(valor_fob, na.rm = TRUE), .groups = "drop")
  comex_cad %>% filter(ano == ult, fluxo == "Exportação") %>% group_by(cadeia_id, id_municipio) %>%
    summarise(exportacoes = sum(valor_fob, na.rm = TRUE), .groups = "drop") %>%
    left_join(tot, by = "id_municipio") %>% group_by(cadeia_id) %>% mutate(tot_c = sum(exportacoes)) %>% ungroup() %>%
    mutate(ql_exportacoes = ql_formula(exportacoes, tot_m, tot_c, sum(tot$tot_m)),
           # município que quase não exporta gera QL enorme com valores pequenos: fica sem QL
           ql_exportacoes = ifelse(exportacoes >= 1e5 & tot_m >= 1e6, ql_exportacoes, NA), ano_exportacoes = ult) %>%
    select(cadeia_id, id_municipio, exportacoes, ql_exportacoes, ano_exportacoes)
}

# Valor da produção do IBGE. O total do município soma todos os produtos das
# tabelas baixadas (lavouras, produtos de origem animal, aquicultura e
# extrativismo/silvicultura), não só os da cadeia.
ql_producao_agro <- function(sidra_bruto, sidra_cad) {
  vp <- function(d) d %>% filter(variavel == "Valor da produção", !sidra_subtotais(produto), !is.na(valor))
  b <- vp(sidra_bruto); if (!nrow(b)) return(tibble())
  ult <- b %>% group_by(tabela) %>% summarise(ult = max(ano), .groups = "drop")
  b <- b %>% inner_join(ult, by = "tabela") %>% filter(ano == ult)
  tot <- b %>% distinct(tabela, id_municipio, produto, valor) %>% group_by(id_municipio) %>% summarise(tot_m = sum(valor), .groups = "drop")
  c <- vp(sidra_cad) %>% inner_join(ult, by = "tabela") %>% filter(ano == ult) %>%
    distinct(cadeia_id, tabela, id_municipio, produto, valor) %>%
    group_by(cadeia_id, id_municipio) %>% summarise(producao_mil = sum(valor), .groups = "drop")
  c %>% left_join(tot, by = "id_municipio") %>% group_by(cadeia_id) %>% mutate(tot_c = sum(producao_mil)) %>% ungroup() %>%
    mutate(ql_producao = ifelse(producao_mil >= 100, ql_formula(producao_mil, tot_m, tot_c, sum(tot$tot_m)), NA), ano_producao = max(ult$ult)) %>%
    select(cadeia_id, id_municipio, producao_mil, ql_producao, ano_producao)
}

# QL do valor adicionado de cada grande setor, por município (último ano com a divisão)
ql_vab_municipios <- function(pib_pop) {
  p <- pib_pop %>% filter(!is.na(va_industria))
  ult <- max(p$ano)
  p <- p %>% filter(ano == ult) %>% transmute(id_municipio, agro = va_agropecuaria, ind = va_industria, serv = va_servicos, adm = va_adespss)
  t <- colSums(p[, -1], na.rm = TRUE); T <- sum(t)
  p %>% mutate(tot = agro + ind + serv + adm,
               ql_agro = ql_formula(agro, tot, t[["agro"]], T), ql_ind = ql_formula(ind, tot, t[["ind"]], T),
               ql_serv = ql_formula(serv, tot, t[["serv"]], T), ql_adm = ql_formula(adm, tot, t[["adm"]], T), ano = ult) %>%
    select(id_municipio, ano, starts_with("ql_"))
}

# Tabela única: um registro por cadeia e município, com todos os QLs
montar_ql_multiplo <- function(locais_cadeia, empresas_cad, export_cad, producao_cad) {
  ult <- max(locais_cadeia$ano)
  base <- locais_cadeia %>% filter(ano == ult) %>% transmute(cadeia_id = atividade, id_municipio, emprego, ql_emprego = ql)
  junta <- function(a, b) if (NROW(b)) full_join(a, b, by = c("cadeia_id", "id_municipio")) else a
  out <- base %>% junta(empresas_cad) %>% junta(export_cad) %>% junta(producao_cad) %>% mutate(ano_emprego = ult)
  # quantos indicadores apontam especialização (QL >= 1)
  qls <- intersect(c("ql_emprego", "ql_empresas", "ql_exportacoes", "ql_producao"), names(out))
  out$n_ql_acima_1 <- rowSums(sapply(qls, function(k) coalesce(out[[k]] >= 1, FALSE)))
  out$n_ql_medidos <- rowSums(sapply(qls, function(k) !is.na(out[[k]])))
  out
}

# Leva os QLs para o raio-X de cada município (cartões das cadeias e bloco de produção)
adicionar_ql_raiox <- function(raiox, ql_mult, ql_vab) {
  r2 <- function(x) round(x, 2)
  cols <- c("ql_empresas", "pct_mei", "empresas", "ql_exportacoes", "ql_producao")
  for (k in setdiff(cols, names(ql_mult))) ql_mult[[k]] <- NA_real_
  qm <- ql_mult %>% select(id_municipio, cadeia_id, all_of(cols)) %>% mutate(across(c(ql_empresas, pct_mei, ql_exportacoes, ql_producao), r2))
  lapply(raiox, function(m) {
    v <- ql_vab %>% filter(id_municipio == m$id)
    if (nrow(v)) m$pib$ql_va <- list(ano = v$ano, agro = r2(v$ql_agro), ind = r2(v$ql_ind), serv = r2(v$ql_serv), adm = r2(v$ql_adm))
    if (NROW(m$cadeias)) m$cadeias <- m$cadeias %>% left_join(qm %>% filter(id_municipio == m$id) %>% select(-id_municipio), by = "cadeia_id")
    m
  })
}
