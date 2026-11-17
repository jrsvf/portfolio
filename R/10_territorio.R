# =============================================================================
# Território: empresas (CNPJ), qualificação, ensino superior e técnico, ICMS,
# agricultura familiar, crédito rural (PRONAF), infraestrutura e o corte de 2026
# =============================================================================
# Fontes (Base dos Dados, faturado no projeto do usuário):
#   br_rf_cnpj.estabelecimentos / empresas / simples   (Receita Federal)
#   br_me_rais.microdados_vinculos                     (escolaridade e ocupação)
#   br_inep_censo_educacao_superior.curso / ies
#   br_inep_censo_escolar.turma                        (educação profissional)
#   br_me_siconfi.municipio_receitas_orcamentarias / uf_receitas_orcamentarias
#   br_bcb_sicor.operacao + recurso_publico_complemento_operacao
# SIDRA: Censo Agropecuário 2017 (tabelas 6753 e 6898, tipologia familiar)
#
# Cuidado com dados pessoais: a lista de empresas exclui MEI e empresário
# individual (a razão social dessas formas é o nome da pessoa) e qualquer
# razão social que contenha um número com cara de CPF. Sócios não são lidos.
# =============================================================================

tentar <- function(rotulo, expr) {
  tryCatch(expr, error = function(e) { msg(rotulo, " falhou: ", conditionMessage(e)); tibble::tibble() })
}

# Dicionários da Base dos Dados (códigos -> rótulos)
extrair_dicionarios_bd <- function(projeto) {
  ds <- c("br_rf_cnpj", "br_inep_censo_educacao_superior", "br_inep_censo_escolar", "br_bcb_sicor", "br_me_rais")
  bind_rows(lapply(ds, function(d) tentar(paste("dicionário", d), consultar(glue::glue(
    "SELECT '{d}' AS dataset, id_tabela, nome_coluna, chave, valor FROM `basedosdados.{d}.dicionario`"), projeto))))
}

rotular <- function(x, dic, dataset, tabela, coluna) {
  d <- dic %>% filter(.data$dataset == !!dataset, id_tabela == tabela, nome_coluna == coluna) %>% distinct(chave, .keep_all = TRUE)
  if (!nrow(d)) return(x)
  y <- d$valor[match(as.character(x), d$chave)]
  ifelse(is.na(y), as.character(x), y)
}

# ---------------------------------------------------------------------------
# 1. Empresas (CNPJ): aberturas, baixas e estoque por município x divisão CNAE
# ---------------------------------------------------------------------------
# Em novembro de 2026 a Base dos Dados mudou o CNPJ de br_me_cnpj para br_rf_cnpj (coluna
# data virou data_referencia); as tabelas antigas ficaram só com um aviso do novo endereço.
SQL_CNPJ_ULT <- "(SELECT MAX(data_referencia) FROM `basedosdados.br_rf_cnpj.estabelecimentos` WHERE data_referencia >= DATE_SUB(CURRENT_DATE(), INTERVAL 540 DAY))"

extrair_cnpj_fluxos <- function(projeto, uf, ano_ini = 2010) {
  sql <- glue::glue("
    WITH e AS (
      SELECT id_municipio, SUBSTR(cnae_fiscal_principal, 1, 2) AS divisao, cnpj_basico, situacao_cadastral,
             data_inicio_atividade, data_situacao_cadastral, data_referencia AS data
      FROM `basedosdados.br_rf_cnpj.estabelecimentos`
      WHERE data_referencia = {SQL_CNPJ_ULT} AND sigla_uf = '{uf}'),
    s AS (SELECT cnpj_basico, MAX(opcao_mei) AS mei FROM `basedosdados.br_rf_cnpj.simples` GROUP BY 1)
    SELECT 'abertura' AS tipo, e.id_municipio, e.divisao, IFNULL(s.mei, 0) AS mei,
           EXTRACT(YEAR FROM e.data_inicio_atividade) AS ano, e.situacao_cadastral, MAX(e.data) AS data_base, COUNT(*) AS n
    FROM e LEFT JOIN s USING (cnpj_basico)
    WHERE e.data_inicio_atividade >= '{ano_ini}-01-01'
    GROUP BY 1, 2, 3, 4, 5, 6
    UNION ALL
    SELECT 'situacao' AS tipo, e.id_municipio, e.divisao, IFNULL(s.mei, 0) AS mei,
           EXTRACT(YEAR FROM e.data_situacao_cadastral) AS ano, e.situacao_cadastral, MAX(e.data) AS data_base, COUNT(*) AS n
    FROM e LEFT JOIN s USING (cnpj_basico)
    GROUP BY 1, 2, 3, 4, 5, 6")
  consultar(sql, projeto) %>% mutate(across(c(mei, ano, n), as.numeric), id_municipio = as.character(id_municipio))
}

# Maiores empresas (por capital social) de cada cadeia em cada município.
# O capital social é da empresa inteira e se repete em cada filial. Por isso
# cada empresa (raiz do CNPJ) entra uma vez só por cadeia e município, e uma vez
# só no ranking estadual: fica a matriz, se houver, ou a unidade mais antiga, e
# `n_unidades` guarda quantos estabelecimentos ativos ela tem ali.
extrair_cnpj_maiores <- function(projeto, uf, elos, top = 5) {
  cods <- elos %>% distinct(cadeia_id, codigo)
  linhas <- paste0("STRUCT('", cods$cadeia_id, "' AS cadeia_id, '", cods$codigo, "' AS cod)", collapse = ", ")
  sql <- glue::glue("
    WITH cods AS (SELECT * FROM UNNEST([{linhas}])),
    e AS (
      SELECT cnpj, cnpj_basico, id_municipio, cnae_fiscal_principal AS cnae, data_inicio_atividade,
             identificador_matriz_filial AS matriz_filial, nome_fantasia, situacao_cadastral
      FROM `basedosdados.br_rf_cnpj.estabelecimentos`
      WHERE data_referencia = {SQL_CNPJ_ULT} AND sigla_uf = '{uf}' AND situacao_cadastral IN ('2', '02')),
    emp AS (
      SELECT cnpj_basico, razao_social, natureza_juridica, capital_social, porte
      FROM `basedosdados.br_rf_cnpj.empresas`
      WHERE data_referencia = (SELECT MAX(data_referencia) FROM `basedosdados.br_rf_cnpj.empresas` WHERE data_referencia >= DATE_SUB(CURRENT_DATE(), INTERVAL 540 DAY))),
    s AS (SELECT cnpj_basico, MAX(opcao_mei) AS mei FROM `basedosdados.br_rf_cnpj.simples` GROUP BY 1),
    j AS (
      SELECT c.cadeia_id, e.*, emp.razao_social, emp.natureza_juridica, emp.capital_social, emp.porte
      FROM e
      JOIN cods c ON (LENGTH(c.cod) = 7 AND e.cnae = c.cod) OR (LENGTH(c.cod) = 5 AND SUBSTR(e.cnae, 1, 5) = c.cod)
      JOIN emp USING (cnpj_basico)
      LEFT JOIN s USING (cnpj_basico)
      WHERE IFNULL(s.mei, 0) != 1 AND emp.natureza_juridica NOT IN ('2135', '4014', '4080', '4090', '4120')),
    u AS (
      SELECT j.*,
             COUNT(*) OVER (PARTITION BY cadeia_id, id_municipio, cnpj_basico) AS n_unidades,
             COUNT(*) OVER (PARTITION BY cadeia_id, cnpj_basico) AS n_unidades_uf,
             ROW_NUMBER() OVER (PARTITION BY cadeia_id, id_municipio, cnpj_basico
                                ORDER BY IF(matriz_filial IN ('1', '01'), 0, 1), data_inicio_atividade, cnpj) AS ord_mun,
             ROW_NUMBER() OVER (PARTITION BY cadeia_id, cnpj_basico
                                ORDER BY IF(matriz_filial IN ('1', '01'), 0, 1), data_inicio_atividade, cnpj) AS ord_uf
      FROM j),
    rm AS (
      SELECT cadeia_id, cnpj, ROW_NUMBER() OVER (PARTITION BY cadeia_id, id_municipio ORDER BY capital_social DESC, data_inicio_atividade, cnpj) AS rk_mun
      FROM u WHERE ord_mun = 1),
    ru AS (
      SELECT cadeia_id, cnpj, ROW_NUMBER() OVER (PARTITION BY cadeia_id ORDER BY capital_social DESC, data_inicio_atividade, cnpj) AS rk_uf
      FROM u WHERE ord_uf = 1)
    SELECT u.* EXCEPT (ord_mun, ord_uf), rm.rk_mun, ru.rk_uf
    FROM u
    LEFT JOIN rm ON rm.cadeia_id = u.cadeia_id AND rm.cnpj = u.cnpj
    LEFT JOIN ru ON ru.cadeia_id = u.cadeia_id AND ru.cnpj = u.cnpj
    WHERE rm.rk_mun <= {top} OR ru.rk_uf <= 15")
  consultar(sql, projeto) %>%
    mutate(across(c(capital_social, rk_mun, rk_uf, n_unidades, n_unidades_uf), as.numeric), id_municipio = as.character(id_municipio)) %>%
    # nada que pareça nome de pessoa com CPF
    filter(!grepl("[0-9]{11}|[0-9]{3}\\.[0-9]{3}\\.[0-9]{3}-?[0-9]{2}", razao_social))
}

montar_empresas <- function(fluxos, dic, dir_cnae) {
  if (!NROW(fluxos)) return(list(fluxo = tibble(), estoque = tibble(), data_base = NA))
  sit <- function(x) {
    r <- tolower(rotular(x, dic, "br_rf_cnpj", "estabelecimentos", "situacao_cadastral"))
    case_when(grepl("ativa", r) | x %in% c("2", "02") ~ "ativa",
              grepl("baixada", r) | x %in% c("8", "08") ~ "baixada",
              TRUE ~ "outra")
  }
  secoes <- dir_cnae %>% distinct(divisao, secao, descricao_secao)
  f <- fluxos %>% mutate(sit = sit(situacao_cadastral), mei = mei == 1) %>%
    left_join(secoes, by = "divisao")
  aberturas <- f %>% filter(tipo == "abertura") %>% group_by(id_municipio, ano, secao, mei) %>% summarise(aberturas = sum(n), .groups = "drop")
  baixas <- f %>% filter(tipo == "situacao", sit == "baixada", ano >= min(aberturas$ano)) %>%
    group_by(id_municipio, ano, secao, mei) %>% summarise(baixas = sum(n), .groups = "drop")
  fluxo <- full_join(aberturas, baixas, by = c("id_municipio", "ano", "secao", "mei")) %>%
    mutate(across(c(aberturas, baixas), ~ coalesce(.x, 0)))
  estoque <- f %>% filter(tipo == "situacao", sit == "ativa") %>% group_by(id_municipio, secao, mei) %>%
    summarise(ativas = sum(n), .groups = "drop")
  list(fluxo = fluxo, estoque = estoque, data_base = max(as.Date(fluxos$data_base), na.rm = TRUE),
       secoes = secoes %>% distinct(secao, descricao_secao))
}

montar_maiores <- function(maiores, dic, dir_cnae) {
  if (!NROW(maiores)) return(tibble())
  porte_txt <- function(x) {
    r <- rotular(x, dic, "br_rf_cnpj", "empresas", "porte")
    recode(r, "01" = "Microempresa", "1" = "Microempresa", "03" = "Pequeno porte", "3" = "Pequeno porte",
           "05" = "Demais", "5" = "Demais", "00" = "Não informado", "0" = "Não informado")
  }
  maiores %>%
    uma_por_empresa() %>%
    mutate(atividade = dir_cnae$descricao_subclasse[match(cnae, dir_cnae$subclasse)],
           porte = porte_txt(porte),
           matriz_filial = ifelse(matriz_filial %in% c("1", "01"), "Matriz", "Filial"),
           abertura = as.numeric(format(as.Date(data_inicio_atividade), "%Y")),
           razao_social = stringr::str_squish(razao_social),
           nome_fantasia = na_if(stringr::str_squish(nome_fantasia), "")) %>%
    select(cadeia_id, id_municipio, cnpj, razao_social, nome_fantasia, cnae, atividade, porte, matriz_filial,
           abertura, capital_social, n_unidades, n_unidades_uf, rk_mun, rk_uf)
}

# Rede de segurança contra empresas repetidas: uma linha por empresa (raiz do
# CNPJ) em cada cadeia e município, e uma por cadeia no ranking estadual. A
# consulta nova já entrega assim; isto cobre a base antiga, ainda em cache, em
# que cada filial vinha em uma linha (a contagem de unidades, nesse caso, é só
# a das que estavam entre as primeiras).
uma_por_empresa <- function(m) {
  if ("n_unidades" %in% names(m)) return(m)
  filial <- function(x) !x %in% c("1", "01")
  m <- m %>% mutate(cnpj_basico = substr(cnpj, 1, 8)) %>%
    group_by(cadeia_id, id_municipio, cnpj_basico) %>% mutate(n_unidades = n()) %>%
    group_by(cadeia_id, cnpj_basico) %>% mutate(n_unidades_uf = n()) %>% ungroup()
  mun <- m %>% filter(!is.na(rk_mun)) %>% arrange(filial(matriz_filial), data_inicio_atividade, cnpj) %>%
    distinct(cadeia_id, id_municipio, cnpj_basico, .keep_all = TRUE) %>%
    group_by(cadeia_id, id_municipio) %>% arrange(desc(capital_social), data_inicio_atividade, cnpj, .by_group = TRUE) %>%
    mutate(rk_mun = row_number()) %>% ungroup() %>% select(cadeia_id, cnpj, rk_mun)
  uf <- m %>% filter(!is.na(rk_uf)) %>% arrange(filial(matriz_filial), data_inicio_atividade, cnpj) %>%
    distinct(cadeia_id, cnpj_basico, .keep_all = TRUE) %>%
    group_by(cadeia_id) %>% arrange(desc(capital_social), data_inicio_atividade, cnpj, .by_group = TRUE) %>%
    mutate(rk_uf = row_number()) %>% ungroup() %>% select(cadeia_id, cnpj, rk_uf)
  m %>% select(-rk_mun, -rk_uf) %>% left_join(mun, by = c("cadeia_id", "cnpj")) %>% left_join(uf, by = c("cadeia_id", "cnpj")) %>%
    filter(!is.na(rk_mun) | !is.na(rk_uf))
}

# ---------------------------------------------------------------------------
# 2. Qualificação: escolaridade e grande grupo ocupacional (RAIS vínculos)
# ---------------------------------------------------------------------------
extrair_rais_qualif <- function(projeto, uf, anos_q) {
  sql <- glue::glue("
    SELECT ano, id_municipio, grau_instrucao_apos_2005 AS grau, SUBSTR(cbo_2002, 1, 1) AS cbo_gg, COUNT(*) AS vinculos
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = '{uf}' AND ano IN ({paste(anos_q, collapse = ', ')}) AND vinculo_ativo_3112 = '1'
    GROUP BY 1, 2, 3, 4")
  consultar(sql, projeto) %>% mutate(across(c(ano, vinculos), as.numeric), id_municipio = as.character(id_municipio))
}

ESCOLARIDADE <- c("1" = "Até fundamental", "2" = "Até fundamental", "3" = "Até fundamental", "4" = "Até fundamental",
                  "5" = "Até fundamental", "6" = "Médio", "7" = "Médio", "8" = "Superior", "9" = "Superior",
                  "10" = "Pós-graduação", "11" = "Pós-graduação")
CBO_GG <- c("0" = "Forças armadas e policiais", "1" = "Dirigentes e gerentes", "2" = "Profissionais das ciências e das artes",
            "3" = "Técnicos de nível médio", "4" = "Serviços administrativos", "5" = "Serviços e comércio",
            "6" = "Agropecuária, florestal e pesca", "7" = "Produção industrial", "8" = "Produção industrial",
            "9" = "Reparação e manutenção")

montar_qualif <- function(q) {
  if (!NROW(q)) return(tibble())
  q %>% mutate(grau = sub("^0+", "", as.character(grau)),
               escolaridade = coalesce(ESCOLARIDADE[grau], "Não informado"),
               ocupacao = coalesce(CBO_GG[cbo_gg], "Não informado")) %>%
    group_by(ano, id_municipio, escolaridade, ocupacao) %>% summarise(vinculos = sum(vinculos), .groups = "drop")
}

# ---------------------------------------------------------------------------
# 3. Ensino superior (INEP) e educação profissional (Censo Escolar)
# ---------------------------------------------------------------------------
extrair_superior <- function(projeto, uf, ano_ini) {
  cursos <- consultar(glue::glue("
    SELECT ano, id_municipio, id_ies, nome_area_geral AS area, tipo_organizacao_administrativa AS adm,
           tipo_modalidade_ensino AS modalidade, tipo_nivel_academico AS nivel,
           COUNT(*) AS cursos, SUM(quantidade_vagas) AS vagas, SUM(quantidade_matriculas) AS matriculas,
           SUM(quantidade_concluintes) AS concluintes, SUM(quantidade_ingressantes) AS ingressantes
    FROM `basedosdados.br_inep_censo_educacao_superior.curso`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini}
    GROUP BY 1, 2, 3, 4, 5, 6, 7"), projeto) %>%
    mutate(across(c(ano, cursos, vagas, matriculas, concluintes, ingressantes), as.numeric))
  ies <- consultar(glue::glue("
    SELECT ano, id_ies, nome, sigla, id_municipio, tipo_organizacao_academica AS organizacao,
           tipo_categoria_administrativa AS categoria
    FROM `basedosdados.br_inep_censo_educacao_superior.ies`
    WHERE sigla_uf = '{uf}' AND ano = (SELECT MAX(ano) FROM `basedosdados.br_inep_censo_educacao_superior.ies`)"), projeto)
  list(cursos = cursos, ies = ies)
}

extrair_tecnico <- function(projeto, uf, ano_ini) {
  consultar(glue::glue("
    SELECT ano, id_municipio, rede, COUNT(DISTINCT id_escola) AS escolas, SUM(quantidade_matriculas) AS matriculas
    FROM `basedosdados.br_inep_censo_escolar.turma`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini} AND id_curso_educacao_profissional IS NOT NULL
      AND TRIM(CAST(id_curso_educacao_profissional AS STRING)) NOT IN ('', '0')
    GROUP BY 1, 2, 3"), projeto) %>% mutate(across(c(ano, escolas, matriculas), as.numeric))
}

montar_educacao <- function(sup, tec, dic) {
  rot <- function(x, tab, col) rotular(x, dic, "br_inep_censo_educacao_superior", tab, col)
  cursos <- if (NROW(sup$cursos)) sup$cursos %>% mutate(
    adm = rot(adm, "curso", "tipo_organizacao_administrativa"),
    modalidade = rot(modalidade, "curso", "tipo_modalidade_ensino"),
    nivel = rot(nivel, "curso", "tipo_nivel_academico"),
    rede = ifelse(grepl("p[úu]blica|federal|estadual|municipal|especial", adm, ignore.case = TRUE), "Pública", "Privada")) else tibble()
  ies <- if (NROW(sup$ies)) sup$ies %>% mutate(
    organizacao = trimws(rot(organizacao, "ies", "tipo_organizacao_academica")),
    categoria = rot(categoria, "ies", "tipo_categoria_administrativa")) else tibble()
  tecnico <- if (NROW(tec)) tec %>% mutate(rede = rotular(rede, dic, "br_inep_censo_escolar", "turma", "rede")) else tibble()
  list(cursos = cursos, ies = ies, tecnico = tecnico)
}

# ---------------------------------------------------------------------------
# 4. ICMS: cota-parte municipal (SICONFI) e arrecadação dos estados do NE
# ---------------------------------------------------------------------------
UFS_NE <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")

extrair_icms <- function(projeto, uf) {
  mun <- consultar(glue::glue("
    SELECT ano, id_municipio, estagio, conta, SUM(valor) AS valor
    FROM `basedosdados.br_me_siconfi.municipio_receitas_orcamentarias`
    WHERE sigla_uf = '{uf}' AND (LOWER(conta) LIKE '%cota-parte do icms%' OR LOWER(conta_bd) LIKE '%cota-parte do icms%')
    GROUP BY 1, 2, 3, 4"), projeto)
  est <- consultar(glue::glue("
    SELECT ano, sigla_uf, estagio, conta, conta_bd, SUM(valor) AS valor
    FROM `basedosdados.br_me_siconfi.uf_receitas_orcamentarias`
    WHERE sigla_uf IN ({paste0(\"'\", UFS_NE, \"'\", collapse = ', ')})
      AND (LOWER(conta) LIKE '%icms%' OR LOWER(conta) LIKE '%circula%o de mercadorias%' OR LOWER(conta_bd) LIKE '%icms%'
           OR LOWER(conta_bd) LIKE '%circula%o de mercadorias%')
    GROUP BY 1, 2, 3, 4, 5"), projeto)
  list(mun = mun %>% mutate(ano = as.numeric(ano), valor = as.numeric(valor)),
       uf = est %>% mutate(ano = as.numeric(ano), valor = as.numeric(valor)))
}

# Escolhe o estágio "realizado" (bruto) e a linha principal do ICMS. O SICONFI
# muda nomes de conta e de estágio ao longo dos anos: até 2017 as contas vêm
# sem estágio ("CPICMS", "Cota ICMS"); depois, "Receitas Brutas Realizadas".
montar_icms <- function(icms) {
  prio <- function(e) case_when(e == "Receitas Brutas Realizadas" ~ 1, e == "Receitas Realizadas" ~ 2, e == "" | is.na(e) ~ 3, TRUE ~ 9)
  mun <- if (NROW(icms$mun)) icms$mun %>% mutate(p = prio(estagio)) %>% filter(p < 9) %>%
    filter(!grepl("compensa|lc 194|lc n", conta, ignore.case = TRUE)) %>%
    group_by(ano, id_municipio) %>% filter(p == min(p)) %>%
    summarise(cota_icms = sum(valor, na.rm = TRUE), .groups = "drop") %>% filter(cota_icms > 0) else tibble()
  uf <- if (NROW(icms$uf)) icms$uf %>% mutate(p = prio(estagio)) %>% filter(p < 9) %>%
    filter(grepl("^Imposto sobre Opera.*Circula|^ICMS$|^ICMS -? ?Principal|^Imposto sobre Circula", conta, ignore.case = TRUE)) %>%
    group_by(ano, sigla_uf) %>% filter(p == min(p)) %>%
    summarise(icms = max(valor, na.rm = TRUE), conta = conta[which.max(valor)], .groups = "drop") else tibble()
  list(mun = mun, uf = uf, contas_uf = if (NROW(icms$uf)) distinct(icms$uf, estagio, conta) else tibble())
}

# ---------------------------------------------------------------------------
# 5. Agricultura familiar (Censo Agro 2017) e PRONAF (SICOR)
# ---------------------------------------------------------------------------
obter_agro_familiar <- function(uf_codigo = 26) {
  ler <- function(url) {
    raw <- tryCatch(sidra_baixar(url), error = function(e) { msg("Censo Agro: ", conditionMessage(e)); NULL })
    if (is.null(raw) || NROW(raw) < 2) return(tibble())
    cab <- as.character(raw[1, ]); df <- raw[-1, , drop = FALSE]
    col_mun <- names(raw)[which(cab == "Município (Código)")[1]]
    col_var <- names(raw)[which(cab == "Variável")[1]]
    col_tip <- names(raw)[which(cab == "Tipologia")[1]]
    tibble(id_municipio = df[[col_mun]], variavel = df[[col_var]], tipologia = df[[col_tip]],
           valor = suppressWarnings(as.numeric(df$V)))
  }
  a <- ler(sprintf("https://apisidra.ibge.gov.br/values/t/6753/n6/in%%20n3%%20%s/v/183,184/p/all/c829/46303,46304/c221/111195/c218/46502/c12564/41145", uf_codigo))
  b <- ler(sprintf("https://apisidra.ibge.gov.br/values/t/6898/n6/in%%20n3%%20%s/v/40,1999/p/all/c829/46303,46304/c12547/114017/c12894/46569/c220/110085", uf_codigo))
  bind_rows(a, b) %>% filter(!is.na(valor)) %>%
    mutate(familiar = grepl("sim", tipologia, ignore.case = TRUE),
           medida = case_when(grepl("^Número de estabelecimentos agropecuários com produ", variavel) ~ "estab_producao",
                              grepl("^Número de estabelecimentos", variavel) ~ "estabelecimentos",
                              grepl("^Área", variavel) ~ "area_ha",
                              grepl("^Valor da produção", variavel) ~ "valor_producao_mil",
                              TRUE ~ variavel)) %>%
    select(id_municipio, familiar, medida, valor)
}

extrair_pronaf <- function(projeto, uf, ano_ini = 2013) {
  consultar(glue::glue("
    SELECT c.id_municipio, o.ano_emissao AS ano, o.id_programa, COUNT(*) AS contratos,
           SUM(o.valor_parcela_credito) AS valor
    FROM `basedosdados.br_bcb_sicor.operacao` o
    JOIN `basedosdados.br_bcb_sicor.recurso_publico_complemento_operacao` c
      ON o.id_referencia_bacen = c.id_referencia_bacen AND o.numero_ordem = c.numero_ordem
    WHERE o.sigla_uf = '{uf}' AND o.ano_emissao >= {ano_ini}
    GROUP BY 1, 2, 3"), projeto) %>%
    mutate(across(c(ano, contratos, valor), as.numeric), id_municipio = as.character(id_municipio))
}

montar_credito_rural <- function(pr, dic) {
  if (!NROW(pr)) return(tibble())
  pr %>% mutate(programa = rotular(id_programa, dic, "br_bcb_sicor", "operacao", "id_programa"),
                pronaf = grepl("pronaf", programa, ignore.case = TRUE))
}

# ---------------------------------------------------------------------------
# 6. Infraestrutura (lista curada em config/infraestrutura.csv)
# ---------------------------------------------------------------------------
ler_infraestrutura <- function(arquivo = caminho("config", "infraestrutura.csv")) {
  if (!file.exists(arquivo)) return(tibble())
  readr::read_csv(arquivo, col_types = readr::cols(.default = "c", lat = "d", lon = "d"))
}

# ---------------------------------------------------------------------------
# 7. Comex mensal (para o corte do ano corrente)
# ---------------------------------------------------------------------------
extrair_comex_mensal <- function(projeto, uf, ano_ini) {
  um <- function(tabela, fluxo) consultar(glue::glue("
    SELECT ano, mes, LPAD(CAST(id_sh4 AS STRING), 4, '0') AS sh4, SUM(SAFE_CAST(valor_fob_dolar AS FLOAT64)) AS valor_fob
    FROM `basedosdados.br_me_comex_stat.{tabela}`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini}
    GROUP BY 1, 2, 3"), projeto) %>% mutate(fluxo = fluxo)
  bind_rows(um("municipio_exportacao", "Exportação"), um("municipio_importacao", "Importação")) %>%
    mutate(across(c(ano, mes, valor_fob), as.numeric))
}

# Corte do ano corrente: acumulado no ano contra o mesmo período do ano anterior
corte_ano <- function(macro_ibcr, macro_pim, macro_pmc, macro_pms, caged_pe, comex_mensal, cnae, municipios,
                      ano = as.numeric(format(Sys.Date(), "%Y"))) {
  acum <- function(serie, rotulo, var_filtro = NULL) {
    if (!NROW(serie)) return(tibble())
    if (!is.null(var_filtro) && "variavel" %in% names(serie)) serie <- serie %>% filter(variavel == var_filtro)
    s <- serie %>% mutate(a = as.numeric(format(data, "%Y")), m = as.numeric(format(data, "%m")))
    ult_m <- s %>% filter(a == ano) %>% group_by(recorte) %>% summarise(um = max(m), .groups = "drop")
    if (!nrow(ult_m)) return(tibble())
    um <- min(ult_m$um)
    s %>% filter(a %in% c(ano - 1, ano), m <= um) %>% group_by(recorte, a) %>%
      summarise(v = mean(valor), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = a, values_from = v, names_prefix = "a") %>%
      transmute(indicador = rotulo, recorte, ate_mes = um, var = .data[[paste0("a", ano)]] / .data[[paste0("a", ano - 1)]] - 1)
  }
  ind <- bind_rows(
    acum(macro_ibcr, "Atividade econômica (IBCR)"),
    acum(macro_pim, "Produção industrial (PIM-PF)", "sem ajuste"),
    acum(macro_pmc, "Vendas do varejo (PMC)", "sem ajuste"),
    acum(macro_pms, "Volume de serviços (PMS)", "sem ajuste"))
  # emprego formal: saldo do CAGED no ano
  cg <- caged_pe %>% filter(ano == !!ano)
  caged <- if (nrow(cg)) list(
    ate_mes = max(cg$mes),
    saldo = sum(cg$saldo), admissoes = sum(cg$admissoes), desligamentos = sum(cg$desligamentos),
    anterior = sum(caged_pe$saldo[caged_pe$ano == ano - 1 & caged_pe$mes <= max(cg$mes)]),
    secoes = cg %>% mutate(divisao = substr(subclasse, 1, 2)) %>%
      left_join(distinct(cnae, divisao, secao, descricao_secao), by = "divisao") %>%
      group_by(secao, descricao_secao) %>% summarise(saldo = sum(saldo), .groups = "drop") %>% arrange(desc(saldo)),
    municipios = cg %>% group_by(id_municipio) %>% summarise(saldo = sum(saldo), .groups = "drop") %>%
      mutate(municipio = municipios$municipio[match(id_municipio, municipios$id_municipio)]) %>% arrange(desc(saldo))
  ) else NULL
  # comércio exterior no ano
  cx <- comex_mensal %>% filter(ano %in% c(!!ano - 1, !!ano))
  comex <- if (any(cx$ano == ano)) {
    um <- max(cx$mes[cx$ano == ano])
    cx <- cx %>% filter(mes <= um)
    list(ate_mes = um,
         total = cx %>% group_by(fluxo, ano) %>% summarise(v = sum(valor_fob), .groups = "drop"),
         sh4 = cx %>% filter(ano == !!ano, fluxo == "Exportação") %>% group_by(sh4) %>%
           summarise(v = sum(valor_fob), .groups = "drop") %>% arrange(desc(v)) %>% head(10))
  } else NULL
  list(ano = ano, indicadores = ind, caged = caged, comex = comex)
}
