# =============================================================================
# Bases do site para o território: complemento do raio-X dos municípios,
# raio-X das 12 Regiões de Desenvolvimento e a base de setores agregáveis
# =============================================================================

GRANDES_SETORES <- c(A = "Agropecuária", B = "Indústria", C = "Indústria", D = "Indústria", E = "Indústria",
                     F = "Construção", G = "Comércio")
grande_setor <- function(secao) unname(coalesce(GRANDES_SETORES[secao], ifelse(secao %in% c("?", NA), "Não classificado", "Serviços")))

# Agrega uma tabela com id_municipio para município, RD e estado
com_rd <- function(df, rd) df %>% left_join(rd, by = "id_municipio") %>% mutate(rd = coalesce(rd, "Não informada"))

# ---------------------------------------------------------------------------
# Peças por unidade territorial (vetor de municípios -> lista)
# ---------------------------------------------------------------------------
pedaco_empresas <- function(emp, ids) {
  if (!NROW(emp$fluxo)) return(NULL)
  f <- emp$fluxo %>% filter(id_municipio %in% ids)
  e <- emp$estoque %>% filter(id_municipio %in% ids)
  serie <- f %>% group_by(ano) %>% summarise(aberturas_mei = sum(aberturas[mei]), aberturas = sum(aberturas), baixas = sum(baixas),
                                             .groups = "drop") %>% arrange(ano)
  # o último ano do cadastro é parcial: só entra se a base tiver passado de junho
  ult <- as.numeric(format(emp$data_base, "%Y")); mes_base <- as.numeric(format(emp$data_base, "%m"))
  serie <- serie %>% filter(ano < ult | mes_base >= 7)
  por_secao <- e %>% filter(!mei) %>% group_by(secao) %>% summarise(ativas = sum(ativas), .groups = "drop") %>%
    left_join(emp$secoes, by = "secao") %>% arrange(desc(ativas)) %>% head(8) %>%
    transmute(setor = stringr::str_to_sentence(descricao_secao), ativas)
  list(data_base = format(emp$data_base, "%m/%Y"), serie = serie,
       ativas = sum(e$ativas), ativas_mei = sum(e$ativas[e$mei]), por_secao = por_secao)
}

pedaco_qualif <- function(q, ids) {
  if (!NROW(q)) return(NULL)
  x <- q %>% filter(id_municipio %in% ids)
  if (!nrow(x)) return(NULL)
  anos <- sort(unique(x$ano)); u <- max(anos); a0 <- min(anos)
  esc <- function(a) x %>% filter(ano == a) %>% group_by(escolaridade) %>% summarise(v = sum(vinculos), .groups = "drop") %>%
    mutate(p = v / sum(v))
  e_u <- esc(u); e_0 <- esc(a0)
  sup <- function(e) sum(e$p[e$escolaridade %in% c("Superior", "Pós-graduação")])
  ocup <- x %>% filter(ano == u) %>% group_by(ocupacao) %>% summarise(v = sum(vinculos), .groups = "drop") %>%
    mutate(p = v / sum(v)) %>% arrange(desc(v))
  list(ano = u, ano0 = a0, total = sum(e_u$v),
       escolaridade = e_u %>% mutate(ordem = match(escolaridade, c("Até fundamental", "Médio", "Superior", "Pós-graduação", "Não informado"))) %>% arrange(ordem) %>% select(escolaridade, v, p),
       superior = round(sup(e_u), 4), superior0 = round(sup(e_0), 4),
       ocupacao = ocup %>% select(ocupacao, v, p))
}

pedaco_educ <- function(ed, ids) {
  cur <- ed$cursos; ies <- ed$ies; tec <- ed$tecnico
  out <- list()
  if (NROW(cur)) {
    u <- max(cur$ano)
    c_u <- cur %>% filter(ano == u, id_municipio %in% ids, !grepl("dist", modalidade, ignore.case = TRUE))
    out$ano <- u
    out$matriculas <- sum(c_u$matriculas, na.rm = TRUE); out$concluintes <- sum(c_u$concluintes, na.rm = TRUE)
    out$cursos <- sum(c_u$cursos, na.rm = TRUE)
    out$publica <- sum(c_u$matriculas[c_u$rede == "Pública"], na.rm = TRUE)
    out$areas <- c_u %>% group_by(area) %>% summarise(matriculas = sum(matriculas, na.rm = TRUE), concluintes = sum(concluintes, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(matriculas))
    ies_ids <- unique(c_u$id_ies)
    out$ies <- if (NROW(ies)) ies %>% filter(id_ies %in% ies_ids | id_municipio %in% ids) %>%
      left_join(c_u %>% group_by(id_ies) %>% summarise(matriculas = sum(matriculas, na.rm = TRUE), .groups = "drop"), by = "id_ies") %>%
      arrange(desc(coalesce(matriculas, 0))) %>% transmute(nome, sigla, organizacao, categoria, matriculas = coalesce(matriculas, 0)) %>% head(25) else NULL
  }
  if (NROW(tec)) {
    ut <- max(tec$ano)
    t_u <- tec %>% filter(ano == ut, id_municipio %in% ids)
    out$tecnico <- list(ano = ut, matriculas = sum(t_u$matriculas, na.rm = TRUE), escolas = sum(t_u$escolas, na.rm = TRUE))
  }
  out
}

pedaco_icms <- function(ic, ids, pop_u) {
  if (!NROW(ic$mun)) return(NULL)
  s <- ic$mun %>% filter(id_municipio %in% ids, ano >= max(ic$mun$ano) - 10) %>% group_by(ano) %>%
    summarise(valor = sum(cota_icms), .groups = "drop") %>% arrange(ano)
  if (!nrow(s)) return(NULL)
  u <- max(s$ano)
  list(serie = s, ano = u, valor = s$valor[s$ano == u], per_capita = if (pop_u > 0) s$valor[s$ano == u] / pop_u else NA,
       part_estado = s$valor[s$ano == u] / sum(ic$mun$cota_icms[ic$mun$ano == u]))
}

pedaco_agro <- function(af, cr, ids) {
  out <- list()
  if (NROW(af)) {
    x <- af %>% filter(id_municipio %in% ids) %>% group_by(familiar, medida) %>% summarise(v = sum(valor, na.rm = TRUE), .groups = "drop")
    g <- function(med, fam) sum(x$v[x$medida == med & x$familiar == fam])
    out$censo <- list(ano = 2017,
                      estab_fam = g("estabelecimentos", TRUE), estab_tot = g("estabelecimentos", TRUE) + g("estabelecimentos", FALSE),
                      area_fam = g("area_ha", TRUE), area_tot = g("area_ha", TRUE) + g("area_ha", FALSE),
                      vp_fam = g("valor_producao_mil", TRUE), vp_tot = g("valor_producao_mil", TRUE) + g("valor_producao_mil", FALSE))
  }
  if (NROW(cr)) {
    ano_atual <- as.numeric(format(Sys.Date(), "%Y"))
    out$pronaf <- cr %>% filter(id_municipio %in% ids, pronaf, ano < ano_atual) %>% group_by(ano) %>%
      summarise(contratos = sum(contratos), valor = sum(valor), .groups = "drop") %>% arrange(ano)
    out$credito_total <- cr %>% filter(id_municipio %in% ids, ano < ano_atual) %>% group_by(ano) %>%
      summarise(valor = sum(valor), .groups = "drop") %>% arrange(ano)
  }
  out
}

# QL das cadeias e nichos para um conjunto de municípios
pedaco_cadeias <- function(cad_mun, emp_tot, cad_meta, ids, ult) {
  e <- cad_mun %>% filter(ano == ult) %>% group_by(cadeia_id, sel = id_municipio %in% ids) %>%
    summarise(emp = sum(emprego), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = sel, values_from = emp, values_fill = 0, names_prefix = "s")
  if (!"sTRUE" %in% names(e)) return(NULL)
  t_sel <- sum(emp_tot$e[emp_tot$ano == ult & emp_tot$id_municipio %in% ids]); t_pe <- sum(emp_tot$e[emp_tot$ano == ult])
  e %>% mutate(emprego = sTRUE, ql = round((sTRUE / t_sel) / ((sTRUE + coalesce(sFALSE, 0)) / t_pe), 2),
               part_pe = round(sTRUE / (sTRUE + coalesce(sFALSE, 0)), 4)) %>%
    left_join(cad_meta %>% select(cadeia_id, cadeia, grupo), by = "cadeia_id") %>%
    filter(emprego > 0) %>% arrange(desc(ql)) %>% select(cadeia_id, cadeia, grupo, emprego, ql, part_pe)
}

# ---------------------------------------------------------------------------
# Montagem
# ---------------------------------------------------------------------------
montar_territorio <- function(municipios, rd, pib_pop, pop, rais_secao, cad_mun, cad_meta, empresas, empresas_maiores,
                              qualificacao, educacao, icms, agro_familiar, credito_rural, infraestrutura) {
  pop_all <- bind_rows(pop, pib_pop %>% select(ano, id_municipio, populacao)) %>% filter(!is.na(populacao)) %>%
    distinct(ano, id_municipio, .keep_all = TRUE)
  pop_u <- pop_all %>% filter(ano == max(ano))
  ult <- max(rais_secao$ano)
  emp_tot <- rais_secao %>% group_by(ano, id_municipio) %>% summarise(e = sum(emprego), .groups = "drop")
  maiores <- if (NROW(empresas_maiores)) empresas_maiores else tibble(cadeia_id = character(), id_municipio = character(), rk_mun = numeric(), rk_uf = numeric())
  um <- function(ids) {
    p <- sum(pop_u$populacao[pop_u$id_municipio %in% ids])
    list(empresas = pedaco_empresas(empresas, ids), qualificacao = pedaco_qualif(qualificacao, ids),
         educacao = pedaco_educ(educacao, ids), icms = pedaco_icms(icms, ids, p), agro = pedaco_agro(agro_familiar, credito_rural, ids),
         infra = if (NROW(infraestrutura)) infraestrutura %>% filter(id_municipio %in% ids) %>% select(tipo, nome, nota) else NULL)
  }
  mun <- lapply(municipios$id_municipio, function(id) {
    x <- um(id)
    mm <- maiores %>% filter(id_municipio == id, rk_mun <= 5)
    x$maiores <- if (nrow(mm)) split(mm %>% arrange(rk_mun) %>% select(razao_social, nome_fantasia, atividade, porte, matriz_filial, abertura), mm$cadeia_id[order(mm$rk_mun)]) else NULL
    c(list(id = id), x)
  })
  names(mun) <- municipios$id_municipio

  # Regiões de Desenvolvimento
  rds <- sort(unique(rd$rd))
  pib_u <- pib_pop %>% filter(!is.na(pib)) %>% filter(ano == max(ano))
  # o valor adicionado por setor sai com defasagem em relação ao PIB total
  va_u <- pib_pop %>% filter(!is.na(va_industria)) %>% filter(ano == max(ano))
  regioes <- lapply(rds, function(r) {
    ids <- rd$id_municipio[rd$rd == r]
    p <- pib_u %>% filter(id_municipio %in% ids)
    e_s <- rais_secao %>% filter(ano == ult, id_municipio %in% ids) %>% group_by(secao, descricao_secao) %>%
      summarise(emprego = sum(emprego), .groups = "drop") %>% arrange(desc(emprego))
    e_ser <- rais_secao %>% filter(id_municipio %in% ids) %>% group_by(ano) %>% summarise(emprego = sum(emprego), .groups = "drop")
    c(list(rd = r, municipios = municipios %>% filter(id_municipio %in% ids) %>% transmute(id = id_municipio, nome = municipio) %>% arrange(nome),
           pop = sum(pop_u$populacao[pop_u$id_municipio %in% ids]), ano_pop = max(pop_u$ano),
           pib = list(ano = max(pib_u$ano), valor = sum(p$pib), part = sum(p$pib) / sum(pib_u$pib),
                      pc = sum(p$pib) / sum(p$populacao),
                      va = with(va_u[va_u$id_municipio %in% ids, ],
                                list(ano = max(va_u$ano), agro = sum(va_agropecuaria, na.rm = TRUE), ind = sum(va_industria, na.rm = TRUE),
                                     serv = sum(va_servicos, na.rm = TRUE), adm = sum(va_adespss, na.rm = TRUE)))),
           emprego = list(ano = ult, total = sum(e_s$emprego), part = sum(e_s$emprego) / sum(emp_tot$e[emp_tot$ano == ult]),
                          serie = e_ser, setores = e_s %>% head(8) %>% transmute(setor = stringr::str_to_sentence(descricao_secao), emprego)),
           cadeias = pedaco_cadeias(cad_mun, emp_tot, cad_meta, ids, ult),
           maiores_mun = municipios %>% filter(id_municipio %in% ids) %>%
             left_join(emp_tot %>% filter(ano == ult), by = "id_municipio") %>% arrange(desc(e)) %>% head(5) %>%
             transmute(id = id_municipio, nome = municipio, emprego = e)),
      um(ids))
  })
  names(regioes) <- rds
  list(municipios = mun, regioes = regioes)
}

# Base de setores (classe CNAE) para agregar e desagregar no site
extrair_cnae_grupos <- function(projeto) {
  consultar("SELECT DISTINCT grupo, descricao_grupo FROM `basedosdados.br_bd_diretorios_brasil.cnae_2`", projeto)
}

montar_setores <- function(rais_pe, cnae, grupos, rd) {
  ult <- max(rais_pe$ano); anos <- c(ult - 10, ult - 5, ult)
  grupos <- grupos %>% mutate(grupo = gsub("[^0-9]", "", as.character(grupo))) %>% distinct(grupo, .keep_all = TRUE)
  dic <- cnae %>% distinct(classe, descricao_classe, divisao, descricao_divisao, secao, descricao_secao) %>%
    distinct(classe, .keep_all = TRUE) %>% mutate(grupo = substr(classe, 1, 3)) %>%
    left_join(grupos, by = "grupo") %>%
    mutate(descricao_grupo = coalesce(descricao_grupo, paste("Grupo", grupo)), grande = grande_setor(secao),
           across(starts_with("descricao_"), ~ stringr::str_to_sentence(.x)))
  x <- rais_pe %>% filter(ano %in% anos) %>% com_rd(rd) %>%
    group_by(ano, rd, classe) %>% summarise(emprego = sum(vinculos), estab = sum(estabelecimentos), .groups = "drop")
  pe <- x %>% group_by(ano, classe) %>% summarise(emprego = sum(emprego), estab = sum(estab), .groups = "drop") %>% mutate(rd = "Pernambuco")
  dados <- bind_rows(pe, x) %>% filter(emprego > 0 | estab > 0)
  list(anos = anos, dicionario = dic %>% filter(classe %in% dados$classe), dados = dados)
}

exportar_territorio <- function(territorio, setores, destino = caminho("site", "dados")) {
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  w <- function(obj, nome) { arq <- file.path(destino, nome)
    jsonlite::write_json(obj, arq, auto_unbox = TRUE, na = "null", digits = 6, dataframe = "rows", null = "null"); arq }
  c(w(unname(territorio$municipios), "territorio_municipios.json"),
    w(unname(territorio$regioes), "regioes.json"),
    w(setores, "setores.json"))
}
