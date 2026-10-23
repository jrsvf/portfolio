# =============================================================================
# Nível de atividade (Banco Central e IBGE) e Raio-X econômico dos municípios
# =============================================================================
# Macro (só existem por estado/região):
#   IBCR (BCB/SGS)  índice de atividade econômica regional, com ajuste sazonal
#   PIM-PF (IBGE)   produção física industrial, tabela SIDRA 8888
# Municípios (dados públicos que descem ao município):
#   PIB e valor adicionado por setor (IBGE), população, emprego formal por
#   setor e remuneração (RAIS), saldo de empregos (Novo CAGED), exportações e
#   importações (ComexStat), produção agropecuária (PAM/PPM) e as 21 cadeias.
# Toda extração externa falha de forma suave: sem a fonte, a seção some.
# =============================================================================

SGS_ATIVIDADE <- c("Pernambuco" = 25418, "Nordeste" = 25389, "Brasil" = 24364)

obter_sgs <- function(codigos = SGS_ATIVIDADE, inicio = "01/01/2003") {
  purrr::imap_dfr(codigos, function(cod, nome) {
    url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s", cod, inicio)
    x <- NULL
    for (i in 1:4) {   # a API do SGS falha de forma intermitente
      x <- tryCatch(jsonlite::fromJSON(url), error = function(e) { msg("SGS ", cod, " tentativa ", i, ": ", conditionMessage(e)); NULL })
      if (!is.null(x) && NROW(x)) break
      Sys.sleep(3 * i)
    }
    if (is.null(x) || !NROW(x)) return(tibble())
    tibble(recorte = nome, serie = as.character(cod),
           data = as.Date(x$data, "%d/%m/%Y"), valor = as.numeric(x$valor))
  })
}

# PIM-PF regional: descobre variáveis e a categoria "Indústria geral" pelos
# metadados, para não depender de códigos fixos que o IBGE troca a cada rebase.
obter_pim <- function(tabela = 8888, uf_codigo = 26) {
  meta <- tryCatch(jsonlite::fromJSON(sprintf("https://servicodados.ibge.gov.br/api/v3/agregados/%s/metadados", tabela)),
                   error = function(e) { msg("PIM-PF: metadados indisponíveis: ", conditionMessage(e)); NULL })
  if (is.null(meta)) return(tibble())
  v <- meta$variaveis
  v_sem <- v$id[grepl("^PIMPF - N[úu]mero-[íi]ndice \\(", v$nome) & !grepl("ajuste", v$nome)][1]
  v_com <- v$id[grepl("N[úu]mero-[íi]ndice com ajuste sazonal", v$nome)][1]
  if (is.na(v_sem)) v_sem <- v$id[grepl("[íi]ndice", v$nome) & !grepl("ajuste|Varia", v$nome)][1]
  cl <- meta$classificacoes
  cat_ig <- NA; cl_id <- NA
  for (i in seq_len(NROW(cl))) {
    cats <- cl$categorias[[i]]
    k <- cats$id[grepl("Ind[úu]stria geral", cats$nome)]
    if (length(k)) { cat_ig <- k[1]; cl_id <- cl$id[i]; break }
  }
  if (is.na(v_sem) || is.na(cat_ig)) { msg("PIM-PF: variável ou categoria não encontrada"); return(tibble()) }
  vars <- paste(na.omit(c(v_sem, v_com)), collapse = ",")
  url <- sprintf("https://apisidra.ibge.gov.br/values/t/%s/n1/all/n2/2/n3/%s/v/%s/p/all/c%s/%s",
                 tabela, uf_codigo, vars, cl_id, cat_ig)
  raw <- tryCatch(sidra_baixar(url), error = function(e) { msg("PIM-PF indisponível: ", conditionMessage(e)); NULL })
  if (is.null(raw) || NROW(raw) < 2) return(tibble())
  cab <- as.character(raw[1, ]); df <- raw[-1, , drop = FALSE]
  col_mes <- names(raw)[which(cab == "Mês (Código)")[1]]
  col_var <- names(raw)[which(cab == "Variável (Código)")[1]]
  # nome do território: coluna D?N cujo cabeçalho fala de Brasil/Região/UF
  col_ter <- names(raw)[grepl("^D[0-9]N$", names(raw)) & grepl("Brasil|Região|Federação", cab)][1]
  tibble(recorte = df[[col_ter]],
         variavel = ifelse(df[[col_var]] == as.character(v_com), "com ajuste", "sem ajuste"),
         data = as.Date(paste0(substr(df[[col_mes]], 1, 4), "-", substr(df[[col_mes]], 5, 6), "-01")),
         valor = suppressWarnings(as.numeric(df$V))) %>%
    filter(!is.na(valor)) %>%
    mutate(recorte = dplyr::case_when(grepl("Pernambuco", recorte) ~ "Pernambuco",
                                      grepl("Nordeste", recorte) ~ "Nordeste",
                                      grepl("Brasil", recorte) ~ "Brasil", TRUE ~ recorte))
}

# Médias anuais (só anos completos) e variação nos cortes de 10, 5 e 2 anos
atividade_anual <- function(serie) {
  if (is.null(serie) || !NROW(serie)) return(tibble())
  serie %>% mutate(ano = as.numeric(format(data, "%Y"))) %>%
    group_by(recorte, ano) %>% filter(n() == 12) %>%
    summarise(valor = mean(valor), .groups = "drop")
}

# ---------------------------------------------------------------------------
# Extrações municipais extras (BigQuery)
# ---------------------------------------------------------------------------
extrair_pop_uf <- function(projeto, uf) {
  sql <- glue::glue("
    SELECT p.ano, p.id_municipio, p.populacao
    FROM `basedosdados.br_ibge_populacao.municipio` p
    JOIN `basedosdados.br_bd_diretorios_brasil.municipio` d USING (id_municipio)
    WHERE d.sigla_uf = '{uf}' AND p.ano >= 2010")
  tryCatch(consultar(sql, projeto) %>% mutate(ano = as.numeric(ano), populacao = as.numeric(populacao),
                                              id_municipio = as.character(id_municipio)),
           error = function(e) { msg("População: ", conditionMessage(e)); tibble() })
}

# Nomes dos produtos SH4 (descobre as colunas do diretório pela INFORMATION_SCHEMA)
# Nomes das posições SH4 em português. A Base dos Dados só tem o diretório NCM;
# os nomes vêm da API do ComexStat (MDIC), guardados em config/sh4_nomes.csv
# porque a API falha com frequência. Se a API responder, o arquivo é atualizado.
extrair_sh4_nomes <- function(projeto = NULL, arquivo = caminho("config", "sh4_nomes.csv")) {
  api <- NULL
  for (i in 1:3) {
    api <- tryCatch({
      x <- jsonlite::fromJSON("https://api-comexstat.mdic.gov.br/tables/hs?language=pt")$data$list
      tibble(sh4 = x$headingCode, produto = x$heading) %>% filter(!is.na(produto)) %>% distinct(sh4, .keep_all = TRUE)
    }, error = function(e) { msg("ComexStat (nomes SH4) tentativa ", i, ": ", conditionMessage(e)); NULL })
    if (!is.null(api) && nrow(api)) break
    Sys.sleep(3 * i)
  }
  if (!is.null(api) && nrow(api)) { readr::write_csv(api, arquivo); return(api) }
  if (file.exists(arquivo)) return(readr::read_csv(arquivo, col_types = "cc"))
  tibble(sh4 = character(), produto = character())
}

# ---------------------------------------------------------------------------
# Raio-X: uma lista por município, gravada em JSON para a página Municípios
# ---------------------------------------------------------------------------
var_corte <- function(df, ult, janelas = CORTES_PADRAO$janelas) {
  # df: ano, valor. Devolve variação % do último ano contra cada corte
  fim <- df$valor[df$ano == ult]
  if (!length(fim)) return(list())
  setNames(lapply(janelas, function(j) {
    ini <- df$valor[df$ano == ult - j]
    if (length(ini) && !is.na(ini) && ini > 0) round(fim / ini - 1, 4) else NULL
  }), paste0("a", janelas))
}

montar_raiox <- function(municipios, rd, pib_pop, pop, rais_secao, perfil_pe, caged_pe, comex, sh4_nomes,
                         sidra_bruto, locais_cadeia, sidra_cad, comex_cad, cad_meta, krugman, ipca) {
  r2 <- function(x, d = 2) round(x, d)
  ult_rais <- max(rais_secao$ano)
  ult_pib <- max(pib_pop$ano[!is.na(pib_pop$pib)])
  ult_va  <- max(pib_pop$ano[!is.na(pib_pop$va_industria)])   # o valor adicionado por setor sai com defasagem
  anos_graf <- (ult_rais - max(CORTES_PADRAO$janelas)):ult_rais

  # deflator IPCA (preços do último ano do PIB)
  defl <- if (!is.null(ipca) && NROW(ipca)) {
    i <- ipca %>% select(ano, ipca = 2)
    i %>% mutate(f = i$ipca[i$ano == ult_pib] / ipca) %>% select(ano, f)
  } else tibble(ano = numeric(), f = numeric())

  # população mais recente (estimativas) com fallback para a do PIB
  pop_all <- bind_rows(pop, pib_pop %>% select(ano, id_municipio, populacao)) %>%
    filter(!is.na(populacao)) %>% distinct(ano, id_municipio, .keep_all = TRUE)
  ult_pop <- max(pop_all$ano)

  emp_tot <- rais_secao %>% group_by(ano, id_municipio) %>% summarise(e = sum(emprego), .groups = "drop")
  rem <- perfil_pe %>% group_by(ano, id_municipio) %>%
    summarise(rem = weighted.mean(remuneracao_media, vinculos_ativos, na.rm = TRUE), .groups = "drop")
  rem_est <- perfil_pe %>% group_by(ano) %>%
    summarise(rem = weighted.mean(remuneracao_media, vinculos_ativos, na.rm = TRUE), .groups = "drop")
  caged <- caged_pe %>% mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes))) %>%
    group_by(id_municipio, data) %>% summarise(saldo = sum(saldo), .groups = "drop")
  ult_caged <- max(caged$data)
  ult_comex <- max(comex$ano)
  nomes_sh4 <- setNames(sh4_nomes$produto, sh4_nomes$sh4)

  # produção agropecuária (último ano de cada tabela)
  sb <- sidra_bruto %>% filter(produto != "Total", !is.na(valor))
  ult_sidra <- sb %>% group_by(tabela) %>% summarise(u = max(ano))
  sb_u <- sb %>% inner_join(ult_sidra, by = "tabela") %>% filter(ano == u)
  pam <- sb_u %>% filter(tabela == 5457, variavel == "Valor da produção")
  pam_qtd <- sb_u %>% filter(tabela == 5457, variavel == "Quantidade produzida") %>% select(id_municipio, produto, qtd = valor)
  pam_serie <- sb %>% filter(tabela == 5457, variavel == "Valor da produção") %>%
    group_by(id_municipio, ano) %>% summarise(v = sum(valor), .groups = "drop")
  reb <- sb_u %>% filter(tabela == 3939, !produto %in% c("Suíno - matrizes de suínos", "Galináceos - galinhas"))
  ani <- sb_u %>% filter(tabela == 74, variavel == "Valor da produção")
  aqu <- sb_u %>% filter(tabela == 3940, variavel == "Valor da produção")
  ano_tab <- setNames(ult_sidra$u, ult_sidra$tabela)

  # especialização no estado (para comparar)
  pib_est <- pib_pop %>% filter(ano == ult_pib) %>% summarise(across(c(pib, va_agropecuaria, va_industria, va_servicos, va_adespss, populacao), ~ sum(.x, na.rm = TRUE)))
  pibpc <- pib_pop %>% filter(ano == ult_pib, !is.na(pib), populacao > 0) %>% mutate(pc = pib / populacao) %>%
    mutate(rank = rank(-pc, ties.method = "min"))
  rd_pc <- pibpc %>% left_join(rd, by = "id_municipio") %>% group_by(rd) %>%
    summarise(pc_rd = sum(pib) / sum(populacao), .groups = "drop")

  nm_cad <- setNames(cad_meta$cadeia, cad_meta$cadeia_id)

  out <- lapply(seq_len(nrow(municipios)), function(i) {
    id <- municipios$id_municipio[i]; nome <- municipios$municipio[i]
    rd_m <- rd$rd[rd$id_municipio == id]
    p <- pib_pop %>% filter(id_municipio == id, !is.na(pib)) %>% arrange(ano)
    pu <- p %>% filter(ano == ult_pib)
    pv <- p %>% filter(ano == ult_va)
    pop_u <- pop_all %>% filter(id_municipio == id, ano == ult_pop) %>% pull(populacao)
    pib_real <- p %>% left_join(defl, by = "ano") %>% mutate(v = pib * coalesce(f, NA_real_)) %>% filter(!is.na(v))
    e <- emp_tot %>% filter(id_municipio == id) %>% arrange(ano)
    sec <- rais_secao %>% filter(id_municipio == id, ano == ult_rais, emprego > 0) %>% arrange(desc(emprego))
    rm_ <- rem %>% filter(id_municipio == id, ano == ult_rais) %>% pull(rem)
    cg <- caged %>% filter(id_municipio == id) %>% arrange(data)
    cg12 <- cg %>% filter(data > seq(ult_caged, by = "-1 year", length.out = 2)[2]) %>% summarise(s = sum(saldo)) %>% pull(s)
    cx <- comex %>% filter(id_municipio == id)
    cx_ano <- cx %>% group_by(ano, fluxo) %>% summarise(v = sum(valor_fob), .groups = "drop")
    cx_top <- cx %>% filter(ano == ult_comex, fluxo == "Exportação") %>% group_by(sh4) %>%
      summarise(v = sum(valor_fob), .groups = "drop") %>% arrange(desc(v)) %>% head(8)
    cad_l <- locais_cadeia %>% filter(id_municipio == id)
    cad_u <- cad_l %>% filter(ano == ult_rais, emprego > 0)
    cad_10 <- cad_l %>% filter(ano == ult_rais - max(CORTES_PADRAO$janelas)) %>% select(atividade, e0 = emprego)
    sc <- sidra_cad %>% filter(id_municipio == id, !is.na(valor)) %>% group_by(cadeia_id) %>% filter(ano == max(ano)) %>% ungroup()
    sc_val <- sc %>% filter(variavel == "Valor da produção") %>% group_by(cadeia_id) %>% summarise(vp = sum(valor), .groups = "drop")
    cc <- comex_cad %>% filter(id_municipio == id, ano == ult_comex, fluxo == "Exportação") %>%
      group_by(cadeia_id) %>% summarise(ex = sum(valor_fob), .groups = "drop")
    tab_cad <- cad_u %>% left_join(cad_10, by = "atividade") %>%
      full_join(sc_val, by = c("atividade" = "cadeia_id")) %>% full_join(cc, by = c("atividade" = "cadeia_id")) %>%
      mutate(cadeia = unname(nm_cad[atividade])) %>% filter(!is.na(cadeia)) %>%
      filter(coalesce(emprego, 0) > 0 | coalesce(vp, 0) > 0 | coalesce(ex, 0) > 0) %>%
      arrange(desc(coalesce(emprego, 0)), desc(coalesce(vp, 0))) %>%
      transmute(cadeia, emprego = coalesce(emprego, 0), ql = r2(ql), var10 = ifelse(!is.na(e0) & e0 > 0, r2(emprego / e0 - 1, 4), NA),
                vp_mil = vp, ex_usd = ex)
    lav <- pam %>% filter(id_municipio == id, valor > 0) %>% left_join(pam_qtd, by = c("id_municipio", "produto")) %>%
      arrange(desc(valor)) %>% head(8) %>% transmute(produto, valor_mil = valor, qtd_t = qtd)
    k <- krugman %>% filter(id_municipio == id, ano == ult_rais) %>% pull(krugman)
    list(
      id = id, nome = nome, rd = if (length(rd_m)) rd_m else NA,
      pop = if (length(pop_u)) pop_u else NA, ano_pop = ult_pop,
      pib = list(ano = ult_pib, valor = pu$pib, pc = if (nrow(pu) && pu$populacao > 0) r2(pu$pib / pu$populacao) else NA,
                 rank_pc = pibpc$rank[pibpc$id_municipio == id], part_estado = r2(pu$pib / pib_est$pib, 5),
                 pc_rd = r2(rd_pc$pc_rd[match(rd_m, rd_pc$rd)]), pc_estado = r2(pib_est$pib / pib_est$populacao),
                 ano_va = ult_va,
                 va = list(agro = pv$va_agropecuaria, ind = pv$va_industria, serv = pv$va_servicos, adm = pv$va_adespss),
                 serie = p %>% filter(ano >= ult_va - max(CORTES_PADRAO$janelas), !is.na(va_industria)) %>% transmute(ano, pib, agro = va_agropecuaria, ind = va_industria, serv = va_servicos, adm = va_adespss),
                 var_real = var_corte(pib_real %>% transmute(ano, valor = v), ult_pib)),
      emprego = list(ano = ult_rais, total = e$e[e$ano == ult_rais],
                     part_estado = r2(e$e[e$ano == ult_rais] / sum(emp_tot$e[emp_tot$ano == ult_rais]), 5),
                     por_mil = if (length(pop_u) && pop_u > 0) r2(1000 * e$e[e$ano == ult_rais] / pop_u, 1) else NA,
                     var = var_corte(e %>% transmute(ano, valor = e), ult_rais),
                     serie = e %>% filter(ano %in% anos_graf) %>% transmute(ano, emprego = e),
                     setores = sec %>% transmute(setor = str_to_sentence(descricao_secao), emprego),
                     remuneracao = if (length(rm_)) r2(rm_) else NA,
                     rem_estado = r2(rem_est$rem[rem_est$ano == ult_rais]),
                     krugman = if (length(k)) r2(k, 3) else NA),
      caged = list(ate = format(ult_caged, "%m/%Y"), saldo_12m = if (length(cg12)) cg12 else 0,
                   serie = cg %>% filter(data >= as.Date(paste0(format(ult_caged, "%Y"), "-01-01")) - 365 * 2) %>%
                     transmute(data = format(data, "%Y-%m"), saldo)),
      comex = list(ano = ult_comex,
                   serie = cx_ano %>% filter(ano >= ult_comex - max(CORTES_PADRAO$janelas)) %>% tidyr::pivot_wider(names_from = fluxo, values_from = v, values_fill = 0),
                   top = cx_top %>% mutate(produto = coalesce(unname(nomes_sh4[sh4]), paste("SH", sh4)))),
      agro = list(ano_pam = unname(ano_tab["5457"]), ano_ppm = unname(ano_tab["3939"]),
                  lavouras = lav, pam_total = sum(pam$valor[pam$id_municipio == id]),
                  pam_serie = pam_serie %>% filter(id_municipio == id, ano >= max(pam_serie$ano) - max(CORTES_PADRAO$janelas)) %>% transmute(ano, valor_mil = v),
                  rebanhos = reb %>% filter(id_municipio == id, valor > 0) %>% transmute(produto = sub(" - total", "", produto), cabecas = valor),
                  animal = ani %>% filter(id_municipio == id, valor > 0) %>% transmute(produto, valor_mil = valor),
                  aquicultura = sum(aqu$valor[aqu$id_municipio == id])),
      cadeias = tab_cad
    )
  })
  names(out) <- municipios$id_municipio
  out
}

exportar_raiox <- function(raiox, malha, destino = caminho("site", "dados", "raiox_municipios.json")) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unname(raiox), destino, auto_unbox = TRUE, na = "null", digits = NA, dataframe = "rows")
  # malha leve para o mapa de localização
  # Fernando de Noronha fica fora do mapa de localização (distorce o enquadramento)
  geo <- sf::st_simplify(sf::st_transform(malha %>% filter(id_municipio != "2605459"), 4326), dTolerance = 0.005, preserveTopology = TRUE)
  geo_arq <- caminho("site", "dados", "malha_pe.geojson")
  unlink(geo_arq)
  sf::st_write(geo %>% select(id_municipio, name_muni), geo_arq, driver = "GeoJSON", quiet = TRUE,
               layer_options = "COORDINATE_PRECISION=4")
  c(destino, geo_arq)
}
