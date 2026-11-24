# =============================================================================
# Demanda das famílias por cadeia (POF 2017-2018, IBGE)
#
# A Pesquisa de Orçamentos Familiares diz quanto as famílias gastam, em média,
# por mês, com cada tipo de produto e serviço, por UF e por faixa de renda.
# Aqui os tipos de despesa ganham uma cadeia (config/pof_cadeias.csv) e viram:
#   - gasto médio mensal por família com os produtos da cadeia (R$ de 2018);
#   - gasto anual das famílias do estado (média x 12 x número de famílias);
#   - peso no consumo das famílias e índice frente ao Brasil e ao Nordeste
#     (peso em PE / peso na região; acima de 1, as famílias daqui gastam
#     relativamente mais com a cadeia);
#   - de onde vem a demanda por faixa de renda.
# Fonte: API de agregados do IBGE, tabelas 6715 (despesa por tipo), 6972
# (despesa com alimentação) e 6977 (número de famílias). Valores de janeiro de
# 2018, sem correção. É demanda das famílias do estado, atendida por produção
# daqui ou de fora; não mede consumo das empresas nem do governo.
# =============================================================================

POF_ANO <- 2018
POF_CONSUMO <- "103538"   # 2.1 Despesas de consumo (tabela 6715)
POF_CLASSES <- c("47558", "47559", "47560", "47561", "47562", "47563", "47564")

pof_ler <- function(url, tentativas = 4) {
  op <- options(timeout = max(300, getOption("timeout"))); on.exit(options(op), add = TRUE)
  for (i in seq_len(tentativas)) {
    res <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    msg("POF/IBGE falhou (tentativa ", i, "): ", conditionMessage(res))
    Sys.sleep(5 * i)
  }
  stop("API de agregados do IBGE indisponível: ", url)
}

# resposta da API v3 (lista de variáveis > resultados > séries) em tabela longa
pof_arrumar <- function(j, tabela) {
  linhas <- list()
  for (v in j) for (r in v$resultados) {
    cid <- vapply(r$classificacoes, function(cl) as.character(cl$id), "")
    cat_id <- vapply(r$classificacoes, function(cl) names(cl$categoria)[1], "")
    cat_nm <- vapply(r$classificacoes, function(cl) as.character(cl$categoria[[1]]), "")
    pega <- function(x, id) if (any(cid == id)) x[cid == id][1] else NA_character_
    for (s in r$series) {
      linhas[[length(linhas) + 1]] <- data.frame(
        tabela = tabela, variavel = as.character(v$id),
        local_id = as.character(s$localidade$id), local = as.character(s$localidade$nome),
        classe_id = pega(cat_id, "339"), classe = pega(cat_nm, "339"),
        tipo_id = pega(cat_id, "12190"), tipo = pega(cat_nm, "12190"),
        valor = suppressWarnings(as.numeric(s$serie[[as.character(POF_ANO)]])), stringsAsFactors = FALSE)
    }
  }
  dplyr::bind_rows(linhas)
}

extrair_pof <- function(uf_codigo) {
  regiao <- substr(as.character(uf_codigo), 1, 1)
  loc <- sprintf("N1[all]|N2[%s]|N3[%s]", regiao, uf_codigo)
  url <- function(tab, var, cl) sprintf(
    "https://servicodados.ibge.gov.br/api/v3/agregados/%d/periodos/%d/variaveis/%s?localidades=%s&classificacao=%s",
    tab, POF_ANO, utils::URLencode(var, reserved = TRUE), utils::URLencode(loc, reserved = TRUE), utils::URLencode(cl, reserved = TRUE))
  out <- dplyr::bind_rows(
    pof_arrumar(pof_ler(url(6715, "1201", "339[all]|12190[all]")), 6715),
    pof_arrumar(pof_ler(url(6972, "1201", "339[all]|12190[all]")), 6972),
    pof_arrumar(pof_ler(url(6977, "1211|170", "339[all]")), 6977))
  msg("POF 2017-2018: ", nrow(out), " valores (Brasil, região e UF).")
  out
}

ler_pof_cadeias <- function(arq) {
  m <- utils::read.csv(arq, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  m$tabela <- as.numeric(m$tabela); m$peso <- as.numeric(m$peso)
  m
}

demanda_pof_cadeia <- function(pof, mapa) {
  vazio <- list(cadeia = tibble::tibble(), renda = tibble::tibble(), itens = tibble::tibble(), ano = POF_ANO)
  if (!NROW(pof)) return(vazio)
  fam  <- pof %>% filter(tabela == 6977, variavel == "1211") %>% select(local_id, classe_id, familias = valor)
  desp <- pof %>% filter(tabela %in% c(6715, 6972), variavel == "1201")
  cons <- desp %>% filter(tabela == 6715, tipo_id == POF_CONSUMO) %>% select(local_id, classe_id, consumo = valor)
  itens <- desp %>% inner_join(mapa %>% select(tabela, tipo_id, cadeia_id, peso), by = c("tabela", "tipo_id")) %>%
    mutate(valor = valor * peso)
  d <- itens %>%
    group_by(local_id, local, classe_id, classe, cadeia_id) %>%
    summarise(despesa_mensal = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    left_join(fam, by = c("local_id", "classe_id")) %>%
    left_join(cons, by = c("local_id", "classe_id")) %>%
    mutate(despesa_anual = despesa_mensal * 12 * familias, part_consumo = despesa_mensal / consumo)
  tot <- d %>% filter(classe_id == "7999")
  br  <- tot %>% filter(local_id == "1") %>% select(cadeia_id, despesa_mensal_br = despesa_mensal, part_br = part_consumo)
  ne  <- tot %>% filter(nchar(local_id) == 1, local_id != "1") %>% select(cadeia_id, despesa_mensal_ne = despesa_mensal, part_ne = part_consumo)
  uf  <- tot %>% filter(nchar(local_id) == 2) %>%
    left_join(br, by = "cadeia_id") %>% left_join(ne, by = "cadeia_id") %>%
    mutate(indice_br = part_consumo / part_br, indice_ne = part_consumo / part_ne, ano = POF_ANO) %>%
    arrange(desc(despesa_anual))
  renda <- d %>% filter(nchar(local_id) == 2, classe_id %in% POF_CLASSES) %>%
    group_by(cadeia_id) %>% mutate(part_demanda = despesa_anual / sum(despesa_anual, na.rm = TRUE)) %>% ungroup() %>%
    arrange(cadeia_id, match(classe_id, POF_CLASSES))
  itens_uf <- itens %>% filter(nchar(local_id) == 2, classe_id == "7999") %>%
    left_join(mapa %>% select(tabela, tipo_id, nota), by = c("tabela", "tipo_id")) %>%
    select(cadeia_id, tabela, tipo_id, tipo, despesa_mensal = valor, nota)
  msg("POF: demanda das famílias calculada para ", n_distinct(uf$cadeia_id), " cadeias.")
  list(cadeia = uf, renda = renda, itens = itens_uf, ano = POF_ANO)
}
