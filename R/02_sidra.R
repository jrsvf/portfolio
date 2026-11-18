# =============================================================================
# IBGE SIDRA (API pública): produção agropecuária municipal
# =============================================================================
# Usa a API REST diretamente (sem sidrar) para controlar paginação e erros.
# Para cada tabela pede todos os produtos/rebanhos e todas as variáveis
# (exceto percentuais) dos municípios da UF, ano a ano, e filtra pelo NOME
# do produto definido em config/cadeias.yml.
# =============================================================================

# Classificação de produto de cada tabela (necessária na URL da API)
SIDRA_CLASSIFICACAO <- c(
  "5457" = "c782",   # PAM - produto das lavouras temporárias e permanentes
  "3939" = "c79",    # PPM - tipo de rebanho
  "74"   = "c80",    # PPM - tipo de produto de origem animal
  "3940" = "c654",   # PPM - tipo de produto da aquicultura
  "289"  = "c193",   # PEVS - tipo de produto extrativo
  "291"  = "c194"    # PEVS - tipo de produto da silvicultura
)

# Códigos especiais do IBGE para valores não numéricos
SIDRA_ESPECIAIS <- c(
  "-"   = "zero absoluto",
  "X"   = "omitido (sigilo)",
  ".."  = "não se aplica",
  "..." = "não disponível"
)

# Descobre a classificação de produto pelos metadados da tabela; se a API de
# metadados falhar, usa o mapa fixo acima.
sidra_classificacao <- function(tabela) {
  meta <- tryCatch(
    jsonlite::fromJSON(sprintf("https://servicodados.ibge.gov.br/api/v3/agregados/%s/metadados", tabela)),
    error = function(e) NULL)
  if (!is.null(meta) && length(meta$classificacoes) > 0 && NROW(meta$classificacoes) > 0) {
    return(paste0("c", meta$classificacoes$id[1]))
  }
  cl <- SIDRA_CLASSIFICACAO[as.character(tabela)]
  if (is.na(cl)) stop("Tabela SIDRA sem classificação mapeada: ", tabela,
                      ". Inclua em SIDRA_CLASSIFICACAO (R/02_sidra.R).")
  unname(cl)
}

# Variáveis pedidas por tabela. A PAM tem muitos produtos e estoura o limite
# de 100 mil valores da API se pedir todas; ficam área colhida, quantidade e valor.
SIDRA_VARIAVEIS <- c("5457" = "216,214,215")

sidra_url <- function(tabela, ano, uf_codigo, cl = sidra_classificacao(tabela)) {
  v <- SIDRA_VARIAVEIS[as.character(tabela)]
  if (!is.na(v)) {
    return(sprintf("https://apisidra.ibge.gov.br/values/t/%s/n6/in%%20n3%%20%s/v/%s/p/%s/%s/all",
                   tabela, uf_codigo, v, ano, cl))
  }
  sprintf("https://apisidra.ibge.gov.br/values/t/%s/n6/in%%20n3%%20%s/v/allxp/p/%s/%s/all",
          tabela, uf_codigo, ano, cl)
}

sidra_baixar <- function(url, tentativas = 4) {
  # a API do SIDRA às vezes leva mais de 60 s (o limite padrão do R) em tabelas grandes
  op <- options(timeout = max(300, getOption("timeout"))); on.exit(options(op), add = TRUE)
  for (i in seq_len(tentativas)) {
    res <- tryCatch(jsonlite::fromJSON(url, simplifyDataFrame = TRUE),
                    error = function(e) e)
    if (!inherits(res, "error")) return(res)
    msg("SIDRA falhou (tentativa ", i, "): ", conditionMessage(res))
    Sys.sleep(5 * i)
  }
  stop("SIDRA indisponível após ", tentativas, " tentativas: ", url)
}

# Converte a resposta da API (primeira linha = cabeçalho) em tabela arrumada.
sidra_arrumar <- function(raw, tabela) {
  if (is.null(raw) || NROW(raw) < 2) return(tibble())
  cab <- as.character(raw[1, ])
  df <- tibble::as_tibble(raw[-1, , drop = FALSE])
  names(df) <- cab
  col_mun  <- grep("^Município \\(Código\\)$", cab, value = TRUE)
  col_ano  <- grep("^Ano$", cab, value = TRUE)
  col_var  <- grep("^Variável$", cab, value = TRUE)
  col_uni  <- grep("^Unidade de Medida$", cab, value = TRUE)
  # a coluna de produto é a única descritiva que sobra após as conhecidas
  conhecidas <- c("Nível Territorial", "Nível Territorial (Código)", "Unidade de Medida",
                  "Unidade de Medida (Código)", "Valor", "Brasil", "Município",
                  "Município (Código)", "Variável", "Variável (Código)", "Ano", "Ano (Código)")
  col_prod <- setdiff(cab[!grepl("\\(Código\\)$", cab)], conhecidas)[1]
  tibble(
    tabela       = as.integer(tabela),
    id_municipio = df[[col_mun]],
    ano          = as.numeric(df[[col_ano]]),
    variavel     = df[[col_var]],
    unidade      = df[[col_uni]],
    produto      = df[[col_prod]],
    valor_txt    = df[["Valor"]]
  ) %>%
    mutate(
      valor    = suppressWarnings(as.numeric(valor_txt)),
      situacao = unname(ifelse(valor_txt %in% names(SIDRA_ESPECIAIS),
                               SIDRA_ESPECIAIS[valor_txt], "informado")),
      valor    = ifelse(valor_txt == "-", 0, valor)
    )
}

# Anos disponíveis de uma tabela (algumas, como a 3940, só começam em 2013)
sidra_periodos <- function(tabela) {
  meta <- sidra_baixar(sprintf("https://servicodados.ibge.gov.br/api/v3/agregados/%s/periodos", tabela))
  sort(as.numeric(meta$id))
}

extrair_sidra <- function(tabelas, ano_ini, uf_codigo, ano_fim = "auto") {
  map_dfr(unique(tabelas), function(tab) {
    disp <- sidra_periodos(tab)
    fim  <- if (identical(ano_fim, "auto")) max(disp) else as.numeric(ano_fim)
    anos <- disp[disp >= ano_ini & disp <= fim]
    cl <- sidra_classificacao(tab)
    map_dfr(anos, function(a) {
      msg("SIDRA tabela ", tab, " ano ", a)
      sidra_arrumar(sidra_baixar(sidra_url(tab, a, uf_codigo, cl)), tab)
    })
  })
}

# Mantém só os produtos de cada cadeia (filtro por nome via expressão regular)
sidra_por_cadeia <- function(sidra, cadeias_sidra) {
  if (nrow(sidra) == 0 || nrow(cadeias_sidra) == 0) return(tibble())
  pmap_dfr(cadeias_sidra, function(cadeia_id, tabela, padrao) {
    sidra %>%
      filter(.data$tabela == !!tabela, !sidra_subtotais(produto),
             str_detect(produto, regex(padrao, ignore_case = TRUE))) %>%
      mutate(cadeia_id = cadeia_id)
  })
}

# ---------------------------------------------------------------------------
# PIB dos Municípios: valor adicionado por setor direto do IBGE (tabela 5938)
# ---------------------------------------------------------------------------
# Nos anos em que a divisão por setor está vazia na Base dos Dados, tenta o
# SIDRA. Se o IBGE também não divulgou (valor "..."), nada muda: o PIB total e
# o per capita seguem até o último ano e as páginas dizem até que ano existe a
# divisão por setor. Em novembro de 2026 era o caso de 2022 e 2023.
completar_va_sidra <- function(pib_pop, uf_codigo = 26) {
  cols <- c("va_agropecuaria", "va_industria", "va_servicos", "va_adespss")
  anos <- pib_pop %>% group_by(ano) %>% summarise(falta = all(is.na(va_industria)), .groups = "drop") %>% filter(falta) %>% pull(ano)
  if (!length(anos)) return(pib_pop)
  url <- sprintf("https://apisidra.ibge.gov.br/values/t/5938/n6/in%%20n3%%20%s/v/513,517,6575,525/p/%s", uf_codigo, paste(anos, collapse = ","))
  raw <- tryCatch(sidra_baixar(url, tentativas = 2), error = function(e) { msg("PIB por setor no SIDRA: ", conditionMessage(e)); NULL })
  novo <- va_sidra_arrumar(raw, n_mun = n_distinct(pib_pop$id_municipio))
  if (!nrow(novo)) { msg("PIB por setor: o IBGE ainda não divulgou ", paste(anos, collapse = " e "), "."); return(pib_pop) }
  msg("PIB por setor completado pelo SIDRA: ", paste(sort(unique(novo$ano)), collapse = ", "))
  j <- pib_pop %>% left_join(novo, by = c("ano", "id_municipio"), suffix = c("", ".sidra"))
  for (k in cols) if (paste0(k, ".sidra") %in% names(j)) j[[k]] <- coalesce(j[[k]], j[[paste0(k, ".sidra")]])
  j %>% select(-ends_with(".sidra"))
}

# Resposta da tabela 5938 (primeira linha = cabeçalho) em uma linha por ano e
# município, em reais. Só entram os anos com os quatro setores em quase todos
# os municípios.
va_sidra_arrumar <- function(raw, n_mun = 185) {
  vazio <- tibble(ano = numeric(), id_municipio = character())
  if (is.null(raw) || NROW(raw) < 2) return(vazio)
  cab <- as.character(raw[1, ]); df <- raw[-1, , drop = FALSE]
  col <- function(rotulo) as.character(df[[which(cab == rotulo)[1]]])
  t <- tibble(id_municipio = col("Município (Código)"), ano = as.numeric(col("Ano")), variavel = col("Variável"),
              unidade = col("Unidade de Medida"), valor = suppressWarnings(as.numeric(col("Valor")))) %>%
    filter(!is.na(valor)) %>%
    mutate(valor = valor * ifelse(grepl("^Mil", unidade), 1000, 1),
           campo = case_when(grepl("agropecuária", variavel) ~ "va_agropecuaria", grepl("da indústria", variavel) ~ "va_industria",
                             grepl("dos serviços", variavel) ~ "va_servicos", grepl("administração", variavel) ~ "va_adespss")) %>%
    filter(!is.na(campo))
  if (!nrow(t)) return(vazio)
  completos <- t %>% count(ano, campo) %>% group_by(ano) %>% summarise(ok = n() == 4 && all(n >= 0.9 * n_mun), .groups = "drop") %>% filter(ok) %>% pull(ano)
  t %>% filter(ano %in% completos) %>% select(ano, id_municipio, campo, valor) %>% tidyr::pivot_wider(names_from = campo, values_from = valor)
}
