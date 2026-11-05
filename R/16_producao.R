# =============================================================================
# Produção agropecuária por território (IBGE: PAM, PPM, aquicultura)
#
# Todos os produtos das tabelas do SIDRA já baixadas pelo pipeline, no último
# ano de cada tabela, por município e por RD: valor da produção, quantidade e
# QL do valor da produção (peso do produto no valor agropecuário do território
# dividido pelo peso no estado). Rebanhos (efetivo) entram sem QL.
# =============================================================================

GRUPO_SIDRA <- c("5457" = "Lavouras", "74" = "Produtos de origem animal", "3940" = "Aquicultura", "3939" = "Rebanhos (efetivo)",
                 "289" = "Extração vegetal", "291" = "Silvicultura")

# produtos que são soma ou parte de outros: totais, café arábica e canéfora (dentro do
# total do café), "Peixes" (soma das espécies), matrizes de suínos e galinhas (dentro dos totais)
sidra_subtotais <- function(produto) grepl("^Total$|^Café \\(em grão\\) (Arábica|Canephora)|^Peixes$|matrizes de suínos|^Galináceos - galinhas", produto)

montar_producao <- function(sidra_bruto, rd) {
  if (!NROW(sidra_bruto)) return(NULL)
  s <- sidra_bruto %>% filter(!sidra_subtotais(produto), !is.na(valor)) %>%
    group_by(tabela) %>% filter(ano == max(ano)) %>% ungroup()
  val <- s %>% filter(variavel == "Valor da produção") %>% group_by(tabela, produto, id_municipio, ano) %>% summarise(valor_mil = sum(valor), .groups = "drop")
  qtd <- s %>% filter(variavel %in% c("Quantidade produzida", "Produção de origem animal", "Produção da aquicultura", "Efetivo dos rebanhos")) %>%
    group_by(tabela, produto, id_municipio) %>% summarise(qtd = sum(valor), unidade = first(unidade), .groups = "drop")
  x <- full_join(val, qtd, by = c("tabela", "produto", "id_municipio")) %>%
    mutate(produto = sub("\\*$", "", produto), produto = sub(" Total$", "", produto), grupo = unname(GRUPO_SIDRA[as.character(tabela)])) %>%
    filter(coalesce(valor_mil, 0) > 0 | coalesce(qtd, 0) > 0)
  qlv <- function(d, chave) {
    tot_t <- d %>% group_by(across(all_of(chave))) %>% summarise(tot_t = sum(valor_mil, na.rm = TRUE), .groups = "drop")
    tot_p <- d %>% group_by(tabela, produto) %>% summarise(tot_p = sum(valor_mil, na.rm = TRUE), .groups = "drop")
    T <- sum(d$valor_mil, na.rm = TRUE)
    d %>% left_join(tot_t, by = chave) %>% left_join(tot_p, by = c("tabela", "produto")) %>%
      mutate(ql = ifelse(!is.na(valor_mil) & valor_mil >= 50 & tot_t > 0 & tot_p > 0, (valor_mil / tot_t) / (tot_p / T), NA_real_)) %>%
      select(-tot_t, -tot_p)
  }
  mun <- qlv(x, "id_municipio")
  r <- x %>% left_join(rd %>% select(id_municipio, rd), by = "id_municipio") %>% filter(!is.na(rd)) %>%
    group_by(tabela, grupo, produto, rd) %>% summarise(valor_mil = sum(valor_mil, na.rm = TRUE), qtd = sum(qtd, na.rm = TRUE), unidade = first(unidade), .groups = "drop") %>%
    mutate(valor_mil = ifelse(valor_mil > 0, valor_mil, NA)) %>% qlv("rd")
  prod <- x %>% group_by(tabela, grupo, produto) %>%
    summarise(valor_pe = sum(valor_mil, na.rm = TRUE), qtd_pe = sum(qtd, na.rm = TRUE), unidade = first(na.omit(unidade)),
              n_mun = n_distinct(id_municipio), ano = suppressWarnings(max(ano, na.rm = TRUE)), .groups = "drop") %>%
    mutate(ano = ifelse(is.finite(ano), ano, NA)) %>% arrange(desc(valor_pe))
  list(produtos = prod, municipios = mun, regioes = r, anos = sort(unique(x$ano[!is.na(x$ano)])))
}

exportar_producao <- function(p, destino = caminho("site", "dados", "producao.json")) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (is.null(p)) { jsonlite::write_json(list(), destino); return(destino) }
  a <- p$produtos; chave <- paste(a$tabela, a$produto)
  idx <- setNames(seq_len(nrow(a)) - 1L, chave)
  r2 <- function(z) round(z, 2)
  lin <- function(d) unname(as.matrix(data.frame(idx[paste(d$tabela, d$produto)], round(coalesce(d$valor_mil, 0)), round(coalesce(d$qtd, 0)), r2(d$ql))))
  pm <- p$municipios %>% arrange(id_municipio, desc(coalesce(valor_mil, 0)))
  pr <- p$regioes %>% arrange(rd, desc(coalesce(valor_mil, 0)))
  obj <- list(anos = p$anos,
              produtos = list(nome = a$produto, grupo = a$grupo, unidade = a$unidade, valor = round(a$valor_pe), qtd = round(a$qtd_pe), n_mun = a$n_mun, ano = a$ano),
              # [produto, valor da produção (mil R$), quantidade, QL do valor]
              municipios = lapply(split(pm, pm$id_municipio), lin),
              regioes = lapply(split(pr, pr$rd), lin))
  jsonlite::write_json(obj, destino, auto_unbox = TRUE, digits = NA, na = "null")
  destino
}
