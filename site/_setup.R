# Carregado no início de cada página: acha a raiz do projeto, carrega as
# funções e as bases geradas pelo pipeline (pasta dados/).
local({
  d <- normalizePath(getwd())
  while (!file.exists(file.path(d, "_targets.R"))) {
    p <- dirname(d)
    if (p == d) stop("Não encontrei a raiz do projeto (_targets.R).")
    d <- p
  }
  Sys.setenv(PASTA_PROJETO = d)
})
for (f in c("00_utils.R", "03_indicadores.R", "05_lacunas_destaques.R", "07_site.R", "08_comparacoes.R")) {
  source(file.path(Sys.getenv("PASTA_PROJETO"), "R", f), encoding = "UTF-8")
}
local({
  p <- yaml::read_yaml(file.path(Sys.getenv("PASTA_PROJETO"), "config", "parametros.yml"))$comparacoes
  if (!is.null(p)) CORTES_PADRAO <<- list(janelas = p$janelas, desde = p$desde)
})
suppressPackageStartupMessages({ library(plotly); library(leaflet); library(sf); library(htmltools) })

carregar <- function(...) {
  nomes <- c(...)
  for (n in nomes) assign(n, ler_base(n, "targets::tar_make() na raiz do projeto"), envir = globalenv())
}
carregar("par", "cad", "municipios", "malha", "atualizado_em", "ultimos_anos")
nome_mun <- function(id) municipios$municipio[match(id, municipios$id_municipio)]
rodape_fonte <- function(txt) htmltools::div(class = "fonte", paste0("Fonte: ", txt, ". Elaboração própria."))

# Tabela "Dados usados": período mostrado nos gráficos (últimos N anos) e
# série completa disponível na fonte (que segue nos CSVs para download).
tabela_fontes <- function(fd, acesso = TRUE) {
  jan <- max(CORTES_PADRAO$janelas)
  usado <- vapply(fd$cobertura, function(x) {
    m <- regmatches(x, regexec("^(\\d{4}) a (\\d{4})(.*)$", x))[[1]]
    if (length(m) == 0) return(x)
    a <- as.integer(m[2]); b <- as.integer(m[3])
    sprintf("%d a %d%s", max(a, b - jan), b, m[4])
  }, character(1), USE.NAMES = FALSE)
  usado[grepl("IPCA|insumo", fd$fonte)] <- fd$cobertura[grepl("IPCA|insumo", fd$fonte)]
  t <- fd %>% transmute(Fonte = fonte, `Órgão` = orgao, `Período nesta análise` = usado,
                        `Série disponível na fonte` = cobertura, Acesso = onde)
  if (!acesso) t <- t %>% select(-Acesso)
  knitr::kable(t)
}
