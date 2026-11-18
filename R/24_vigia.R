# =============================================================================
# Vigia das fontes
#
# Depois de cada extração, confere se cada fonte veio com dados e se o dado
# mais recente não está velho demais. Foi o que faltou em 11/2026: o CNPJ da
# Base dos Dados mudou de tabela e voltou vazio, e o Novo CAGED ficou parado
# em março, e o pipeline seguiu em silêncio nos dois casos.
#
# Situação de cada fonte:
#   ok      dados presentes e dentro da defasagem esperada
#   parada  o dado mais recente é mais velho que a defasagem máxima
#   vazia   a extração não trouxe nenhuma linha
# O resultado vai para o log (mensagem e aviso do targets) e para a página
# "O que mudou".
# =============================================================================

# Defasagem máxima aceitável, em meses, entre o mês de referência do dado mais
# recente e o mês corrente. NA = só confere se veio vazia.
FONTES_VIGIADAS <- tibble::tribble(
  ~alvo,               ~fonte,                                   ~max_meses,
  "bq_rais_pe",        "RAIS (emprego formal anual)",              24,
  "bq_caged_pe",       "Novo CAGED (emprego formal mensal)",        3,
  "bq_comex_pe",       "ComexStat (exportações e importações)",    14,
  "bq_pib_pop",        "PIB dos Municípios (IBGE)",                48,
  "sidra_bruto",       "Produção agropecuária (PAM, PPM, SIDRA)",  30,
  "macro_ibcr",        "IBC-R (Banco Central)",                     5,
  "macro_pim",         "Produção industrial (PIM-PF)",              5,
  "macro_pmc",         "Comércio (PMC)",                            5,
  "macro_pms",         "Serviços (PMS)",                            5,
  "bq_cnpj_cadeias",   "CNPJ: empresas por cadeia",                NA,
  "bq_associativas",   "CNPJ: cooperativas e associações",         NA,
  "bq_cursos_nome",    "Censo da Educação Superior",               36,
  "bq_tecnico_cursos", "Censo Escolar (cursos técnicos)",          30,
  "camada_territorial","Camada territorial (IBGE, SUDENE, ANA)",   NA
)

# Data de referência do dado mais recente de um objeto (tabela ou lista de tabelas)
referencia_dado <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    tabs <- Filter(is.data.frame, x)
    if (!length(tabs)) return(as.Date(NA))
    x <- tabs[[which.max(vapply(tabs, NROW, 1L))]]
  }
  if (!is.data.frame(x) || !NROW(x)) return(as.Date(NA))
  datas <- names(x)[vapply(x, function(c) inherits(c, c("Date", "POSIXt")), TRUE)]
  if (length(datas)) return(as.Date(max(x[[datas[1]]], na.rm = TRUE)))
  if ("data_referencia" %in% names(x)) return(suppressWarnings(as.Date(max(as.character(x$data_referencia), na.rm = TRUE))))
  if (all(c("ano", "mes") %in% names(x))) {
    a <- suppressWarnings(as.integer(x$ano)); m <- suppressWarnings(as.integer(x$mes))
    u <- max(a * 100 + m, na.rm = TRUE)
    return(as.Date(sprintf("%d-%02d-01", u %/% 100, u %% 100)))
  }
  if ("ano" %in% names(x)) return(as.Date(sprintf("%d-12-01", max(suppressWarnings(as.integer(x$ano)), na.rm = TRUE))))
  as.Date(NA)
}

linhas_dado <- function(x) {
  if (is.data.frame(x)) return(NROW(x))
  if (is.list(x)) return(sum(vapply(Filter(is.data.frame, x), NROW, 1L)))
  if (is.null(x)) return(0L)
  length(x)
}

# objs: lista nomeada com os alvos (nomes iguais aos de FONTES_VIGIADAS$alvo)
vigiar_fontes <- function(objs, regras = FONTES_VIGIADAS, hoje = Sys.Date()) {
  mes_atual <- as.Date(format(hoje, "%Y-%m-01"))
  meses_entre <- function(d) ifelse(is.na(d), NA_integer_,
    (as.integer(format(mes_atual, "%Y")) - as.integer(format(d, "%Y"))) * 12L + as.integer(format(mes_atual, "%m")) - as.integer(format(d, "%m")))
  out <- purrr::map_dfr(seq_len(nrow(regras)), function(i) {
    r <- regras[i, ]
    x <- objs[[r$alvo]]
    n <- tryCatch(linhas_dado(x), error = function(e) 0L)
    ref <- if (n > 0) tryCatch(referencia_dado(x), error = function(e) as.Date(NA)) else as.Date(NA)
    tibble::tibble(fonte = r$fonte, alvo = r$alvo, linhas = n, referencia = ref, meses = meses_entre(ref), max_meses = r$max_meses)
  })
  out <- out %>% mutate(
    situacao = case_when(linhas == 0 ~ "vazia",
                         !is.na(max_meses) & !is.na(meses) & meses > max_meses ~ "parada",
                         TRUE ~ "ok"),
    nota = case_when(situacao == "vazia" ~ "a extração não trouxe nenhuma linha: ver se a tabela de origem mudou de nome ou de colunas",
                     situacao == "parada" ~ paste0("o dado mais recente é de ", format(referencia, "%m/%Y"), ", ", meses, " meses atrás (o esperado é até ", max_meses, ")"),
                     TRUE ~ ""),
    conferido_em = hoje)
  prob <- out %>% filter(situacao != "ok")
  if (nrow(prob)) {
    for (i in seq_len(nrow(prob))) msg("ALERTA de fonte ", toupper(prob$situacao[i]), ": ", prob$fonte[i], " (", prob$alvo[i], "): ", prob$nota[i])
    warning(paste0(nrow(prob), " fonte(s) com problema: ", paste(prob$fonte, collapse = "; ")), call. = FALSE)
  } else {
    msg("Vigia das fontes: todas as ", nrow(out), " fontes vieram com dados e em dia.")
  }
  out
}

# Bloco para as páginas: aviso no topo quando há problema, tabela completa embaixo
bloco_vigia <- function(v, completo = TRUE) {
  if (is.null(v) || !NROW(v)) return(NULL)
  prob <- v %>% filter(situacao != "ok")
  aviso <- if (nrow(prob)) htmltools::div(class = "aviso",
    htmltools::strong(paste0("Atenção: ", nrow(prob), if (nrow(prob) == 1) " fonte veio" else " fontes vieram", " com problema nesta atualização. ")),
    htmltools::tags$ul(lapply(seq_len(nrow(prob)), function(i) htmltools::tags$li(paste0(prob$fonte[i], ": ", prob$nota[i], "."))))) else NULL
  if (!completo) return(aviso)
  tab <- v %>% transmute(Fonte = fonte,
                         `Dado mais recente` = ifelse(is.na(referencia), "–", format(referencia, "%m/%Y")),
                         Linhas = fmt_num(linhas),
                         `Situação` = c(ok = "em dia", parada = "parada", vazia = "vazia")[situacao])
  htmltools::tagList(aviso, htmltools::HTML(knitr::kable(tab, format = "html", align = "llrl", table.attr = "class='table table-sm'")),
                     htmltools::p(class = "fonte", paste0("Conferido em ", format(v$conferido_em[1], "%d/%m/%Y"),
                                                         ". Para fontes anuais, a referência é dezembro do último ano disponível.")))
}
