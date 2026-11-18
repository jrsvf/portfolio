# =============================================================================
# Camada territorial: o que o emprego não mostra sobre a capacidade produtiva
#
# Por município, de fontes oficiais em planilha (baixadas uma vez e guardadas
# em dados_externos/territorio/):
#   Semiárido (SUDENE/IBGE, delimitação vigente, lista de 2022)
#   Tipologia rural-urbana (IBGE, 2017): urbano, intermediário, rural,
#     adjacente ou remoto
#   Índice de acessibilidade geográfica (IBGE, 2018): distância e custo de
#     chegar aos centros que oferecem bens e serviços
#   Hierarquia urbana (REGIC 2018, IBGE): metrópole, capital regional,
#     centro sub-regional, centro de zona, centro local
#   Segurança hídrica do abastecimento urbano (Atlas Águas 2021, ANA)
# As planilhas mudam de formato entre edições: a leitura procura a coluna do
# código do município e as colunas pelo conteúdo, e registra no log o que achou.
# =============================================================================

FONTES_TERRITORIO <- tibble::tribble(
  ~chave,          ~arquivo,                               ~url,
  "semiarido",     "semiarido_2022.xlsx",                  "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/semiarido_brasileiro/Situacao_2022/lista_municipios_Semiarido_2022.xlsx",
  "tipologia",     "tipologia_rural_urbana_2017.xlsx",     "https://geoftp.ibge.gov.br/organizacao_do_territorio/tipologias_do_territorio/classificacao_rural_e_urbana/classificacao_e_caracterizacao_dos_espacos_rurais_e_urbanos_do_brasil_2017/tabelas/Tipologia_municipal_rural_urbano.xlsx",
  "acessibilidade","acessibilidade_geografica_2018.xlsx",  "https://geoftp.ibge.gov.br/organizacao_do_territorio/tipologias_do_territorio/indice_de_acessibilidade_geografica_2018/tabelas/AcessibilidadeGeografica_xlsx.xlsx",
  "regic",         "regic_2018_hierarquia.xlsx",           "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/regioes_de_influencia_das_cidades/Regioes_de_influencia_das_cidades_2018_Resultados_definitivos/base_tabular/REGIC2018_Municipios_Hierarquia_e_regiao.xlsx",
  "agua",          "atlas_aguas_2021_seguranca.xlsx",      "https://metadados.snirh.gov.br/geonetwork/srv/api/records/d77a2d01-0578-4c71-a57e-87f5c565aacf/attachments/AtlasAguas_Indicadores_SegurancaHidrica.xlsx"
)

# Baixa o que faltar e devolve os caminhos (os já baixados não são baixados de novo)
baixar_territorio <- function(pasta = caminho("dados_externos", "territorio"), fontes = FONTES_TERRITORIO) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  op <- options(timeout = max(300, getOption("timeout"))); on.exit(options(op), add = TRUE)
  arqs <- file.path(pasta, fontes$arquivo)
  for (i in seq_along(arqs)) if (!file.exists(arqs[i]) || file.size(arqs[i]) < 1000) {
    ok <- tryCatch({ utils::download.file(fontes$url[i], arqs[i], mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) { msg("Camada territorial: não baixou ", fontes$chave[i], ": ", conditionMessage(e)); FALSE })
    if (!ok) unlink(arqs[i])
  }
  setNames(ifelse(file.exists(arqs), arqs, NA_character_), fontes$chave)
}

# Lê uma planilha qualquer e devolve as linhas da UF, com id_municipio (7 dígitos) e
# as demais colunas como texto, com os nomes do cabeçalho que estiver logo acima dos dados.
ler_planilha_municipios <- function(arq, uf_codigo) {
  if (is.na(arq) || !file.exists(arq)) return(NULL)
  abas <- tryCatch(readxl::excel_sheets(arq), error = function(e) character())
  melhor <- NULL
  for (aba in abas) {
    x <- tryCatch(suppressMessages(readxl::read_excel(arq, sheet = aba, col_names = FALSE, col_types = "text")), error = function(e) NULL)
    if (is.null(x) || !nrow(x)) next
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    cod <- vapply(x, function(v) sum(grepl(paste0("^", uf_codigo, "[0-9]{5}$"), gsub("\\.0$", "", trimws(v)))), 0)
    if (max(cod) < 20) next
    j <- which.max(cod)
    # com mais de uma coluna de códigos (ex.: município e cidade do arranjo no REGIC), fica a do município
    cand <- which(cod >= 0.9 * max(cod))
    if (length(cand) > 1) {
      l0 <- gsub("\\.0$", "", trimws(x[[j]])); h0 <- max(1, min(which(grepl("^[1-5][0-9]{6}$", l0))) - 1)
      cab <- vapply(cand, function(k) tolower(iconv(as.character(x[[k]][h0]), "UTF-8", "ASCII//TRANSLIT")), "")
      pref <- cand[grepl("mun|geoc|ibge", cab) & !grepl("cid|arranj|ap$", cab)]
      if (length(pref)) j <- pref[1]
    }
    limpo <- gsub("\\.0$", "", trimws(x[[j]]))
    linhas <- which(grepl(paste0("^", uf_codigo, "[0-9]{5}$"), limpo))
    # cabeçalho: a linha logo acima do primeiro código de município de qualquer UF (as planilhas trazem o Brasil todo)
    h <- max(1, min(which(grepl("^[1-5][0-9]{6}$", limpo))) - 1)
    nomes <- vapply(seq_along(x), function(k) { n <- x[[k]][h]; if (is.na(n) || !nzchar(trimws(n))) paste0("col", k) else trimws(n) }, "")
    d <- x[linhas, , drop = FALSE]; names(d) <- make.unique(nomes)
    d$id_municipio <- gsub("\\.0$", "", trimws(x[[j]][linhas]))
    if (is.null(melhor) || nrow(d) > nrow(melhor)) melhor <- d
  }
  if (is.null(melhor)) return(NULL)
  tibble::as_tibble(melhor)
}

# Coluna cujo conteúdo casa com o padrão (a de mais acertos)
coluna_por_conteudo <- function(d, padrao) {
  if (is.null(d)) return(NA_character_)
  n <- vapply(d, function(v) sum(grepl(padrao, v, ignore.case = TRUE)), 0)
  n[names(d) == "id_municipio"] <- 0
  if (max(n) < 5) NA_character_ else names(d)[which.max(n)]
}
coluna_por_nome <- function(d, padrao, numerica = FALSE) {
  if (is.null(d)) return(NA_character_)
  ok <- grepl(padrao, iconv(names(d), "UTF-8", "ASCII//TRANSLIT"), ignore.case = TRUE)
  if (numerica) ok <- ok & vapply(d, function(v) mean(!is.na(suppressWarnings(as.numeric(gsub(",", ".", v))))) > 0.8, TRUE)
  if (any(ok)) names(d)[which(ok)[1]] else NA_character_
}
num <- function(v) suppressWarnings(as.numeric(gsub(",", ".", v)))

montar_camada_territorial <- function(arqs, municipios, rd, uf_codigo) {
  base <- municipios %>% transmute(id_municipio = as.character(id_municipio), municipio) %>%
    left_join(rd %>% transmute(id_municipio = as.character(id_municipio), rd), by = "id_municipio")
  achados <- character()
  # semiárido: estar na lista
  s <- ler_planilha_municipios(arqs[["semiarido"]], uf_codigo)
  base$semiarido <- if (is.null(s)) NA else base$id_municipio %in% s$id_municipio
  achados <- c(achados, paste0("semiárido: ", if (is.null(s)) "planilha não lida" else paste(nrow(s), "municípios de PE na lista")))
  # tipologia rural-urbana
  t <- ler_planilha_municipios(arqs[["tipologia"]], uf_codigo)
  ct <- coluna_por_nome(t, "^tipo"); if (is.na(ct)) ct <- coluna_por_conteudo(t, "^(urbano|intermedi|rural)")
  base <- base %>% left_join(if (!is.na(ct)) t %>% transmute(id_municipio, tipologia = stringr::str_squish(.data[[ct]])) else tibble(id_municipio = character(), tipologia = character()), by = "id_municipio")
  achados <- c(achados, paste0("tipologia: coluna ", ct))
  # acessibilidade geográfica (IBGE 2018): classe (TipoSen_4c), coeficiente em relação à metrópole e distância
  a <- ler_planilha_municipios(arqs[["acessibilidade"]], uf_codigo)
  ca <- coluna_por_nome(a, "^coefmet|^coef", numerica = TRUE)
  cc <- coluna_por_nome(a, "^tipo"); if (is.na(cc)) cc <- coluna_por_conteudo(a, "acess[ií]vel|remoto")
  cd <- coluna_por_nome(a, "^d_metrop", numerica = TRUE)
  base <- base %>% left_join(if (!is.null(a)) a %>% transmute(id_municipio,
                                                              acessibilidade = if (!is.na(ca)) num(.data[[ca]]) else NA_real_,
                                                              acessibilidade_classe = if (!is.na(cc)) stringr::str_squish(.data[[cc]]) else NA_character_,
                                                              distancia_metropole = if (!is.na(cd)) num(.data[[cd]]) else NA_real_) %>% distinct(id_municipio, .keep_all = TRUE)
                             else tibble(id_municipio = character(), acessibilidade = numeric(), acessibilidade_classe = character(), distancia_metropole = numeric()), by = "id_municipio")
  achados <- c(achados, paste0("acessibilidade: índice em ", ca, ", classe em ", cc, ", distância em ", cd))
  # REGIC 2018: hierarquia urbana
  g <- ler_planilha_municipios(arqs[["regic"]], uf_codigo)
  cg <- coluna_por_nome(g, "^hierarquia$"); if (is.na(cg)) cg <- coluna_por_conteudo(g, "^(metr[oó]pole|capital regional|centro sub|centro de zona|centro local)")
  base <- base %>% left_join(if (!is.na(cg)) g %>% transmute(id_municipio, hierarquia = stringr::str_squish(.data[[cg]])) %>% distinct(id_municipio, .keep_all = TRUE)
                             else tibble(id_municipio = character(), hierarquia = character()), by = "id_municipio")
  achados <- c(achados, paste0("REGIC: hierarquia em ", cg))
  # água (Atlas Águas 2021, ANA): ISH do abastecimento urbano, sistema produtor e manancial
  w <- ler_planilha_municipios(arqs[["agua"]], uf_codigo)
  cw <- coluna_por_nome(w, "seguranca hidrica|ish"); if (is.na(cw)) cw <- coluna_por_conteudo(w, "^(m[ií]nim|baix|m[eé]di|alt|m[aá]xim)[oa]?$")
  cs <- coluna_por_nome(w, "sistema produtor"); if (is.na(cs)) cs <- coluna_por_conteudo(w, "satisfat|amplia|adequa")
  cm <- coluna_por_nome(w, "manancial")
  base <- base %>% left_join(if (!is.null(w)) w %>% transmute(id_municipio,
                                                              seguranca_hidrica = if (!is.na(cw)) stringr::str_squish(.data[[cw]]) else NA_character_,
                                                              abastecimento = if (!is.na(cs)) stringr::str_squish(.data[[cs]]) else NA_character_,
                                                              manancial = if (!is.na(cm)) stringr::str_squish(.data[[cm]]) else NA_character_) %>% distinct(id_municipio, .keep_all = TRUE)
                             else tibble(id_municipio = character(), seguranca_hidrica = character(), abastecimento = character(), manancial = character()), by = "id_municipio")
  achados <- c(achados, paste0("água: ISH em ", cw, ", sistema produtor em ", cs, ", manancial em ", cm))
  msg("Camada territorial: ", paste(achados, collapse = "; "))
  attr(base, "leitura") <- achados
  base
}

# Mapa de categorias (para as variáveis territoriais)
mapa_categorias <- function(malha, dados, coluna, cores, rotulo, municipios) {
  m <- malha %>% mutate(id_municipio = as.character(id_municipio)) %>% left_join(dados %>% select(id_municipio, v = all_of(coluna)), by = "id_municipio")
  v <- as.character(m$v)
  niveis <- names(cores)
  cor <- ifelse(is.na(v) | !v %in% niveis, "#F2F3F4", cores[v])
  rot <- sprintf("<strong>%s</strong><br/>%s: %s", m$name_muni, rotulo, ifelse(is.na(v), "sem informação", v)) %>% lapply(htmltools::HTML)
  bb <- tryCatch(as.numeric(sf::st_bbox(m[m$id_municipio != "2605459", ])), error = function(e) NULL)
  mp <- leaflet::leaflet(m, options = leaflet::leafletOptions(zoomSnap = 0.25))
  if (length(bb) == 4) mp <- mp %>% leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
  mp %>% leaflet::addPolygons(fillColor = cor, weight = 0.6, color = "white", fillOpacity = 0.9, label = rot,
                              highlightOptions = leaflet::highlightOptions(weight = 2, color = "#2C3E50", bringToFront = TRUE)) %>%
    leaflet::addLegend("bottomright", colors = unname(cores), labels = niveis, title = rotulo, opacity = 1)
}

# Pedaço da camada territorial para as páginas de município e região (cartões: rótulo, valor, nota)
rotulo_tipologia <- function(x) {
  x <- gsub("([a-zá-ú])([A-ZÁ-Ú])", "\\1 \\2", x)
  x <- stringr::str_to_sentence(gsub("Intermediario", "Intermediário", x))
  x
}
rotulo_hierarquia <- function(x) stringr::str_squish(sub(" - .*$", "", x))
pedaco_territorio <- function(ct, ids, municipios) {
  if (is.null(ct) || !NROW(ct)) return(NULL)
  x <- ct %>% filter(id_municipio %in% ids)
  if (!nrow(x)) return(NULL)
  nf <- function(v) format(round(v), big.mark = ".", decimal.mark = ",")
  if (length(ids) == 1) {
    arr <- if (!is.na(x$hierarquia) && grepl("Arranjo Populacional", x$hierarquia)) sub(".*(Arranjo Populacional [^)]*)$", "\\1", x$hierarquia) else ""
    return(list(
      list(rotulo = "Semiárido", valor = if (isTRUE(x$semiarido)) "Sim" else if (isFALSE(x$semiarido)) "Não" else "–", nota = "delimitação da SUDENE"),
      list(rotulo = "Tipo de município (IBGE)", valor = if (is.na(x$tipologia)) "–" else rotulo_tipologia(x$tipologia), nota = "rural, intermediário ou urbano; adjacente ou remoto"),
      list(rotulo = "Papel na rede de cidades (REGIC)", valor = if (is.na(x$hierarquia)) "–" else rotulo_hierarquia(x$hierarquia), nota = if (nzchar(arr)) paste("integra o", arr) else ""),
      list(rotulo = "Acesso aos centros (IBGE)", valor = if (is.na(x$acessibilidade_classe)) "–" else x$acessibilidade_classe,
           nota = if (!is.na(x$distancia_metropole) && x$distancia_metropole > 0) paste0(nf(x$distancia_metropole), " km da metrópole") else ""),
      list(rotulo = "Segurança hídrica do abastecimento (ANA)", valor = if (is.na(x$seguranca_hidrica)) "–" else x$seguranca_hidrica,
           nota = paste(c(if (!is.na(x$abastecimento)) paste("sistema produtor:", tolower(x$abastecimento)), if (!is.na(x$manancial)) tolower(x$manancial)), collapse = "; "))))
  }
  conta <- function(v, k) sum(grepl(k, v, ignore.case = TRUE))
  nm <- municipios %>% transmute(id_municipio = as.character(id_municipio), municipio)
  # um arranjo populacional (ex.: Recife e vizinhos, Petrolina e Juazeiro) conta como um centro só
  if (!"municipio" %in% names(x)) x <- x %>% left_join(nm, by = "id_municipio")
  centros <- x %>% filter(grepl("metr|capital|sub-?regional", hierarquia, ignore.case = TRUE)) %>%
    mutate(h = rotulo_hierarquia(hierarquia),
           municipio = ifelse(grepl("Arranjo Populacional", hierarquia), sub(".*Arranjo Populacional (de |do |da )?", "", hierarquia), municipio)) %>%
    distinct(municipio, h) %>% arrange(h)
  list(
    list(rotulo = "Municípios no semiárido", valor = paste(sum(x$semiarido %in% TRUE), "de", nrow(x)), nota = "delimitação da SUDENE"),
    list(rotulo = "Municípios rurais (IBGE)", valor = paste(conta(x$tipologia, "^rural"), "de", nrow(x)),
         nota = paste0(conta(x$tipologia, "^urbano"), if (conta(x$tipologia, "^urbano") == 1) " urbano e " else " urbanos e ",
                       conta(x$tipologia, "^intermedi"), if (conta(x$tipologia, "^intermedi") == 1) " intermediário" else " intermediários")),
    list(rotulo = "Centros regionais (REGIC)", valor = as.character(nrow(centros)),
         nota = if (nrow(centros)) paste0(paste0(centros$municipio, " (", tolower(centros$h), ")"), collapse = ", ") else "nenhum centro sub-regional ou maior"),
    list(rotulo = "Distância média à metrópole", valor = if (all(is.na(x$distancia_metropole))) "–" else paste(nf(mean(x$distancia_metropole, na.rm = TRUE)), "km"),
         nota = paste(conta(x$acessibilidade_classe, "remoto|pouco"), "municípios pouco acessíveis ou remotos")),
    list(rotulo = "Segurança hídrica baixa ou mínima", valor = paste(conta(x$seguranca_hidrica, "^(baix|m[ií]n)"), "de", nrow(x)),
         nota = paste(conta(x$abastecimento, "amplia"), "precisam ampliar o sistema produtor de água")))
}
