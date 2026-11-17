# =============================================================================
# Formação: cursos por nome em cada município (superior presencial e a
# distância, técnico) e cursos novos que ainda não estão no Censo
#
# Fontes:
#   Censo da Educação Superior (INEP): curso por nome, município e modalidade
#   Censo Escolar (INEP): turmas de educação profissional pelo código do curso
#     técnico; o nome vem da lista oficial de cursos (config/cursos_tecnicos.csv,
#     baixada uma vez e guardada)
#   config/cursos_novos.csv: cursos que começaram depois do último Censo
#     publicado (ex.: UFPE em Sertânia, 2025), mantidos à mão com a fonte
# A ligação curso x cadeia usa as palavras de config/cadeia_cursos.csv.
# =============================================================================

# Lista oficial de cursos técnicos (código e nome), do Censo Escolar
URL_CURSOS_TECNICOS <- "https://dados.recife.pe.gov.br/dataset/821abdb7-f805-4a4b-8bfc-50a71b8dc3e6/resource/bb4f941b-fe84-44d8-ad30-15ea03d232cc/download/lista-de-cursos-da-educacao-profissional-tecnica.csv"

# Lê config/cursos_tecnicos.csv; se não existir, tenta baixar a lista e guarda.
# Sem a lista, os cursos técnicos aparecem só pelo código.
obter_cursos_tecnicos <- function(arq = caminho("config", "cursos_tecnicos.csv"), url = URL_CURSOS_TECNICOS) {
  vazio <- tibble(cod_curso = character(), curso = character(), eixo = character())
  if (!file.exists(arq)) {
    tmp <- tempfile(fileext = ".csv")
    ok <- tryCatch({ utils::download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE }, error = function(e) { msg("Lista de cursos técnicos: ", conditionMessage(e)); FALSE })
    if (!ok) return(vazio)
    linhas <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
    if (!any(validUTF8(linhas))) linhas <- iconv(linhas, "latin1", "UTF-8")
    linhas[!validUTF8(linhas)] <- iconv(linhas[!validUTF8(linhas)], "latin1", "UTF-8")
    sep <- if (lengths(regmatches(linhas[1], gregexpr(";", linhas[1]))) >= 2) ";" else ","
    df <- tryCatch(utils::read.table(text = linhas, sep = sep, header = TRUE, quote = "\"", colClasses = "character", fill = TRUE, comment.char = ""),
                   error = function(e) NULL)
    if (is.null(df) || ncol(df) < 2) return(vazio)
    nm <- tolower(names(df))
    cod <- df[[which(grepl("cod", nm))[1]]]; nome <- df[[which(grepl("curso", nm) & !grepl("cod", nm))[1]]]
    eixo <- if (any(grepl("eixo", nm))) df[[which(grepl("eixo", nm))[1]]] else NA_character_
    out <- tibble(cod_curso = gsub("\\D", "", cod), curso = stringr::str_squish(nome), eixo = stringr::str_squish(eixo)) %>% filter(nzchar(cod_curso))
    readr::write_csv(out, arq)
    msg("Lista de cursos técnicos guardada em ", arq, " (", nrow(out), " cursos).")
  }
  readr::read_csv(arq, col_types = "ccc") %>% mutate(cod_curso = gsub("\\D", "", cod_curso))
}

# Turmas de educação profissional do último ano do Censo Escolar, por curso
extrair_tecnico_cursos <- function(projeto, uf) {
  consultar(glue::glue("
    SELECT ano, id_municipio, CAST(rede AS STRING) AS rede, CAST(id_curso_educacao_profissional AS STRING) AS cod_curso,
           COUNT(DISTINCT id_escola) AS escolas, COUNT(*) AS turmas, SUM(quantidade_matriculas) AS matriculas
    FROM `basedosdados.br_inep_censo_escolar.turma`
    WHERE sigla_uf = '{uf}' AND ano = (SELECT MAX(ano) FROM `basedosdados.br_inep_censo_escolar.turma` WHERE sigla_uf = '{uf}')
      AND id_curso_educacao_profissional IS NOT NULL
      AND TRIM(CAST(id_curso_educacao_profissional AS STRING)) NOT IN ('', '0')
    GROUP BY 1, 2, 3, 4"), projeto) %>%
    mutate(across(c(ano, escolas, turmas, matriculas), as.numeric), id_municipio = as.character(id_municipio),
           cod_curso = gsub("\\D", "", cod_curso))
}

ler_cursos_novos <- function(arq = caminho("config", "cursos_novos.csv")) {
  if (!file.exists(arq)) return(tibble())
  readr::read_csv(arq, col_types = readr::cols(.default = "c")) %>% mutate(inicio = as.numeric(inicio), vagas = as.numeric(vagas))
}

# Cadeias de um nome de curso pelas palavras de config/cadeia_cursos.csv
cadeias_do_curso <- function(nomes, mapa) {
  n <- normalizar_txt(nomes)
  res <- vector("list", length(n))
  for (i in seq_len(nrow(mapa))) {
    ok <- grepl(mapa$padrao[i], n, perl = TRUE)
    if (nzchar(mapa$excluir[i])) ok <- ok & !grepl(mapa$excluir[i], n, perl = TRUE)
    for (j in which(ok)) res[[j]] <- c(res[[j]], mapa$cadeia_id[i])
  }
  vapply(res, function(x) paste(unique(x), collapse = ";"), "")
}

# Uma linha por curso em cada município, de todas as fontes
montar_formacao <- function(cursos_nome, tec_cursos, nomes_tec, cursos_novos, mapa, dic, municipios, rd) {
  rot <- function(x, col) rotular(x, dic, "br_inep_censo_educacao_superior", "curso", col)
  mod_txt <- function(x) ifelse(is.na(x) | x %in% c("1", "Presencial"), "Superior presencial", "Superior a distância")
  sup <- if (NROW(cursos_nome)) cursos_nome %>%
    mutate(modalidade = if ("modalidade" %in% names(.)) modalidade else "1",
           tipo = mod_txt(modalidade), grau = rot(grau, "tipo_grau_academico"),
           rede = ifelse(grepl("p[úu]blica|federal|estadual|municipal|especial", rot(adm, "tipo_organizacao_administrativa"), ignore.case = TRUE) |
                           adm %in% c("1", "2", "3", "7"), "Pública", "Privada")) %>%
    group_by(ano, id_municipio, tipo, curso = stringr::str_to_sentence(stringr::str_squish(nome_curso)), detalhe = grau, rede) %>%
    summarise(oferta = sum(cursos, na.rm = TRUE), matriculas = sum(matriculas, na.rm = TRUE), concluintes = sum(concluintes, na.rm = TRUE), .groups = "drop") else tibble()
  tec <- if (NROW(tec_cursos)) tec_cursos %>%
    left_join(nomes_tec %>% select(cod_curso, nome = curso, eixo), by = "cod_curso") %>%
    mutate(rede = rotular(rede, dic, "br_inep_censo_escolar", "turma", "rede"),
           rede = ifelse(grepl("privad", rede, ignore.case = TRUE) | rede == "4", "Privada", "Pública"),
           curso = coalesce(nome, paste0("Curso técnico (código ", cod_curso, ")")), tipo = "Técnico", detalhe = eixo) %>%
    group_by(ano, id_municipio, tipo, curso, detalhe, rede) %>%
    summarise(oferta = sum(escolas), matriculas = sum(matriculas, na.rm = TRUE), concluintes = NA_real_, .groups = "drop") else tibble()
  novos <- if (NROW(cursos_novos)) cursos_novos %>%
    transmute(ano = inicio, id_municipio = as.character(id_municipio), tipo = "Novo (ainda fora do Censo)", curso,
              detalhe = paste0(instituicao, ", desde ", inicio, if_else(is.na(vagas), "", paste0(", ", vagas, " vagas por ano"))),
              rede, oferta = 1, matriculas = NA_real_, concluintes = NA_real_, cadeia_fixa = coalesce(cadeia_id, ""), fonte) else tibble()
  out <- bind_rows(sup, tec, novos)
  if (!nrow(out)) return(out)
  out %>% mutate(cadeias = cadeias_do_curso(curso, mapa),
                 cadeias = ifelse(coalesce(cadeia_fixa, "") != "", cadeia_fixa, cadeias)) %>% select(-any_of("cadeia_fixa")) %>%
    left_join(municipios %>% select(id_municipio, municipio), by = "id_municipio") %>%
    left_join(rd %>% mutate(id_municipio = as.character(id_municipio)) %>% select(id_municipio, rd), by = "id_municipio") %>%
    arrange(id_municipio, match(tipo, c("Novo (ainda fora do Censo)", "Superior presencial", "Técnico", "Superior a distância")), desc(matriculas))
}

# Pedaço para o JSON do território (município ou região)
pedaco_formacao <- function(f, ids, nomes_cad, n = 60) {
  if (!NROW(f)) return(NULL)
  x <- f %>% filter(id_municipio %in% ids)
  if (!nrow(x)) return(NULL)
  if (length(ids) > 1) x <- x %>% group_by(tipo, curso, detalhe = if_else(tipo == "Novo (ainda fora do Censo)", paste0(municipio, ": ", detalhe), detalhe), cadeias) %>%
    summarise(ano = max(ano), rede = paste(sort(unique(rede)), collapse = " e "), oferta = sum(oferta), matriculas = sum(matriculas), concluintes = sum(concluintes),
              municipios = paste(sort(unique(municipio)), collapse = ", "), .groups = "drop") %>%
    arrange(match(tipo, c("Novo (ainda fora do Censo)", "Superior presencial", "Técnico", "Superior a distância")), desc(matriculas))
  resumo <- x %>% group_by(tipo) %>% summarise(cursos = n(), matriculas = sum(matriculas, na.rm = TRUE), .groups = "drop")
  lista <- x %>% head(n) %>% mutate(cadeias = lapply(strsplit(cadeias, ";"), function(k) if (length(k)) lapply(k, function(i) list(id = i, nome = unname(nomes_cad[i]))) else list()))
  list(resumo = resumo, lista = lista %>% select(any_of(c("tipo", "curso", "detalhe", "rede", "matriculas", "concluintes", "municipios", "cadeias"))), total = nrow(x))
}
