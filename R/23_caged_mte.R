# =============================================================================
# Novo CAGED: meses recentes direto do MTE (PDET)
#
# A Base dos Dados libera os meses mais recentes do Novo CAGED só no plano
# pago; no acesso gratuito a tabela fica alguns meses atrás (em 11/2026 parou
# em 03/2026). Esta função completa a série com os microdados publicados pelo
# MTE no FTP do PDET, um arquivo .7z por mês (CAGEDMOV, declarações no prazo,
# o mesmo recorte da tabela da Base dos Dados).
#
# Cada mês baixado vira um .rds em dados/caged_mte/ (fora do Git) e não é
# baixado de novo. Se o FTP não responder, segue só com a Base dos Dados.
# =============================================================================

CAGED_FTP <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED"

# Descompacta um .7z: pacote archive, se houver; senão o 7-Zip do Windows
extrair_7z <- function(arq, destino) {
  if (!requireNamespace("archive", quietly = TRUE))
    try(utils::install.packages("archive", quiet = TRUE), silent = TRUE)
  if (requireNamespace("archive", quietly = TRUE)) {
    archive::archive_extract(arq, dir = destino)
  } else {
    z <- c(Sys.which("7z"), "C:/Program Files/7-Zip/7z.exe", "C:/Program Files (x86)/7-Zip/7z.exe")
    z <- z[nzchar(z) & file.exists(z)][1]
    if (is.na(z)) stop("sem o pacote archive e sem o 7-Zip para abrir o .7z")
    system2(z, c("x", shQuote(arq), paste0("-o", shQuote(destino)), "-y"), stdout = FALSE)
  }
  list.files(destino, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)[1]
}

# Lê um mês do MTE e agrega como extrair_caged_uf (município x subclasse)
caged_mte_mes <- function(aaaamm, uf_codigo, municipios, pasta) {
  arq_rds <- file.path(pasta, paste0(aaaamm, ".rds"))
  if (file.exists(arq_rds)) return(readRDS(arq_rds))
  url <- sprintf("%s/%s/%s/CAGEDMOV%s.7z", CAGED_FTP, substr(aaaamm, 1, 4), aaaamm, aaaamm)
  tmp <- tempfile(fileext = ".7z"); dir_x <- tempfile("caged_"); dir.create(dir_x)
  on.exit(unlink(c(tmp, dir_x), recursive = TRUE), add = TRUE)
  op <- options(timeout = max(900, getOption("timeout"))); on.exit(options(op), add = TRUE)
  ok <- tryCatch({ suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE)) == 0 }, error = function(e) FALSE)
  if (!ok || !file.exists(tmp) || file.size(tmp) < 1e5) return(NULL)   # mês ainda não publicado (ou FTP fora)
  txt <- extrair_7z(tmp, dir_x)
  if (is.na(txt)) stop("o .7z de ", aaaamm, " não trouxe o .txt")

  # cabeçalho: nomes sem acento, para achar as colunas mesmo com encoding trocado
  cab <- strsplit(readLines(txt, n = 1, warn = FALSE, encoding = "UTF-8"), ";", fixed = TRUE)[[1]]
  if (!all(validUTF8(cab))) cab <- iconv(cab, "latin1", "UTF-8")
  norm <- tolower(gsub("[^A-Za-z]", "", iconv(cab, "UTF-8", "ASCII//TRANSLIT", sub = "")))
  acha <- function(p) { i <- which(grepl(p, norm))[1]; if (is.na(i)) stop("coluna não encontrada no CAGED do MTE: ", p); i }
  idx <- c(comp = acha("^compet.*mov$"), uf = acha("^uf$"), mun = acha("^munic"), sub = acha("^subclasse$"),
           saldo = acha("^saldomov"), sal = acha("^sal.?rio$"))
  if (!requireNamespace("data.table", quietly = TRUE)) utils::install.packages("data.table", quiet = TRUE)
  df <- data.table::fread(txt, sep = ";", select = unname(idx), colClasses = "character", showProgress = FALSE, encoding = "UTF-8")
  names(df) <- names(idx)
  df <- tibble::as_tibble(df) %>% filter(suppressWarnings(as.integer(uf)) == as.integer(uf_codigo))

  id7 <- municipios %>% transmute(id_municipio = as.character(id_municipio), mun = substr(id_municipio, 1, 6))
  out <- df %>%
    transmute(ano = as.numeric(substr(comp, 1, 4)), mes = as.numeric(substr(comp, 5, 6)), mun = substr(gsub("\\D", "", mun), 1, 6),
              subclasse = stringr::str_pad(gsub("\\D", "", sub), 7, pad = "0"),
              saldo = suppressWarnings(as.integer(saldo)),
              salario = suppressWarnings(as.numeric(gsub(",", ".", sal)))) %>%
    left_join(id7, by = "mun") %>% filter(!is.na(id_municipio), !is.na(saldo)) %>%
    group_by(ano, mes, id_municipio, subclasse) %>%
    summarise(salario_admissao = if (any(saldo > 0)) mean(salario[saldo > 0], na.rm = TRUE) else NA_real_,
              admissoes = sum(saldo > 0), desligamentos = sum(saldo < 0), saldo = sum(saldo), .groups = "drop") %>%
    mutate(salario_admissao = ifelse(is.nan(salario_admissao), NA_real_, salario_admissao)) %>%
    select(ano, mes, id_municipio, subclasse, saldo, admissoes, desligamentos, salario_admissao) %>% com_classe()
  if (!nrow(out)) stop("o CAGED do MTE de ", aaaamm, " veio sem linhas para a UF ", uf_codigo)
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, arq_rds)
  msg("Novo CAGED ", aaaamm, " lido do MTE: saldo ", sum(out$saldo), " em ", n_distinct(out$id_municipio), " municípios.")
  out
}

# Junta a Base dos Dados com os meses que só o MTE já publicou
completar_caged_mte <- function(bd, municipios, uf_codigo, pasta = caminho("dados", "caged_mte")) {
  if (!NROW(bd)) return(bd)
  ult <- max(as.Date(sprintf("%d-%02d-01", as.integer(bd$ano), as.integer(bd$mes))))
  lim <- as.Date(format(Sys.Date(), "%Y-%m-01"))   # o mês corrente nunca está publicado
  alvo <- seq(ult, by = "month", length.out = 2)[2]
  novos <- list()
  while (alvo < lim) {
    x <- tryCatch(caged_mte_mes(format(alvo, "%Y%m"), uf_codigo, municipios, pasta),
                  error = function(e) { msg("Novo CAGED do MTE (", format(alvo, "%m/%Y"), "): ", conditionMessage(e)); NULL })
    if (is.null(x)) break
    novos[[length(novos) + 1]] <- x
    alvo <- seq(alvo, by = "month", length.out = 2)[2]
  }
  if (!length(novos)) {
    msg("Novo CAGED: só a Base dos Dados, até ", format(ult, "%m/%Y"), ".")
    return(bd)
  }
  novos <- bind_rows(novos)
  msg("Novo CAGED: Base dos Dados até ", format(ult, "%m/%Y"), " e MTE até ",
      sprintf("%02d/%d", max(novos$mes[novos$ano == max(novos$ano)]), max(novos$ano)), ".")
  bind_rows(bd, novos %>% anti_join(bd %>% distinct(ano, mes), by = c("ano", "mes")))
}
