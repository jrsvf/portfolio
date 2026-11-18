# Roda o pipeline e renderiza o site no computador local, fora do OneDrive.
#
# Uso (de qualquer sessão do R, sem travar o console):
#   Sys.setenv(GARGLE_EMAIL = "seu-email@gmail.com")   # conta do BigQuery já autorizada
#   rodar_local()            # roda em segundo plano; acompanhe em log_local.log
#
# Por que fora do OneDrive: durante a renderização o OneDrive sincroniza e
# trava arquivos (erro "arquivo já está sendo usado por outro processo").
# O site é gerado numa cópia em %LOCALAPPDATA%/portfolio_render e o resultado
# volta para site/_site.

rodar_local <- function(projeto = getwd(), espera = FALSE) {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  script <- file.path(projeto, "R", "rodar_local.R")
  log <- file.path(projeto, "log_local.log")
  Sys.setenv(PORTFOLIO_PROJETO = normalizePath(projeto, winslash = "/"))
  system2(rscript, c("-e", shQuote("source(Sys.getenv('PORTFOLIO_PROJETO') |> file.path('R', 'rodar_local.R')); executar_local()")),
          stdout = log, stderr = log, wait = espera)
  message("Rodando em segundo plano. Acompanhe: ", log)
  invisible(log)
}

executar_local <- function() {
  proj <- Sys.getenv("PORTFOLIO_PROJETO")
  setwd(proj)
  passo <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
  # conta do BigQuery: a do GARGLE_EMAIL ou, sem ele, o único token já autorizado
  options(gargle_oauth_email = if (nzchar(Sys.getenv("GARGLE_EMAIL"))) Sys.getenv("GARGLE_EMAIL") else TRUE)

  passo("1/3 pipeline (targets)")
  alvos <- strsplit(Sys.getenv("PORTFOLIO_ALVOS"), ",")[[1]]
  if (length(alvos)) {
    # só os alvos pedidos, sem reconferir os de cima (útil quando uma API externa está fora do ar)
    targets::tar_make(names = tidyselect::any_of(alvos), shortcut = TRUE, reporter = "timestamp")
  } else targets::tar_make(reporter = "timestamp")

  passo("2/3 renderização fora do OneDrive")
  base <- file.path(Sys.getenv("LOCALAPPDATA", tempdir()), "portfolio_render")
  unlink(base, recursive = TRUE); dir.create(file.path(base, "site"), recursive = TRUE)
  file.copy(c("R", "config", "dados", "_targets.R", "DESCRIPTION"), base, recursive = TRUE)
  fora <- c("_site", "site_libs", ".quarto", "index_files")
  s <- setdiff(list.files("site", all.files = TRUE, no.. = TRUE), fora)
  file.copy(file.path("site", s), file.path(base, "site"), recursive = TRUE)
  lixo <- list.files(file.path(base, "site"), pattern = "(\\.rmarkdown|\\.knit\\.md|_files)$", recursive = TRUE,
                     full.names = TRUE, include.dirs = TRUE)
  unlink(lixo, recursive = TRUE)
  st <- withr::with_dir(base, system2(quarto::quarto_path(), c("render", "site")))
  if (!identical(as.integer(st), 0L)) stop("quarto render falhou (código ", st, ")")

  passo("3/3 copiando o site para site/_site")
  unlink("site/_site", recursive = TRUE); dir.create("site/_site")
  file.copy(list.files(file.path(base, "site", "_site"), full.names = TRUE, all.files = TRUE, no.. = TRUE),
            "site/_site", recursive = TRUE)
  conferir_site("site/_site", "site")
  passo("pronto:", length(list.files("site/_site", recursive = TRUE)), "arquivos. Publicar: source('R/publicar_github.R'); publicar_github()")
}


# Conferência do site gerado: procura erro de R impresso nas páginas, avisos de
# "ainda não calculado" e arquivos de dados que as páginas OJS pedem e não existem.
# Não para nada: escreve o resultado no log e em <site>/conferencia.txt.
conferir_site <- function(dir_site, dir_fonte = "site") {
  if (!dir.exists(dir_site)) return(invisible(NULL))
  htmls <- list.files(dir_site, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  achados <- character()
  padroes <- c(erro_r = "cell-output-error|<pre[^>]*>\\s*(##\\s*)?(Error|Erro)( in| em|:)",
               nao_calculado = "ainda n[ãa]o (foram|foi) calculad|Rode o pipeline",
               sem_dados = "Sem dados suficientes")
  for (f in htmls) {
    tx <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    for (k in names(padroes)) if (grepl(padroes[[k]], tx, perl = TRUE))
      achados <- c(achados, paste0(k, ": ", sub(paste0("^", dir_site, "/?"), "", f)))
  }
  # FileAttachment("dados/x.json") nas páginas OJS: o arquivo tem de existir no site
  qmds <- list.files(dir_fonte, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
  for (q in qmds) {
    tx <- paste(readLines(q, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    anexos <- unique(regmatches(tx, gregexpr('FileAttachment\\("([^"]+)"\\)', tx))[[1]])
    for (a in sub('FileAttachment\\("([^"]+)"\\)', "\\1", anexos)) {
      alvo <- file.path(dir_site, dirname(sub(paste0("^", dir_fonte, "/?"), "", q)), a)
      if (!file.exists(alvo)) achados <- c(achados, paste0("dado_faltando: ", basename(q), " pede ", a))
    }
  }
  achados <- unique(achados)
  # cobertura das listas curadas: cadeia sem norma, sem incentivo próprio ou sem pesquisa cadastrada vira aviso
  # (não é erro do site, é lacuna de pesquisa a fechar)
  avisos <- tryCatch({
    raiz <- Sys.getenv("PASTA_PROJETO", normalizePath(file.path(dir_fonte, "..")))
    meta <- readRDS(file.path(raiz, "dados", "cad.rds"))$meta
    rg <- utils::read.csv(file.path(raiz, "config", "regulacao_cadeias.csv"), encoding = "UTF-8")
    rf <- utils::read.csv(file.path(raiz, "config", "referencias_cadeias.csv"), encoding = "UTF-8")
    proprio <- unique(rg$cadeia_id[rg$tipo == "Incentivo fiscal" & !grepl("SUDENE|PRODEPE|PROIND", rg$instrumento)])
    c(sprintf("sem_norma: %s", setdiff(meta$cadeia_id, rg$cadeia_id)),
      sprintf("sem_incentivo_proprio: %s", setdiff(meta$cadeia_id, proprio)),
      sprintf("sem_pesquisa: %s", setdiff(meta$cadeia_id, rf$cadeia_id)))
  }, error = function(e) character())
  linhas <- c(paste("Conferência do site em", format(Sys.time(), "%d/%m/%Y %H:%M")), paste(length(htmls), "páginas"), "",
              if (length(achados)) achados else "Nenhum problema encontrado.",
              if (length(avisos)) c("", paste0("Lacunas das listas curadas (", length(avisos), "), para pesquisar:"), avisos))
  writeLines(linhas, file.path(dir_site, "conferencia.txt"), useBytes = TRUE)
  cat(format(Sys.time(), "%H:%M:%S"), "| conferência do site:", if (length(achados)) paste(length(achados), "problema(s):") else "nenhum problema", "\n")
  if (length(achados)) cat(paste0("   ", achados, "\n"), sep = "")
  invisible(achados)
}
