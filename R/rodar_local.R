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
  targets::tar_make(reporter = "summary")

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
  passo("pronto:", length(list.files("site/_site", recursive = TRUE)), "arquivos. Publicar: source('R/publicar_github.R'); publicar_github()")
}
