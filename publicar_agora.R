# Atualiza e publica o portfólio em segundo plano.
# Abra este arquivo no RStudio e clique em "Source" (ou Ctrl+Shift+S).
P <- "C:/Users/jose.vercosa/OneDrive - adepe.pe.gov.br/\u00c1rea de Trabalho/07_Pessoal/Escrita e Portf\u00f3lio/portfolio-jrsvf"
setwd(P)
if (!nzchar(Sys.getenv("GITHUB_PAT"))) Sys.setenv(GITHUB_PAT = rstudioapi::askForPassword("Token do GitHub"))
# rodada completa (sem PORTFOLIO_ALVOS): entram a matriz NEREUS e os QLs de empresas, exportações e produção
Sys.setenv(PORTFOLIO_PROJETO = normalizePath(P, winslash = "/"), PORTFOLIO_ALVOS = "")
log <- file.path(P, "log_publicar.log")
system2(file.path(R.home("bin"), "Rscript.exe"),
        c("-e", shQuote(paste0(
          "source(file.path(Sys.getenv('PORTFOLIO_PROJETO'), 'R', 'rodar_local.R')); executar_local(); ",
          "source(file.path(Sys.getenv('PORTFOLIO_PROJETO'), 'R', 'publicar_github.R'), encoding = 'UTF-8'); ",
          "publicar_github(mensagem = 'QL por emprego, empresas, exportação e produção; matriz NEREUS 2019')"))),
        stdout = log, stderr = log, wait = FALSE)
message("Rodando em segundo plano (uns 40 minutos). Para acompanhar: file.info(log)$size")
