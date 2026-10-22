# Publica o portfólio no GitHub sem precisar do git instalado.
#
# Usa a API do GitHub com um token (nunca escreva o token neste arquivo).
# Crie o token em https://github.com/settings/tokens/new?scopes=repo,workflow
# e, na sessão do R, rode (abre uma caixa de senha):
#   Sys.setenv(GITHUB_PAT = rstudioapi::askForPassword("Token do GitHub"))
# Com git instalado, gitcreds::gitcreds_set() também serve.
#
# Depois, na raiz do projeto:
#   source("R/publicar_github.R"); publicar_github()
#
# O que faz: copia site/_site para docs/, cria o repositório (se não existir),
# envia todos os arquivos do projeto respeitando o .gitignore num único
# commit e liga o GitHub Pages lendo a pasta docs/ do branch main.

publicar_github <- function(repo = "portfolio", descricao = "Portfólio de análises setoriais e cadeias produtivas de Pernambuco com dados públicos",
                            privado = FALSE, mensagem = paste("Atualiza portfólio", format(Sys.Date(), "%d/%m/%Y"))) {
  for (p in c("httr2", "gitcreds", "jsonlite", "rstudioapi")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  stopifnot(file.exists("_targets.R"), dir.exists("site/_site"))

  # token: variável GITHUB_PAT da sessão (sem git instalado) ou gitcreds
  token <- Sys.getenv("GITHUB_PAT")
  if (!nzchar(token)) token <- tryCatch(gitcreds::gitcreds_get(url = "https://github.com")$password, error = function(e) "")
  if (!nzchar(token)) stop('Sem token. Rode: Sys.setenv(GITHUB_PAT = rstudioapi::askForPassword("Token do GitHub"))')
  api <- function(caminho, metodo = "GET", corpo = NULL, ok404 = FALSE) {
    r <- httr2::request(paste0("https://api.github.com", caminho)) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_headers(Accept = "application/vnd.github+json", `X-GitHub-Api-Version` = "2022-11-28") |>
      httr2::req_method(metodo) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_retry(max_tries = 4)
    if (!is.null(corpo)) r <- httr2::req_body_json(r, corpo, auto_unbox = TRUE)
    resp <- httr2::req_perform(r)
    st <- httr2::resp_status(resp)
    if (ok404 && st == 404) return(NULL)
    if (st >= 300) stop("GitHub ", st, " em ", metodo, " ", caminho, ": ", httr2::resp_body_string(resp))
    if (length(httr2::resp_body_raw(resp)) == 0) return(list())
    httr2::resp_body_json(resp)
  }

  dono <- api("/user")$login
  message("Conta: ", dono)

  # 1. docs/ = site renderizado
  unlink("docs", recursive = TRUE)
  dir.create("docs")
  file.copy(list.files("site/_site", full.names = TRUE, all.files = TRUE, no.. = TRUE), "docs", recursive = TRUE)
  file.create("docs/.nojekyll")
  message("docs/: ", length(list.files("docs", recursive = TRUE)), " arquivos")

  # 2. repositório
  info <- api(paste0("/repos/", dono, "/", repo), ok404 = TRUE)
  if (is.null(info)) {
    message("Criando ", dono, "/", repo, if (privado) " (privado)" else " (público)")
    api("/user/repos", "POST", list(name = repo, description = descricao, private = privado,
                                    homepage = sprintf("https://%s.github.io/%s/", dono, repo), auto_init = TRUE))
    Sys.sleep(3)
    info <- api(paste0("/repos/", dono, "/", repo))
  }
  ramo <- info$default_branch
  base <- paste0("/repos/", dono, "/", repo)
  ref <- api(paste0(base, "/git/ref/heads/", ramo))
  commit_pai <- ref$object$sha

  # 3. arquivos (regras do .gitignore)
  arqs <- arquivos_versionados()
  message("Enviando ", length(arqs), " arquivos (", round(sum(file.size(arqs)) / 1e6, 1), " MB)...")
  arvore <- vector("list", length(arqs))
  for (i in seq_along(arqs)) {
    conteudo <- jsonlite::base64_enc(readBin(arqs[i], "raw", file.size(arqs[i])))
    b <- api(paste0(base, "/git/blobs"), "POST", list(content = gsub("\n", "", conteudo), encoding = "base64"))
    arvore[[i]] <- list(path = arqs[i], mode = "100644", type = "blob", sha = b$sha)
    if (i %% 25 == 0) message("  ", i, "/", length(arqs))
  }
  # árvore completa (sem base_tree): arquivos apagados no projeto somem do repositório
  tr <- api(paste0(base, "/git/trees"), "POST", list(tree = arvore))
  cm <- api(paste0(base, "/git/commits"), "POST", list(message = mensagem, tree = tr$sha, parents = list(commit_pai)))
  api(paste0(base, "/git/refs/heads/", ramo), "PATCH", list(sha = cm$sha))
  message("Commit enviado: ", cm$html_url)

  # 4. GitHub Pages lendo docs/
  pg <- api(paste0(base, "/pages"), ok404 = TRUE)
  if (is.null(pg)) {
    pg <- api(paste0(base, "/pages"), "POST", list(source = list(branch = ramo, path = "/docs")))
  }
  message("Site: ", pg$html_url %||% sprintf("https://%s.github.io/%s/", dono, repo),
          " (o GitHub leva 1 a 3 minutos para publicar)")
  invisible(cm$html_url)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Lista os arquivos do projeto aplicando o .gitignore (padrões simples: pasta/,
# *.ext, caminho exato e exceções com !).
arquivos_versionados <- function(raiz = ".") {
  todos <- list.files(raiz, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  todos <- todos[!grepl("^\\.git/|^\\.Rproj\\.user/|(^|/)desktop\\.ini$|(^|/)\\.DS_Store$", todos)]
  regras <- trimws(readLines(file.path(raiz, ".gitignore"), warn = FALSE))
  regras <- regras[regras != "" & !startsWith(regras, "#")]
  para_regex <- function(p) {
    ancorado <- grepl("/", sub("/$", "", p))
    pasta <- endsWith(p, "/")
    p <- sub("/$", "", sub("^/", "", p))
    p <- gsub("\\.", "\\\\.", p)
    p <- gsub("\\*\\*", "\u0001", p)
    p <- gsub("\\*", "[^/]*", p)
    p <- gsub("\u0001", ".*", p)
    paste0(if (ancorado) "^" else "(^|/)", p, if (pasta) "/" else "($|/)")
  }
  fica <- rep(TRUE, length(todos))
  for (r in regras) {
    neg <- startsWith(r, "!")
    rx <- para_regex(sub("^!", "", r))
    m <- grepl(rx, todos, perl = TRUE)
    fica[m] <- neg
  }
  sort(todos[fica])
}
