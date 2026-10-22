# =============================================================================
# Utilitários gerais: configuração, mensagens, leitura das cadeias
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(yaml)
})

# Pasta raiz do projeto. Funciona no RStudio, no terminal e no GitHub Actions.
PASTA_PROJETO <- Sys.getenv("PASTA_PROJETO", unset = getwd())

caminho <- function(...) file.path(PASTA_PROJETO, ...)

msg <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", paste0(...))
}

# Erro claro quando um arquivo de uma etapa anterior não existe.
exigir_arquivo <- function(arquivo, etapa_anterior) {
  if (!file.exists(arquivo)) {
    stop(
      "Arquivo não encontrado: ", arquivo, "\n",
      "Rode antes a etapa: ", etapa_anterior, call. = FALSE
    )
  }
  invisible(arquivo)
}

ler_parametros <- function(arquivo = caminho("config", "parametros.yml")) {
  exigir_arquivo(arquivo, "config/parametros.yml faz parte do repositório")
  p <- yaml::read_yaml(arquivo)
  # Variáveis de ambiente têm prioridade (usadas no GitHub Actions)
  if (nzchar(Sys.getenv("GCP_PROJECT"))) p$bigquery$projeto <- Sys.getenv("GCP_PROJECT")
  p
}

# Classe CNAE 2.0 sempre como texto de 5 dígitos (nunca numérico: perde zeros).
normalizar_cnae <- function(x, digitos = 5) {
  x <- gsub("[^0-9]", "", as.character(x))
  x <- ifelse(nchar(x) == 0, NA_character_, x)
  x <- str_pad(x, width = pmax(nchar(x), digitos), side = "left", pad = "0")
  substr(x, 1, digitos)
}

# Lê config/cadeias.yml e devolve:
#   $meta  uma linha por cadeia
#   $elos  uma linha por (cadeia, elo, código CNAE). `codigo` tem 5 dígitos
#          (classe) ou 7 (subclasse); `nivel` diz qual; `classe` é sempre a
#          classe de 5 dígitos correspondente.
#   $sh4   uma linha por (cadeia, sh4), com a origem (manual ou automática)
#   $sidra uma linha por (cadeia, tabela, padrao)
ler_cadeias <- function(arquivo = caminho("config", "cadeias.yml"),
                        arq_sh4 = caminho("config", "sh4_cnae.csv")) {
  exigir_arquivo(arquivo, "config/cadeias.yml faz parte do repositório")
  cfg <- yaml::read_yaml(arquivo)$cadeias

  ids <- map_chr(cfg, "id")
  if (anyDuplicated(ids)) stop("IDs de cadeia repetidos em cadeias.yml: ",
                               paste(ids[duplicated(ids)], collapse = ", "))

  meta <- map_dfr(cfg, function(c) tibble(
    cadeia_id   = c$id,
    cadeia      = c$nome,
    macrossetor = c$macrossetor %||% "Outros",
    descricao   = str_squish(c$descricao %||% ""),
    nota        = str_squish(c$nota %||% "")
  ))

  elos <- map_dfr(cfg, function(c) {
    map_dfr(seq_along(c$elos), function(i) {
      e <- c$elos[[i]]
      cod <- gsub("[^0-9]", "", as.character(unlist(e$cnae)))
      tibble(cadeia_id = c$id, elo_ordem = i, elo = e$nome, codigo = cod)
    })
  }) %>%
    mutate(nivel = case_when(nchar(codigo) == 5 ~ "classe", nchar(codigo) == 7 ~ "subclasse", TRUE ~ NA_character_),
           classe = substr(codigo, 1, 5))

  invalidas <- elos %>% filter(is.na(nivel))
  if (nrow(invalidas) > 0) {
    stop("Códigos CNAE inválidos em cadeias.yml (use 5 ou 7 dígitos): ",
         paste(invalidas$cadeia_id, invalidas$codigo, collapse = "; "))
  }
  dup <- elos %>% count(cadeia_id, codigo) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop("O mesmo código aparece em dois elos da mesma cadeia: ",
         paste(dup$cadeia_id, dup$codigo, collapse = "; "))
  }
  # classe e uma subclasse dela na mesma cadeia contariam em dobro
  sobrepostos <- elos %>% filter(nivel == "subclasse") %>%
    semi_join(elos %>% filter(nivel == "classe"), by = c("cadeia_id", "classe"))
  if (nrow(sobrepostos) > 0) {
    stop("Subclasse e a própria classe na mesma cadeia (dupla contagem): ",
         paste(sobrepostos$cadeia_id, sobrepostos$codigo, collapse = "; "))
  }

  sh4_manual <- map_dfr(cfg, function(c) {
    v <- unlist(c$sh4)
    if (length(v) == 0) return(tibble(cadeia_id = character(), sh4 = character()))
    tibble(cadeia_id = c$id, sh4 = str_pad(as.character(v), 4, pad = "0"))
  }) %>% mutate(origem = "manual")

  # posições automáticas: classes (5 dígitos) da cadeia na correspondência NCM x CNAE
  sh4_auto <- tibble(cadeia_id = character(), sh4 = character(), origem = character())
  if (file.exists(arq_sh4)) {
    corr <- readr::read_csv(arq_sh4, col_types = "cc")
    auto_on <- map_chr(cfg, "id")[map_lgl(cfg, ~ !isFALSE(.x$sh4_auto))]
    sh4_auto <- elos %>% filter(nivel == "classe", cadeia_id %in% auto_on) %>%
      distinct(cadeia_id, classe) %>%
      inner_join(corr, by = "classe", relationship = "many-to-many") %>%
      distinct(cadeia_id, sh4) %>% mutate(origem = "automática")
  }
  sh4 <- bind_rows(sh4_manual, sh4_auto) %>% distinct(cadeia_id, sh4, .keep_all = TRUE)

  sidra <- map_dfr(cfg, function(c) {
    if (is.null(c$sidra)) return(tibble(cadeia_id = character(), tabela = integer(), padrao = character()))
    map_dfr(c$sidra, ~ tibble(cadeia_id = c$id, tabela = as.integer(.x$tabela), padrao = .x$padrao))
  })

  list(meta = meta, elos = elos, sh4 = sh4, sidra = sidra)
}

# Liga uma base com colunas `classe` e `subclasse` aos elos das cadeias,
# respeitando o nível de cada código (classe ou subclasse).
ligar_elos <- function(df, elos, colunas_elo = c("cadeia_id", "elo_ordem", "elo")) {
  por_classe <- df %>%
    inner_join(elos %>% filter(nivel == "classe") %>% select(all_of(colunas_elo), classe),
               by = "classe", relationship = "many-to-many")
  if (!"subclasse" %in% names(df)) return(por_classe)
  por_sub <- df %>%
    inner_join(elos %>% filter(nivel == "subclasse") %>% select(all_of(colunas_elo), subclasse = codigo),
               by = "subclasse", relationship = "many-to-many")
  bind_rows(por_classe, por_sub)
}

# Confere os códigos das cadeias contra o diretório CNAE oficial
conferir_codigos <- function(elos, diretorio) {
  falta_cl  <- elos %>% filter(nivel == "classe", !classe %in% diretorio$classe)
  falta_sub <- elos %>% filter(nivel == "subclasse", !codigo %in% diretorio$subclasse)
  ruins <- bind_rows(falta_cl, falta_sub)
  if (nrow(ruins) > 0) {
    stop("Códigos CNAE em cadeias.yml que não existem no diretório oficial:\n  ",
         paste(ruins$cadeia_id, ruins$elo, ruins$codigo, sep = " | ", collapse = "\n  "), call. = FALSE)
  }
  msg("Códigos CNAE das cadeias conferidos: ", nrow(elos), " códigos válidos.")
  TRUE
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Salva/lê bases intermediárias em .rds (sem dependências extras)
salvar_base <- function(obj, nome) {
  dir.create(caminho("dados"), showWarnings = FALSE, recursive = TRUE)
  arq <- caminho("dados", paste0(nome, ".rds"))
  saveRDS(obj, arq, compress = "xz")
  arq
}

ler_base <- function(nome, etapa = "targets::tar_make()") {
  arq <- caminho("dados", paste0(nome, ".rds"))
  exigir_arquivo(arq, etapa)
  readRDS(arq)
}

fmt_num <- function(x, dig = 0) {
  formatC(x, format = "f", digits = dig, big.mark = ".", decimal.mark = ",")
}

# 01555 -> 0155-5 ; 0155501 -> 0155-5/01
fmt_cnae <- function(x) {
  ifelse(nchar(x) == 7, paste0(substr(x, 1, 4), "-", substr(x, 5, 5), "/", substr(x, 6, 7)),
         paste0(substr(x, 1, 4), "-", substr(x, 5, 5)))
}

# Cortes de tempo usados em todo o site: 10, 5 e 2 anos antes do último ano
# disponível da fonte e um ano fixo de referência ("desde 2023").
CORTES_PADRAO <- list(janelas = c(10, 5, 2), desde = 2023)

cortes_anos <- function(ult, janelas = CORTES_PADRAO$janelas, desde = CORTES_PADRAO$desde) {
  t <- tibble(rotulo = paste(janelas, "anos"), ano_ini = ult - janelas)
  if (!is.null(desde) && desde < ult) {
    # quando "desde" coincide com uma janela (ex.: 2 anos antes de 2025 = 2023),
    # não repete a coluna: a janela ganha o rótulo "(desde 2023)"
    if (desde %in% t$ano_ini) t$rotulo[t$ano_ini == desde] <- paste0(t$rotulo[t$ano_ini == desde], " (desde ", desde, ")")
    else t <- bind_rows(t, tibble(rotulo = paste("Desde", desde), ano_ini = desde))
  }
  t %>% mutate(ordem = row_number())
}

fmt_pct <- function(x, dig = 1) paste0(fmt_num(100 * x, dig), "%")

fmt_usd <- function(x) {
  ifelse(abs(x) >= 1e9, paste0("US$ ", fmt_num(x / 1e9, 2), " bi"),
  ifelse(abs(x) >= 1e6, paste0("US$ ", fmt_num(x / 1e6, 1), " mi"),
                        paste0("US$ ", fmt_num(x / 1e3, 0), " mil")))
}
