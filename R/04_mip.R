# =============================================================================
# Encadeamentos produtivos: matriz insumo-produto regionalizada para PE
# =============================================================================
# Pernambuco não tem matriz insumo-produto oficial. A saída usual na
# literatura é regionalizar a matriz nacional do IBGE com quocientes
# locacionais. Aqui usamos o FLQ de Flegg (Flegg e Webber, 1997), que corrige
# a tendência do QL simples de superestimar as compras dentro da região:
#
#   SLQ_i   = (E_r,i / E_r) / (E_n,i / E_n)
#   CILQ_ij = SLQ_i / SLQ_j
#   lambda  = [log2(1 + E_r / E_n)]^delta
#   FLQ_ij  = CILQ_ij * lambda    (FLQ_ii = SLQ_i * lambda)
#   a_r,ij  = a_n,ij * min(FLQ_ij, 1)
#
# Depois calculamos a inversa de Leontief B = (I - A_r)^-1 e os índices de
# Rasmussen-Hirschman:
#   Ligação para trás  BL_j = (soma_i b_ij / n) / (soma_ij b_ij / n^2)
#   Ligação para frente FL_i = (soma_j b_ij / n) / (soma_ij b_ij / n^2)
#   Setor-chave: BL > 1 e FL > 1
#
# RESSALVA: é uma ESTIMATIVA. A tecnologia é a nacional; só o grau de
# compras dentro do estado é ajustado pelo emprego. Os números servem para
# ordenar e comparar setores, não como medida exata de fluxos de PE.
#
# Correspondência MIP x CNAE: tabela oficial do IBGE "Atividades-contas de
# divulgação x CNAE 2.0" (config/mip_atividades.csv). Cada classe CNAE da
# RAIS é ligada à atividade da MIP pelo prefixo mais longo (grupo de 3 ou
# classe de 4 dígitos). Educação e saúde públicas/privadas são somadas porque
# a RAIS não as separa sem natureza jurídica.
# =============================================================================

ler_mip_atividades <- function(arquivo = caminho("config", "mip_atividades.csv")) {
  exigir_arquivo(arquivo, "config/mip_atividades.csv faz parte do repositório")
  readr::read_csv(arquivo, col_types = "ccccc")
}

# Setor da MIP de cada classe CNAE (prefixo mais longo)
setor_da_classe <- function(classes, mip_ativ) {
  pref <- mip_ativ %>% distinct(setor, prefixos) %>%
    tidyr::separate_rows(prefixos, sep = ";") %>% distinct(setor, prefixo = prefixos)
  pref <- pref[order(-nchar(pref$prefixo)), ]
  vapply(classes, function(cl) {
    hit <- pref$setor[startsWith(cl, pref$prefixo)]
    if (length(hit) == 0) NA_character_ else hit[1]
  }, character(1), USE.NAMES = FALSE)
}

# Baixa (se preciso) e localiza o arquivo .xls/.xlsx da MIP
obter_arquivo_mip <- function(url, pasta = caminho("dados_externos", "mip")) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  existentes <- list.files(pasta, pattern = "\\.xlsx?$", full.names = TRUE, recursive = TRUE)
  if (length(existentes) > 0) return(existentes)
  destino <- file.path(pasta, basename(url))
  msg("Baixando MIP do IBGE: ", url)
  ok <- tryCatch({ utils::download.file(url, destino, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) { msg("Falha no download da MIP: ", conditionMessage(e)); FALSE })
  if (!ok) return(character())
  if (grepl("\\.zip$", destino)) utils::unzip(destino, exdir = pasta)
  list.files(pasta, pattern = "\\.xlsx?$", full.names = TRUE, recursive = TRUE)
}

# Procura, em todas as planilhas, a matriz de coeficientes técnicos
# intersetoriais (atividade x atividade). Reconhece o bloco pela presença de
# uma linha de cabeçalho com dezenas de códigos de 4 dígitos.
ler_coeficientes_mip <- function(arquivos) {
  candidatos <- list()
  for (arq in arquivos) {
    for (aba in readxl::excel_sheets(arq)) {
      bruto <- suppressMessages(readxl::read_excel(arq, sheet = aba, col_names = FALSE, .name_repair = "minimal"))
      if (nrow(bruto) < 20) next
      texto <- tolower(paste(unlist(bruto[1:min(8, nrow(bruto)), ]), collapse = " "))
      if (!grepl("coeficientes t[eé]cnicos", texto)) next
      if (grepl("produto", texto) && !grepl("intersetoriais|atividade", texto)) next
      m <- extrair_bloco_quadrado(bruto)
      if (!is.null(m)) candidatos[[paste(basename(arq), aba)]] <- m
    }
  }
  if (length(candidatos) == 0) return(NULL)
  # prefere a matriz atividade x atividade (maior bloco quadrado)
  candidatos[[which.max(vapply(candidatos, nrow, 1L))]]
}

extrair_bloco_quadrado <- function(bruto) {
  # códigos de 4 dígitos, sozinhos ou seguidos da descrição ("0191\nAgricultura...")
  eh_cod <- function(x) grepl("^\\d{4}(\\s|$)", trimws(as.character(x)))
  cod4 <- function(x) substr(trimws(as.character(x)), 1, 4)
  linha_cab <- which(apply(bruto, 1, function(r) sum(eh_cod(r), na.rm = TRUE)) >= 20)[1]
  if (is.na(linha_cab)) return(NULL)
  cab <- trimws(as.character(unlist(bruto[linha_cab, ])))
  cols <- which(eh_cod(cab))
  cab <- cod4(cab)
  # coluna com os códigos das linhas = primeira coluna com muitos códigos abaixo do cabeçalho
  abaixo <- bruto[(linha_cab + 1):nrow(bruto), , drop = FALSE]
  col_cod <- which(apply(abaixo, 2, function(c) sum(eh_cod(c), na.rm = TRUE)) >= 20)[1]
  if (is.na(col_cod)) return(NULL)
  linhas <- which(eh_cod(abaixo[[col_cod]]))
  cod_l <- cod4(abaixo[[col_cod]][linhas])
  cod_c <- cab[cols]
  comuns <- intersect(cod_l, cod_c)
  if (length(comuns) < 20) return(NULL)
  m <- suppressWarnings(apply(as.matrix(abaixo[linhas, cols]), 2, as.numeric))
  dimnames(m) <- list(cod_l, cod_c)
  m <- m[comuns, comuns]
  m[is.na(m)] <- 0
  if (any(colSums(m) >= 1)) return(NULL)  # coeficientes técnicos somam < 1 por coluna
  m
}

# Agrega a matriz A das atividades da MIP nos setores (soma nas linhas,
# média simples nas colunas de atividades que viram um setor só)
agregar_mip_setores <- function(A, mip_ativ) {
  setor <- mip_ativ$setor[match(rownames(A), mip_ativ$atividade)]
  if (any(is.na(setor))) stop("Atividades da MIP fora de config/mip_atividades.csv: ",
                              paste(rownames(A)[is.na(setor)], collapse = ", "))
  niveis <- unique(mip_ativ$setor)
  niveis <- niveis[niveis %in% setor]
  S <- outer(niveis, setor, `==`) * 1
  W <- t(S) / rowSums(S)[col(t(S))]
  Ag <- S %*% A %*% W
  dimnames(Ag) <- list(niveis, niveis)
  Ag
}

# Regionalização FLQ
regionalizar_flq <- function(A_n, emp_r, emp_n, delta = 0.3) {
  setores <- rownames(A_n)
  er <- emp_r[setores]; er[is.na(er)] <- 0
  en <- emp_n[setores]; en[is.na(en)] <- 0
  slq <- div0(er / sum(er), en / sum(en))
  lambda <- log2(1 + sum(er) / sum(en))^delta
  cilq <- outer(slq, slq, function(si, sj) div0(si, sj))
  diag(cilq) <- slq
  flq <- pmin(cilq * lambda, 1)
  A_r <- A_n * flq
  # setor inexistente no estado (sem emprego) não compra nem vende localmente
  A_r[er == 0, ] <- 0
  A_r[, er == 0] <- 0
  attr(A_r, "slq") <- slq
  attr(A_r, "lambda") <- lambda
  A_r
}

indices_ligacao <- function(A, nomes = NULL) {
  n <- nrow(A)
  B <- solve(diag(n) - A)
  media <- sum(B) / n^2
  out <- tibble(
    grupo   = rownames(A),
    bl      = (colSums(B) / n) / media,
    fl      = (rowSums(B) / n) / media,
    mult_producao = colSums(B)
  ) %>%
    mutate(classificacao = case_when(
      bl > 1 & fl > 1 ~ "Setor-chave",
      bl > 1          ~ "Forte ligação para trás",
      fl > 1          ~ "Forte ligação para frente",
      TRUE            ~ "Baixo encadeamento"))
  if (!is.null(nomes)) out <- out %>% left_join(nomes, by = "grupo")
  out
}

# Fluxo completo. Devolve NULL (e a página mostra um aviso) se a MIP não for
# encontrada, para não derrubar o resto do pipeline.
calcular_encadeamentos <- function(rais_br, par, uf) {
  mip_ativ <- ler_mip_atividades()
  arquivos <- obter_arquivo_mip(par$mip$url)
  if (length(arquivos) == 0) { msg("MIP indisponível: módulo de encadeamento pulado."); return(NULL) }
  A <- ler_coeficientes_mip(arquivos)
  if (is.null(A)) { msg("Não encontrei a matriz de coeficientes técnicos na MIP: módulo pulado."); return(NULL) }
  A <- A[rownames(A) %in% mip_ativ$atividade, colnames(A) %in% mip_ativ$atividade]
  As <- agregar_mip_setores(A, mip_ativ)

  emp <- rais_br %>% filter(ano == par$mip$ano_emprego) %>%
    group_by(sigla_uf, classe) %>% summarise(vinculos = sum(vinculos), .groups = "drop")
  map <- tibble(classe = unique(emp$classe)) %>% mutate(setor = setor_da_classe(classe, mip_ativ))
  emp <- emp %>% inner_join(map, by = "classe") %>% filter(!is.na(setor))
  emp_n <- emp %>% group_by(setor) %>% summarise(e = sum(vinculos)) %>% { setNames(.$e, .$setor) }
  emp_r <- emp %>% filter(sigla_uf == uf) %>% group_by(setor) %>% summarise(e = sum(vinculos)) %>% { setNames(.$e, .$setor) }

  A_r <- regionalizar_flq(As, emp_r, emp_n, par$mip$flq_delta)
  nomes <- mip_ativ %>% distinct(grupo = setor, grupo_nome = setor_nome)
  list(
    nacional = indices_ligacao(As, nomes) %>% mutate(recorte = "Brasil"),
    regional = indices_ligacao(A_r, nomes) %>%
      mutate(recorte = "Pernambuco (FLQ)", slq = attr(A_r, "slq")[grupo],
             # Setor muito pequeno no estado tem coeficientes pouco reduzidos pelo FLQ
             # e parece "muito encadeado" justamente por ser pequeno. Fica marcado e
             # fora dos rankings e destaques.
             presenca = ifelse(slq < (par$mip$slq_minimo %||% 0.3), "Pequena em PE", "Relevante"),
             classificacao = ifelse(presenca == "Pequena em PE", "Presença pequena em PE", classificacao)),
    slq_minimo = par$mip$slq_minimo %||% 0.3,
    mapa_classes = map,
    lambda   = attr(A_r, "lambda"),
    delta    = par$mip$flq_delta,
    ano_mip  = par$mip$ano
  )
}
