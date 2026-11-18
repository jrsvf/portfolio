# =============================================================================
# Arranjo produtivo: concentração, vizinhança e completude dos elos
#
# Para cada cadeia e município, no último ano da RAIS, olhando só o núcleo
# produtivo (sem os elos de comércio e distribuição):
#
#   ICN  Índice de Concentração Normalizado (Crocco et al., 2006): combina o
#        quociente locacional (QL), o IHH modificado e a participação relativa
#        (PR) com pesos tirados de uma análise de componentes principais feita
#        cadeia a cadeia. Mostrado de 0 a 100 dentro de cada cadeia.
#   LISA Moran local do QL com os municípios vizinhos (que se tocam, com tolerância de 100 m),
#        significância por permutação condicional (999 sorteios, p < 0,05).
#        "Alto-Alto" = município especializado cercado de especializados.
#   Completude  quantos elos do núcleo da cadeia têm emprego no município e
#        na mancha de que ele faz parte.
#
# Classes:
#   Polo do arranjo         ICN entre os 10% maiores da cadeia, emprego mínimo e
#                           (vizinhos especializados ou metade dos elos presentes)
#   Parte de mancha         Alto-Alto no LISA, com ao menos 10 empregos, sem ser polo
#   Aglomeração isolada     QL >= 1,5 e emprego mínimo, sem vizinhos especializados
# Manchas: grupos contíguos de polos e municípios Alto-Alto da mesma cadeia.
# =============================================================================

vizinhos_rainha <- function(malha, tolerancia = 100) {
  # a malha é simplificada e tem frestas entre os municípios: vizinho é quem fica a até 100 m (em UTM 25S)
  m <- sf::st_transform(sf::st_make_valid(malha), 31985)
  nb <- sf::st_is_within_distance(m, m, dist = tolerancia)
  ids <- as.character(m$id_municipio)
  setNames(lapply(seq_along(nb), function(k) ids[setdiff(nb[[k]], k)]), ids)
}

# Moran local com permutação condicional
lisa_local <- function(x, ids, nb, perm = 999, alfa = 0.05, semente = 26) {
  n <- length(x)
  if (n < 5 || stats::sd(x) == 0) return(tibble(id_municipio = ids, lisa_i = NA_real_, lisa_p = NA_real_, lisa = NA_character_))
  z <- (x - mean(x)) / stats::sd(x)
  pos <- setNames(seq_along(ids), ids)
  viz <- lapply(ids, function(i) unname(pos[intersect(nb[[i]] %||% character(), ids)]))
  lag <- vapply(viz, function(v) if (length(v)) mean(z[v]) else NA_real_, 0)
  ii <- z * lag
  set.seed(semente)
  p <- vapply(seq_len(n), function(i) {
    k <- length(viz[[i]]); if (!k || is.na(ii[i])) return(NA_real_)
    outros <- z[-i]
    sim <- z[i] * vapply(seq_len(perm), function(s) mean(sample(outros, k)), 0)
    (sum(abs(sim) >= abs(ii[i])) + 1) / (perm + 1)
  }, 0)
  cl <- dplyr::case_when(is.na(p) | p >= alfa ~ "Não significativo",
                         z > 0 & lag > 0 ~ "Alto-Alto", z < 0 & lag < 0 ~ "Baixo-Baixo",
                         z > 0 & lag < 0 ~ "Alto-Baixo", TRUE ~ "Baixo-Alto")
  tibble(id_municipio = ids, lisa_i = ii, lisa_p = p, lisa = cl)
}

# Pesos do ICN pela análise de componentes principais (Crocco et al., 2006)
pesos_icn <- function(m) {
  pc <- tryCatch(stats::prcomp(m, center = TRUE, scale. = TRUE), error = function(e) NULL)
  if (is.null(pc)) return(rep(1 / ncol(m), ncol(m)))
  share <- pc$sdev^2 / sum(pc$sdev^2)
  w <- as.numeric(abs(pc$rotation) %*% share)
  w / sum(w)
}

montar_arranjo <- function(cad_mun, total_mun, elos, meta, malha, municipios, rd, emprego_minimo = 50, perm = 999) {
  if (!NROW(cad_mun)) return(NULL)
  ano <- max(cad_mun$ano)
  if (!"nucleo" %in% names(elos)) elos$nucleo <- TRUE
  nuc <- elos %>% distinct(cadeia_id, elo, nucleo) %>% group_by(cadeia_id, elo) %>% summarise(nucleo = any(nucleo), .groups = "drop")
  cm <- cad_mun %>% filter(ano == !!ano) %>% mutate(id_municipio = as.character(id_municipio)) %>%
    inner_join(nuc %>% filter(nucleo), by = c("cadeia_id", "elo"))
  n_elos <- nuc %>% filter(nucleo) %>% count(cadeia_id, name = "elos_nucleo")
  base <- cm %>% group_by(id_municipio, cadeia_id) %>%
    summarise(emprego = sum(emprego), estabelecimentos = sum(estabelecimentos), elos_presentes = n_distinct(elo[emprego > 0]), .groups = "drop")
  tm <- total_mun %>% filter(ano == !!ano) %>% transmute(id_municipio = as.character(id_municipio), emprego_total)
  ids_todos <- as.character(malha$id_municipio)
  # grade completa (todos os municípios em todas as cadeias), para o LISA ver os zeros
  grade <- tidyr::expand_grid(id_municipio = ids_todos, cadeia_id = unique(base$cadeia_id)) %>%
    left_join(base, by = c("id_municipio", "cadeia_id")) %>%
    mutate(across(c(emprego, estabelecimentos, elos_presentes), ~ coalesce(.x, 0))) %>%
    left_join(tm, by = "id_municipio") %>% mutate(emprego_total = coalesce(emprego_total, 0))
  tot_pe <- sum(tm$emprego_total)
  ind <- grade %>% group_by(cadeia_id) %>%
    mutate(emp_cad = sum(emprego),
           pl = ifelse(emprego_total > 0, emprego / emprego_total, 0),
           pr = ifelse(emp_cad > 0, emprego / emp_cad, 0),
           ql = ifelse(emp_cad > 0, pl / (emp_cad / tot_pe), 0),
           ihhm = pr - emprego_total / tot_pe) %>% ungroup()
  nb <- vizinhos_rainha(malha)
  out <- ind %>% group_split(cadeia_id) %>% purrr::map_dfr(function(d) {
    cid <- d$cadeia_id[1]
    if (sum(d$emprego) < emprego_minimo) return(NULL)
    pos <- d$emprego > 0
    w <- pesos_icn(as.matrix(d[pos, c("ql", "ihhm", "pr")]))
    zs <- scale(as.matrix(d[pos, c("ql", "ihhm", "pr")]))
    zs[is.nan(zs)] <- 0
    icn <- rep(NA_real_, nrow(d)); icn[pos] <- as.numeric(zs %*% w)
    rng <- range(icn, na.rm = TRUE)
    d$icn <- if (diff(rng) > 0) 100 * (icn - rng[1]) / diff(rng) else ifelse(pos, 50, NA)
    d$peso_ql <- w[1]; d$peso_ihhm <- w[2]; d$peso_pr <- w[3]
    li <- lisa_local(log1p(d$ql), d$id_municipio, nb, perm = perm)
    d %>% left_join(li, by = "id_municipio")
  })
  if (!nrow(out)) return(NULL)
  out <- out %>% left_join(n_elos, by = "cadeia_id") %>%
    group_by(cadeia_id) %>%
    mutate(corte_icn = stats::quantile(icn[emprego >= emprego_minimo], 0.9, na.rm = TRUE, names = FALSE)) %>% ungroup() %>%
    mutate(completude = ifelse(elos_nucleo > 0, elos_presentes / elos_nucleo, NA_real_),
           classe = case_when(
             emprego >= emprego_minimo & !is.na(icn) & icn >= corte_icn & (lisa == "Alto-Alto" | completude >= 0.5) ~ "Polo do arranjo",
             lisa == "Alto-Alto" & emprego >= 10 ~ "Parte de mancha",
             ql >= 1.5 & emprego >= emprego_minimo ~ "Aglomeração isolada",
             TRUE ~ NA_character_))
  # manchas: componentes conexos de polos e Alto-Alto, cadeia a cadeia
  elos_mun <- cm %>% filter(emprego > 0) %>% distinct(id_municipio, cadeia_id, elo)
  nomes <- municipios %>% transmute(id_municipio = as.character(id_municipio), municipio)
  rd_m <- rd %>% transmute(id_municipio = as.character(id_municipio), rd)
  manchas <- out %>% filter(classe %in% c("Polo do arranjo", "Parte de mancha")) %>% group_split(cadeia_id) %>% purrr::map_dfr(function(d) {
    ids <- d$id_municipio; grupo <- setNames(rep(NA_integer_, length(ids)), ids); g <- 0L
    for (i in ids) if (is.na(grupo[i])) {
      g <- g + 1L; fila <- i
      while (length(fila)) { a <- fila[1]; fila <- fila[-1]; if (!is.na(grupo[a])) next; grupo[a] <- g
        fila <- c(fila, setdiff(intersect(nb[[a]], ids), names(grupo)[!is.na(grupo)])) }
    }
    d %>% mutate(mancha = unname(grupo[id_municipio]))
  })
  resumo <- if (nrow(manchas)) manchas %>% left_join(nomes, by = "id_municipio") %>% left_join(rd_m, by = "id_municipio") %>%
    group_by(cadeia_id, mancha) %>%
    # listas antes da soma: no summarise, "emprego" passaria a ser o total nas linhas seguintes
    summarise(n_municipios = n(), polos = paste(municipio[classe == "Polo do arranjo"][order(-emprego[classe == "Polo do arranjo"])], collapse = ", "),
              municipios = paste(municipio[order(-emprego)], collapse = ", "), rds = paste(sort(unique(rd)), collapse = ", "),
              ids = list(id_municipio), emprego = sum(emprego), .groups = "drop") %>%
    left_join(n_elos, by = "cadeia_id") %>%
    mutate(elos_presentes = purrr::map2_int(cadeia_id, ids, function(c, i) n_distinct(elos_mun$elo[elos_mun$cadeia_id == c & elos_mun$id_municipio %in% i])),
           completude = elos_presentes / elos_nucleo) %>%
    group_by(cadeia_id) %>% arrange(desc(emprego), .by_group = TRUE) %>% mutate(mancha = row_number()) %>% ungroup() %>%
    mutate(cadeia = meta$cadeia[match(cadeia_id, meta$cadeia_id)]) else tibble()
  mapa_mancha <- if (nrow(resumo)) resumo %>% select(cadeia_id, mancha, id_municipio = ids) %>% tidyr::unnest(id_municipio) else
    tibble(cadeia_id = character(), mancha = integer(), id_municipio = character())
  if (nrow(resumo)) resumo <- resumo %>% select(-ids)
  municipio <- out %>% left_join(nomes, by = "id_municipio") %>% left_join(rd_m, by = "id_municipio") %>%
    left_join(mapa_mancha, by = c("cadeia_id", "id_municipio")) %>%
    mutate(cadeia = meta$cadeia[match(cadeia_id, meta$cadeia_id)]) %>%
    select(cadeia_id, cadeia, id_municipio, municipio, rd, emprego, estabelecimentos, ql, pr, ihhm, icn, lisa, lisa_p,
           elos_presentes, elos_nucleo, completude, classe, mancha, peso_ql, peso_ihhm, peso_pr)
  list(ano = ano, municipio = municipio, manchas = resumo, emprego_minimo = emprego_minimo)
}
