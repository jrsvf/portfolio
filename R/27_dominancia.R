# =============================================================================
# Cadeia dominante de cada município
#
# Antes: a cadeia de maior QL. Com isso uma cadeia pequena e muito concentrada
# (300 empregos com QL 5) passava na frente da que de fato move o município
# (5.800 empregos com QL 6), como o audiovisual na frente da confecção em
# Caruaru, e o comércio de móveis e de material de construção pesava como se
# fosse produção.
#
# Agora: só o núcleo produtivo de cada cadeia (sem os elos de comércio e
# distribuição), e entre as cadeias em que o município é especializado
# (QL do núcleo >= 1, com emprego mínimo) vence a de maior emprego excedente,
# isto é, o emprego que o município tem a mais do que teria se seguisse a
# média do estado:
#
#   excedente = emprego do núcleo x (1 - 1 / QL do núcleo)
#
# A medida junta tamanho e especialização: cresce com os dois e é zero quando
# o município está na média estadual.
# =============================================================================

calcular_dominancia <- function(cad_mun, total_mun, elos, meta, minimo = 10) {
  if (!NROW(cad_mun)) return(NULL)
  ano <- max(cad_mun$ano)
  if (!"nucleo" %in% names(elos)) elos$nucleo <- TRUE
  nuc <- elos %>% distinct(cadeia_id, elo, nucleo) %>% group_by(cadeia_id, elo) %>% summarise(nucleo = any(nucleo), .groups = "drop")
  tm <- total_mun %>% filter(ano == !!ano) %>% transmute(id_municipio = as.character(id_municipio), emprego_total)
  tot_pe <- sum(tm$emprego_total)
  cad_mun %>% filter(ano == !!ano) %>% mutate(id_municipio = as.character(id_municipio)) %>%
    inner_join(nuc %>% filter(nucleo), by = c("cadeia_id", "elo")) %>%
    group_by(id_municipio, cadeia_id) %>% summarise(emprego_nucleo = sum(emprego), .groups = "drop") %>%
    left_join(tm, by = "id_municipio") %>%
    group_by(cadeia_id) %>%
    mutate(ql_nucleo = ifelse(emprego_total > 0, (emprego_nucleo / emprego_total) / (sum(emprego_nucleo) / tot_pe), NA_real_)) %>%
    ungroup() %>%
    mutate(especializada = !is.na(ql_nucleo) & ql_nucleo >= 1 & emprego_nucleo >= minimo,
           excedente = ifelse(especializada, emprego_nucleo * (1 - 1 / ql_nucleo), 0),
           ano = ano) %>%
    left_join(meta %>% select(cadeia_id, grupo), by = "cadeia_id") %>%
    group_by(id_municipio) %>%
    mutate(dominante = especializada & grupo == "cadeia" & excedente == max(excedente[especializada & grupo == "cadeia"], -Inf)) %>%
    ungroup() %>%
    select(ano, id_municipio, cadeia_id, grupo, emprego_nucleo, ql_nucleo, excedente, especializada, dominante)
}

# Uma linha por município: a cadeia dominante (só cadeias; nichos ficam de fora)
cadeia_dominante <- function(dominancia, meta = NULL, emprego_minimo = 10) {
  d <- dominancia %>% filter(dominante, emprego_nucleo >= emprego_minimo) %>%
    group_by(id_municipio) %>% slice_max(excedente, n = 1, with_ties = FALSE) %>% ungroup()
  if (!is.null(meta)) d <- d %>% left_join(meta %>% select(cadeia_id, cadeia), by = "cadeia_id")
  d
}

# Leva o núcleo, o excedente e a cadeia dominante para os cartões do raio-X
adicionar_dominancia_raiox <- function(raiox, dominancia) {
  if (is.null(dominancia) || !NROW(dominancia)) return(raiox)
  d <- dominancia %>% transmute(id_municipio, cadeia_id, emprego_nucleo, ql_nucleo = round(ql_nucleo, 2),
                                excedente = round(excedente), dominante)
  lapply(raiox, function(m) {
    dm <- d %>% filter(id_municipio == m$id) %>% select(-id_municipio)
    if (NROW(m$cadeias)) m$cadeias <- m$cadeias %>% left_join(dm, by = "cadeia_id")
    dom <- dm %>% filter(dominante)
    m$dominante <- if (nrow(dom)) dom$cadeia_id[1] else NULL
    m
  })
}
