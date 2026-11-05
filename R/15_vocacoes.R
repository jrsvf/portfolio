# =============================================================================
# Vocações: todas as subclasses da CNAE por município e por RD
#
# As cadeias e nichos cobrem só as atividades configuradas em cadeias.yml.
# Esta camada mede todas as ~1.300 subclasses com emprego formal em PE, para
# achar qualquer atividade (onde ela está) e qualquer território (em que ele é
# especializado). Mesmo QL do emprego formal das cadeias, no último ano da RAIS.
# Também gera o retrato estático de PE (mapa de vocações) em PNG e PDF.
# =============================================================================

montar_vocacoes <- function(rais_pe, cnae, elos, cad_meta, rd, municipios, emprego_minimo = 10) {
  ult <- max(rais_pe$ano)
  x <- rais_pe %>% filter(ano == ult, vinculos > 0) %>%
    group_by(id_municipio, subclasse) %>% summarise(emprego = sum(vinculos), estab = sum(estabelecimentos), .groups = "drop")
  tot_m <- x %>% group_by(id_municipio) %>% summarise(tot_m = sum(emprego), .groups = "drop")
  tot_s <- x %>% group_by(subclasse) %>% summarise(tot_s = sum(emprego), estab_s = sum(estab), n_mun = n_distinct(id_municipio), .groups = "drop")
  T <- sum(x$emprego)
  x <- x %>% left_join(tot_m, by = "id_municipio") %>% left_join(tot_s, by = "subclasse") %>%
    mutate(ql = (emprego / tot_m) / (tot_s / T), pr = emprego / tot_s)
  # RD
  r <- x %>% left_join(rd %>% select(id_municipio, rd), by = "id_municipio") %>% filter(!is.na(rd)) %>%
    group_by(rd, subclasse) %>% summarise(emprego = sum(emprego), estab = sum(estab), .groups = "drop")
  tot_r <- r %>% group_by(rd) %>% summarise(tot_r = sum(emprego), .groups = "drop")
  r <- r %>% left_join(tot_r, by = "rd") %>% left_join(tot_s, by = "subclasse") %>% mutate(ql = (emprego / tot_r) / (tot_s / T), pr = emprego / tot_s)
  # dicionário: nome da subclasse, seção e cadeias/nichos que a usam
  dic <- cnae %>% mutate(subclasse = gsub("\\D", "", subclasse)) %>% distinct(subclasse, .keep_all = TRUE) %>%
    transmute(subclasse, atividade = stringr::str_to_sentence(descricao_subclasse), secao, descricao_secao = stringr::str_to_sentence(descricao_secao))
  cad_de <- elos %>% transmute(cadeia_id, codigo) %>% distinct()
  liga <- tot_s %>% select(subclasse) %>% rowwise() %>%
    mutate(cadeias = paste(unique(cad_de$cadeia_id[cad_de$codigo == subclasse | cad_de$codigo == substr(subclasse, 1, 5)]), collapse = ";")) %>% ungroup()
  ativ <- tot_s %>% left_join(dic, by = "subclasse") %>% left_join(liga, by = "subclasse") %>%
    mutate(atividade = coalesce(atividade, paste("Subclasse", subclasse))) %>% arrange(desc(tot_s))
  list(ano = ult, emprego_minimo = emprego_minimo, atividades = ativ, municipios = x, regioes = r,
       total = T, nomes_cad = setNames(cad_meta$cadeia, cad_meta$cadeia_id))
}

# JSON compacto para a página: índices em vez de textos repetidos
exportar_vocacoes <- function(v, destino = caminho("site", "dados", "vocacoes.json")) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  a <- v$atividades
  idx <- setNames(seq_len(nrow(a)) - 1L, a$subclasse)
  r2 <- function(z) round(z, 2)
  pm <- v$municipios %>% mutate(i = idx[subclasse]) %>% arrange(id_municipio, desc(emprego))
  pr <- v$regioes %>% mutate(i = idx[subclasse]) %>% arrange(rd, desc(emprego))
  obj <- list(
    ano = v$ano, emprego_minimo = v$emprego_minimo, total = v$total,
    atividades = list(cod = a$subclasse, nome = a$atividade, secao = a$secao, nome_secao = a$descricao_secao,
                      emprego = a$tot_s, estab = a$estab_s, n_mun = a$n_mun, cadeias = a$cadeias),
    cadeias = as.list(v$nomes_cad),
    # por município: [atividade, emprego, estabelecimentos, QL]
    municipios = lapply(split(pm, pm$id_municipio), function(d) unname(as.matrix(data.frame(d$i, d$emprego, d$estab, r2(d$ql))))),
    regioes = lapply(split(pr, pr$rd), function(d) unname(as.matrix(data.frame(d$i, d$emprego, d$estab, r2(d$ql))))))
  jsonlite::write_json(obj, destino, auto_unbox = TRUE, digits = NA, na = "null")
  destino
}

# Retrato de PE: cada município colorido pela cadeia ou nicho de maior QL
# (com pelo menos 50 empregos formais nela), com o contorno das RDs e, ao
# lado, as três vocações mais fortes de cada RD.
retrato_vocacoes <- function(locais_cadeia, cad_meta, malha, rd, destino = caminho("site", "imagens")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) { msg("ggplot2 ausente: retrato pulado"); return(character()) }
  library(ggplot2)
  ult <- max(locais_cadeia$ano)
  nome <- setNames(cad_meta$cadeia, cad_meta$cadeia_id)
  macro <- setNames(cad_meta$macrossetor, cad_meta$cadeia_id)
  lc <- locais_cadeia %>% filter(ano == ult, emprego >= 50, ql >= 1)
  top <- lc %>% group_by(id_municipio) %>% slice_max(ql, n = 1, with_ties = FALSE) %>% ungroup() %>%
    mutate(macro = unname(macro[atividade]), cadeia = unname(nome[atividade]))
  m <- malha %>% filter(id_municipio != "2605459") %>% left_join(top %>% select(id_municipio, macro, cadeia, ql), by = "id_municipio")
  m$macro[is.na(m$macro)] <- "Sem especialização forte"
  rdg <- m %>% left_join(rd %>% select(id_municipio, rd), by = "id_municipio") %>% group_by(rd) %>% summarise(.groups = "drop")
  cores <- c("Agropecuária e agroindústria" = "#2E9E5B", "Indústria" = "#023471", "Serviços" = "#35AFE4",
             "Nichos transversais" = "#B8902A", "Nichos produtivos" = "#E07A1F", "Sem especialização forte" = "#E9EDF1")
  # vocações por RD (QL da RD na cadeia, com ao menos 200 empregos)
  vr <- locais_cadeia %>% filter(ano == ult) %>% left_join(rd %>% select(id_municipio, rd), by = "id_municipio") %>%
    group_by(rd, atividade) %>% summarise(e = sum(emprego), .groups = "drop") %>%
    group_by(rd) %>% mutate(tot = sum(e)) %>% ungroup()
  te <- locais_cadeia %>% filter(ano == ult) %>% distinct(id_municipio, emprego_total) %>% left_join(rd %>% select(id_municipio, rd), by = "id_municipio") %>%
    group_by(rd) %>% summarise(et = sum(emprego_total), .groups = "drop")
  sc <- locais_cadeia %>% filter(ano == ult) %>% group_by(atividade) %>% summarise(es = sum(emprego), .groups = "drop")
  ET <- sum(te$et)
  vr <- vr %>% left_join(te, by = "rd") %>% left_join(sc, by = "atividade") %>% mutate(ql = (e / et) / (es / ET)) %>%
    filter(e >= 200) %>% group_by(rd) %>% slice_max(ql, n = 2, with_ties = FALSE) %>%
    summarise(txt = paste0(sub(" \\(.*\\)$", "", nome[atividade]), " (", formatC(ql, format = "f", digits = 1, decimal.mark = ","), ")", collapse = ", "), .groups = "drop") %>%
    mutate(lbl = paste0(rd, ": ", txt))
  # duas colunas de RDs, uma por linha
  n2 <- ceiling(nrow(vr) / 2); larg <- max(nchar(vr$lbl[seq_len(n2)]))
  col2 <- c(vr$lbl[-seq_len(n2)], rep("", 2 * n2 - nrow(vr)))
  leg <- paste(formatC(vr$lbl[seq_len(n2)], width = -larg), col2, sep = "    ", collapse = "\n")
  g <- ggplot(m) + geom_sf(aes(fill = macro), color = "white", linewidth = 0.15) +
    geom_sf(data = rdg, fill = NA, color = "#1E2B38", linewidth = 0.45) +
    scale_fill_manual(values = cores, breaks = names(cores), name = NULL, drop = TRUE) +
    labs(title = "Retrato das vocações de Pernambuco",
         subtitle = paste0("Cada município pela cadeia ou nicho em que é mais especializado (QL do emprego formal maior ou igual a 1, com ao menos 50 empregos), RAIS ", ult),
         caption = paste0("As duas vocações mais fortes de cada Região de Desenvolvimento (QL entre parênteses):\n", leg,
                          "\n\nFonte: RAIS (MTE), via Base dos Dados. Elaboração própria. Fernando de Noronha fora do mapa.")) +
    theme_void(base_size = 11) +
    theme(legend.position = "top", plot.title = element_text(face = "bold", size = 18, color = "#023471"),
          plot.subtitle = element_text(size = 10, color = "#34495E"), plot.caption = element_text(hjust = 0, size = 8.5, color = "#34495E", lineheight = 1.2, family = "mono"),
          plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(18, 18, 18, 18))
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  png <- file.path(destino, "retrato_vocacoes.png"); pdf <- file.path(destino, "retrato_vocacoes.pdf")
  ggsave(png, g, width = 14, height = 7.8, dpi = 200, bg = "white")
  ggsave(pdf, g, width = 14, height = 7.8, bg = "white")
  c(png, pdf)
}
