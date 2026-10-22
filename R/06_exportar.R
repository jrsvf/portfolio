# =============================================================================
# Exportações finais: malha municipal, base do app "diag" e dados abertos
# =============================================================================

# Malha municipal simplificada (geobr). Guardada em dados/ para o site e apps.
obter_malha <- function(uf_codigo, ano = 2022) {
  m <- geobr::read_municipality(code_muni = uf_codigo, year = ano, showProgress = FALSE, simplified = TRUE)
  m %>%
    mutate(id_municipio = as.character(code_muni)) %>%
    select(id_municipio, name_muni, geom) %>%
    sf::st_transform(4326)
}

# Regiões de Desenvolvimento: config/rd_municipios.csv (id_municipio, rd).
ler_rd <- function(arquivo = caminho("config", "rd_municipios.csv")) {
  if (!file.exists(arquivo)) {
    msg("Aviso: config/rd_municipios.csv não encontrado. Região = 'Não informada'.")
    return(tibble(id_municipio = character(), rd = character()))
  }
  readr::read_csv(arquivo, col_types = "cc")
}

# Gera o dados_ambiente.RData no formato que o app diag (shinyapps.io/diag)
# já espera: dados_apl, dados_apl_final e mapa_pe. Assim o app antigo passa a
# ser atualizado pelo mesmo pipeline, sem mudar o código dele.
exportar_diag <- function(locais_cadeia, cad_meta, municipios, malha, rd,
                          destino = caminho("shiny", "diag", "dados_ambiente.RData")) {
  ult <- max(locais_cadeia$ano)
  dados_apl <- locais_cadeia %>%
    filter(ano == ult, emprego > 0) %>%
    left_join(cad_meta %>% select(cadeia_id, cadeia, macrossetor), by = c("atividade" = "cadeia_id")) %>%
    left_join(municipios %>% select(id_municipio, municipio), by = "id_municipio") %>%
    left_join(rd, by = "id_municipio") %>%
    transmute(
      municipio_nome   = municipio,
      regiao           = coalesce(rd, "Não informada"),
      setor            = macrossetor,
      atividade        = cadeia,
      estabelecimentos = estabelecimentos,
      Emprego          = emprego,
      QL               = round(ql, 3),
      PR               = round(100 * pr, 2)
    )

  mapa_pe <- malha
  base <- locais_cadeia %>% filter(ano == ult, emprego > 0) %>%
    left_join(cad_meta %>% select(cadeia_id, cadeia, macrossetor), by = c("atividade" = "cadeia_id"))
  dados_apl_final <- malha %>%
    inner_join(base, by = "id_municipio") %>%
    transmute(municipio_nome = name_muni, atividade = cadeia, setor = macrossetor,
              Emprego = emprego, Estabelecimentos = estabelecimentos,
              QL = round(ql, 3), PR = round(100 * pr, 2))

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  save(dados_apl, dados_apl_final, mapa_pe, file = destino)
  destino
}

# Dados abertos: CSVs publicados no site para download
exportar_csv <- function(lista, pasta = caminho("site", "dados")) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  arqs <- imap_chr(lista, function(df, nome) {
    arq <- file.path(pasta, paste0(nome, ".csv"))
    readr::write_csv2(sf::st_drop_geometry(df), arq, na = "")
    arq
  })
  unname(arqs)
}
