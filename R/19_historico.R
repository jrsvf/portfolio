# =============================================================================
# Histórico: um retrato do painel por mês
#
# A cada atualização o pipeline grava em historico/AAAA-MM.rds um retrato com
# os números que não têm série própria nas fontes ou que mudam quando uma base
# é revista: empresas ativas, estoque estimado de emprego, vocações de cada
# região, andamento dos programas. Rodar de novo no mesmo mês substitui o
# retrato do mês. A página "O que mudou" compara o retrato mais recente com o
# anterior. Os arquivos são pequenos (só agregados) e ficam na pasta do projeto.
# =============================================================================

montar_retrato <- function(mes_ref, ultimos_anos, cad_mun, total_mun, elos, caged_cad, estoque, comex_cad, comex_pe, empresas, cnpj_cadeias,
                           pib_pop, vocacao = NULL, programas = NULL) {
  if (!"nucleo" %in% names(elos)) elos$nucleo <- TRUE
  linha <- function(indicador, valor, referencia, unidade = "número") tibble(indicador = indicador, valor = as.numeric(valor), unidade = unidade, referencia = as.character(referencia))
  ano_rais <- max(cad_mun$ano); nuc <- elos %>% distinct(cadeia_id, elo, nucleo)

  # ---- fontes: até onde cada uma vai
  fontes <- ultimos_anos %>% transmute(fonte, referencia = as.character(ultimo_ano))
  if (NROW(caged_cad)) fontes <- bind_rows(fontes, tibble(fonte = "Novo CAGED (último mês)", referencia = format(max(caged_cad$data), "%m/%Y")))
  if (!is.null(empresas$data_base) && !is.na(empresas$data_base)) fontes <- bind_rows(fontes, tibble(fonte = "CNPJ (data da base)", referencia = format(empresas$data_base, "%d/%m/%Y")))
  va <- pib_pop %>% filter(!is.na(va_industria))
  if (nrow(va)) fontes <- bind_rows(fontes, tibble(fonte = "PIB dos Municípios, divisão por setor", referencia = as.character(max(va$ano))))

  # ---- Pernambuco
  pe <- linha("Empregos formais (RAIS, 31/12)", sum(total_mun$emprego_total[total_mun$ano == ano_rais], na.rm = TRUE), ano_rais)
  if (!is.null(estoque)) {
    u <- estoque$pe %>% filter(data == max(data)); ref <- format(u$data, "%m/%Y")
    d12 <- estoque$pe %>% filter(data > seq(max(data), by = "-12 months", length.out = 2)[2])
    pe <- bind_rows(pe, linha("Empregos com carteira (estimativa)", u$estoque, ref), linha("Saldo do CAGED em 12 meses", u$saldo_12m, ref),
                    linha("Admissões em 12 meses", sum(d12$admissoes), ref), linha("Desligamentos em 12 meses", sum(d12$desligamentos), ref))
  }
  cx <- comex_pe %>% filter(fluxo == "Exportação")
  if (nrow(cx)) pe <- bind_rows(pe, linha("Exportações no ano", sum(cx$valor_fob[cx$ano == max(cx$ano)], na.rm = TRUE), max(cx$ano), "US$"))
  if (NROW(empresas$estoque)) {
    ref <- if (!is.na(empresas$data_base)) format(empresas$data_base, "%m/%Y") else ""
    pe <- bind_rows(pe, linha("Empresas ativas, sem MEI", sum(empresas$estoque$ativas[!empresas$estoque$mei]), ref),
                    linha("MEI ativos", sum(empresas$estoque$ativas[empresas$estoque$mei]), ref))
  }
  pib <- pib_pop %>% filter(!is.na(pib))
  if (nrow(pib)) pe <- bind_rows(pe, linha("PIB", sum(pib$pib[pib$ano == max(pib$ano)]), max(pib$ano), "R$"))
  if (!is.null(vocacao)) {
    n <- table(factor(vocacao$rd$classe, c("Consolidada", "Emergente", "Potencial")))
    pe <- bind_rows(pe, linha(paste0("Vocações ", tolower(names(n)), "s nas regiões"), as.numeric(n), vocacao$ano),
                    linha("Municípios com vocação destacada", nrow(vocacao$lider), vocacao$ano))
  }

  # ---- cadeias
  cadeia <- cad_mun %>% filter(ano == ano_rais) %>% left_join(nuc, by = c("cadeia_id", "elo")) %>% group_by(cadeia_id) %>%
    summarise(emprego = sum(emprego, na.rm = TRUE), emprego_nucleo = sum(emprego[nucleo], na.rm = TRUE), .groups = "drop")
  if (!is.null(estoque)) cadeia <- cadeia %>% left_join(estoque$cadeia %>% filter(data == max(data)) %>% select(cadeia_id, estoque, estoque_nucleo), by = "cadeia_id")
  if (NROW(caged_cad)) cadeia <- cadeia %>% left_join(caged_cad %>% filter(data == max(data)) %>% select(cadeia_id, any_of(c("saldo_12m", "saldo_nucleo_12m"))), by = "cadeia_id")
  cxc <- comex_cad %>% filter(fluxo == "Exportação")
  if (nrow(cxc)) cadeia <- cadeia %>% left_join(cxc %>% filter(ano == max(ano)) %>% group_by(cadeia_id) %>% summarise(exportacoes = sum(valor_fob, na.rm = TRUE), .groups = "drop"), by = "cadeia_id")
  if (NROW(cnpj_cadeias)) cadeia <- cadeia %>% left_join(cnpj_cadeias %>% filter(nivel == "cadeia") %>% group_by(cadeia_id) %>% summarise(empresas = sum(n), .groups = "drop"), by = "cadeia_id")

  # ---- municípios
  municipio <- total_mun %>% filter(ano == ano_rais) %>% transmute(id_municipio = as.character(id_municipio), emprego = emprego_total)
  if (!is.null(estoque)) municipio <- municipio %>% left_join(estoque$municipio %>% select(id_municipio, estoque, saldo_12m), by = "id_municipio")

  out <- list(ref = mes_ref, gerado_em = Sys.time(), fontes = fontes, pe = pe, cadeia = cadeia, municipio = municipio)
  if (!is.null(vocacao)) {
    out$vocacao <- vocacao$rd %>% filter(!is.na(classe)) %>% select(rd, cadeia_id, classe, indice, emp)
    out$lider <- vocacao$lider %>% select(id_municipio, cadeia_id, classe)
  }
  if (!is.null(programas)) out$programas <- resumo_programas_retrato(programas)
  out
}

# Só os totais dos programas (nada de nomes, CNPJs ou valores por projeto)
resumo_programas_retrato <- function(prog) {
  l <- function(programa, indicador, valor, unidade = "número") tibble(programa = programa, indicador = indicador, valor = as.numeric(valor), unidade = unidade)
  out <- tibble()
  r <- prog$pe_produz$resumo
  if (!is.null(r)) {
    out <- bind_rows(out, l("PE Produz", "Projetos", r$projetos), l("PE Produz", "Entidades", r$entidades), l("PE Produz", "Municípios", r$municipios),
                     l("PE Produz", "Valor ADEPE", r$valor_adepe, "R$"), l("PE Produz", "Beneficiários diretos", r$ben_diretos))
    if (NROW(r$por_status)) out <- bind_rows(out, l("PE Produz", paste("Projetos:", r$por_status$status), r$por_status$projetos))
  }
  r <- prog$obras$resumo
  if (!is.null(r)) out <- bind_rows(out, l("PSM", "Obras concluídas", r$psm_obras), l("PSM", "Municípios", r$psm_municipios), l("PSM", "Valor", r$psm_valor, "R$"))
  r <- prog$exporta_pe$resumo
  if (!is.null(r)) out <- bind_rows(out, l("Exporta PE", "Empresas", r$empresas), l("Exporta PE", "Municípios", r$municipios))
  if (NROW(prog$igs)) out <- bind_rows(out, l("Indicações Geográficas", "IGs acompanhadas", nrow(prog$igs)))
  out
}

salvar_retrato <- function(retrato, pasta = caminho("historico")) {
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  arq <- file.path(pasta, paste0(retrato$ref, ".rds"))
  saveRDS(retrato, arq, compress = "xz")
  arq
}

# Junta todos os retratos em tabelas longas, com a coluna `ref` (AAAA-MM)
ler_historico <- function(pasta = caminho("historico")) {
  arqs <- sort(list.files(pasta, pattern = "^[0-9]{4}-[0-9]{2}\\.rds$", full.names = TRUE))
  if (!length(arqs)) return(list(refs = character()))
  rs <- lapply(arqs, readRDS)
  junta <- function(campo) bind_rows(lapply(rs, function(r) if (NROW(r[[campo]])) mutate(r[[campo]], ref = r$ref, .before = 1)))
  list(refs = vapply(rs, function(r) r$ref, ""), gerado_em = do.call(c, lapply(rs, function(r) r$gerado_em)),
       fontes = junta("fontes"), pe = junta("pe"), cadeia = junta("cadeia"), municipio = junta("municipio"),
       vocacao = junta("vocacao"), lider = junta("lider"), programas = junta("programas"))
}

# Retrato atual contra o anterior, lado a lado. `chaves` identifica a linha.
comparar_retratos <- function(tab, chaves, valores, ref_atual, ref_antes) {
  if (!NROW(tab) || is.na(ref_antes)) return(tibble())
  a <- tab %>% filter(ref == ref_atual) %>% select(all_of(c(chaves, valores)))
  b <- tab %>% filter(ref == ref_antes) %>% select(all_of(c(chaves, valores)))
  full_join(a, b, by = chaves, suffix = c("", "_antes"))
}

rotulo_ref <- function(ref) {
  m <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
  paste(m[as.integer(substr(ref, 6, 7))], "de", substr(ref, 1, 4))
}
