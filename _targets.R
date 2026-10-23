# =============================================================================
# Pipeline do portfólio (targets)
# =============================================================================
# Rodar tudo:            targets::tar_make()
# Ver o grafo:           targets::tar_visnetwork()
# Forçar nova extração:  targets::tar_invalidate(starts_with("bq_"))
#
# O targets só refaz o que mudou. No GitHub Actions o pipeline roda do zero
# todo mês (o cache não é reaproveitado), então as fontes são sempre
# consultadas de novo.
# =============================================================================

library(targets)

tar_option_set(
  packages = c("dplyr", "tidyr", "purrr", "stringr", "yaml", "bigrquery", "glue",
               "readr", "readxl", "jsonlite", "sf", "geobr"),
  format = "rds",
  error = "stop"
)

# só as funções do pipeline; scripts avulsos (análises internas, publicação) ficam de fora
for (f in sort(list.files("R", pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) {
  if (!grepl("impacto", f)) source(f)
}

# Cortes de tempo definidos em config/parametros.yml valem para todo o pipeline
local({
  p <- yaml::read_yaml("config/parametros.yml")$comparacoes
  if (!is.null(p)) CORTES_PADRAO <<- list(janelas = p$janelas, desde = p$desde)
})

list(
  # --------------------------------------------------------------- configuração
  tar_target(arq_parametros, "config/parametros.yml", format = "file"),
  tar_target(arq_cadeias,    "config/cadeias.yml",    format = "file"),
  tar_target(arq_mip_ativ,   "config/mip_atividades.csv", format = "file"),
  tar_target(arq_sh4_cnae,   "config/sh4_cnae.csv", format = "file"),
  tar_target(par, ler_parametros(arq_parametros)),
  tar_target(cad, { arq_sh4_cnae; ler_cadeias(arq_cadeias) }),
  tar_target(uf, par$territorio$uf_sigla),
  # recortes do `par`: mudar um parâmetro só refaz o que depende dele
  tar_target(anos, par$anos),
  tar_target(uf_codigo, par$territorio$uf_codigo),
  tar_target(par_ind, par$indicadores),
  tar_target(par_mip, par$mip),
  tar_target(janelas, par$comparacoes$janelas %||% CORTES_PADRAO$janelas),
  tar_target(desde, par$comparacoes$desde %||% CORTES_PADRAO$desde),
  tar_target(projeto, par$bigquery$projeto),

  # ------------------------------------------------------------------ extração
  tar_target(bq_auth, autenticar_bq()),
  tar_target(bq_esquema, { bq_auth; verificar_esquema(projeto) }),
  tar_target(bq_municipios, { bq_esquema; extrair_municipios(projeto, uf) }),
  tar_target(bq_cnae,       { bq_esquema; extrair_cnae(projeto) }),
  tar_target(codigos_ok, conferir_codigos(cad$elos, bq_cnae)),
  tar_target(bq_paises,     { bq_esquema; obter_paises(projeto) }),
  tar_target(bq_rais_pe,    { bq_esquema; extrair_rais_estab_uf(projeto, uf, anos$rais_inicio) }),
  tar_target(bq_rais_br,    { bq_esquema; extrair_rais_estab_brasil(projeto, anos$rais_inicio) }),
  tar_target(bq_perfil_pe,  { bq_esquema; extrair_rais_perfil_uf(projeto, uf, anos$rais_inicio) }),
  tar_target(bq_caged_pe,   { bq_esquema; extrair_caged_uf(projeto, uf, anos$caged_inicio) }),
  tar_target(bq_comex_pe,   { bq_esquema; extrair_comex_uf(projeto, uf, anos$comex_inicio) }),
  tar_target(bq_pib_pop,    { bq_esquema; extrair_pib_pop_uf(projeto, uf) }),
  tar_target(sidra_bruto,
             extrair_sidra(cad$sidra$tabela, anos$sidra_inicio,
                           uf_codigo, anos$sidra_fim)),
  tar_target(malha, obter_malha(uf_codigo)),
  tar_target(ipca, tryCatch(extrair_ipca(), error = function(e) { msg("IPCA indisponível: ", conditionMessage(e)); NULL })),
  tar_target(rd, ler_rd()),
  tar_target(bq_pop,       { bq_esquema; extrair_pop_uf(projeto, uf) }),
  tar_target(bq_sh4_nomes, { bq_esquema; extrair_sh4_nomes(projeto) }),
  # nível de atividade: falha suave se a fonte estiver fora do ar
  tar_target(macro_ibcr, tryCatch(obter_sgs(), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(macro_pim,  tryCatch(obter_pim(uf_codigo = uf_codigo), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(pim_setores, tryCatch(obter_pim_setores(uf_codigo = uf_codigo), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(pim_resumo, resumo_pim_setores(pim_setores)),
  tar_target(pim_cadeias, ler_pim_cadeias(arq_cadeias)),

  # -------------------------------------------------------------- cadeias
  tar_target(cad_mun, { codigos_ok; agregar_cadeias(bq_rais_pe, cad$elos) }),
  tar_target(total_mun, bq_rais_pe %>% group_by(ano, id_municipio) %>%
               summarise(emprego_total = sum(vinculos, na.rm = TRUE), .groups = "drop")),
  tar_target(locais_cadeia,
             cad_mun %>%
               group_by(ano, id_municipio, atividade = cadeia_id) %>%
               summarise(emprego = sum(emprego), estabelecimentos = sum(estabelecimentos), .groups = "drop") %>%
               indicadores_locais(total_mun)),
  tar_target(locais_elo,
             cad_mun %>%
               mutate(atividade = paste(cadeia_id, elo, sep = "||")) %>%
               select(ano, id_municipio, atividade, emprego, estabelecimentos) %>%
               indicadores_locais(total_mun) %>%
               separate(atividade, c("cadeia_id", "elo"), sep = "\\|\\|")),
  tar_target(concentracao,
             concentracao_espacial(locais_cadeia, par_ind$ql_minimo, par_ind$emprego_minimo)),
  tar_target(serie_comp, serie_comparada(bq_rais_br, cad$elos, uf)),
  tar_target(caged_cad, caged_cadeia(bq_caged_pe, cad$elos)),
  # o ano corrente ainda está incompleto no ComexStat: fica de fora das comparações anuais
  tar_target(comex_anos_completos, bq_comex_pe %>% filter(ano < as.numeric(format(Sys.Date(), "%Y")))),
  tar_target(comex_cad,
             comex_cadeia(comex_anos_completos, cad$sh4) %>%
               mutate(id_pais = as.character(as.integer(id_pais))) %>%
               left_join(bq_paises, by = "id_pais") %>%
               mutate(pais = coalesce(pais, paste("País", id_pais)),
                      sigla = coalesce(if ("sigla" %in% names(.)) sigla else NA_character_, id_pais))),
  tar_target(sidra_cad, sidra_por_cadeia(sidra_bruto, cad$sidra)),
  tar_target(perfil_cad,
             bq_perfil_pe %>%
               ligar_elos(cad$elos, "cadeia_id") %>%
               group_by(ano, cadeia_id) %>%
               summarise(across(c(remuneracao_media, pct_mulheres, pct_superior),
                                ~ weighted.mean(.x, vinculos_ativos, na.rm = TRUE)),
                         vinculos = sum(vinculos_ativos), .groups = "drop")),
  tar_target(perfil_estado,
             bq_perfil_pe %>% group_by(ano) %>%
               summarise(across(c(remuneracao_media, pct_mulheres, pct_superior),
                                ~ weighted.mean(.x, vinculos_ativos, na.rm = TRUE)), .groups = "drop")),
  tar_target(shift_share_cad, {
    a1 <- max(serie_comp$ano)
    a0 <- if (is.numeric(par_ind$shift_share_inicio)) par_ind$shift_share_inicio else a1 - max(janelas)
    e <- serie_comp %>% filter(ano %in% c(a0, a1)) %>%
      mutate(grupo = ifelse(recorte == "Pernambuco", "reg", "resto"),
             per = ifelse(ano == a0, "e0", "e1")) %>%
      group_by(cadeia_id, per, grupo) %>% summarise(e = sum(emprego), .groups = "drop") %>%
      pivot_wider(names_from = grupo, values_from = e, values_fill = 0) %>%
      mutate(br = reg + resto)
    reg <- e %>% select(atividade = cadeia_id, per, v = reg) %>%
      pivot_wider(names_from = per, values_from = v, values_fill = 0)
    ref <- e %>% select(atividade = cadeia_id, per, v = br) %>%
      pivot_wider(names_from = per, values_from = v, values_fill = 0)
    shift_share(reg, ref) %>% mutate(ano_ini = a0, ano_fim = a1)
  }),

  # ------------------------------------------------- comparações (tempo e Nordeste)
  tar_target(cad_uf, cadeia_por_uf(bq_rais_br, cad$elos)),
  tar_target(tot_uf, total_por_uf(bq_rais_br)),
  tar_target(cmp_ne, comparar_nordeste(cad_uf, tot_uf, uf, janelas, desde)),
  tar_target(janelas_cad, janelas_cadeia(cad_mun, perfil_cad, comex_cad, sidra_cad, ipca, cmp_ne, janelas, desde)),
  tar_target(janelas_uf,
             variacoes_janela(tot_uf %>% filter(sigla_uf %in% UFS_NORDESTE) %>% transmute(ano, sigla_uf, valor = emprego_total),
                              "sigla_uf", janelas, desde)),
  tar_target(fontes_datas, fontes_e_datas(bq_rais_pe, bq_caged_pe, comex_anos_completos, bq_pib_pop, sidra_bruto, ipca, par_mip$ano)),

  # ---------------------------------------------------------- panorama estadual
  tar_target(rais_secao,
             bq_rais_pe %>% left_join(bq_cnae %>% distinct(classe, secao, descricao_secao), by = "classe") %>%
               mutate(secao = coalesce(secao, "?"), descricao_secao = coalesce(descricao_secao, "Não classificado")) %>%
               group_by(ano, id_municipio, secao, descricao_secao) %>%
               summarise(emprego = sum(vinculos, na.rm = TRUE), .groups = "drop")),
  tar_target(krugman, indice_krugman(rais_secao %>% rename(setor = secao))),
  tar_target(serie_uf,
             bq_rais_br %>% group_by(ano) %>%
               summarise(brasil = sum(vinculos), pe = sum(vinculos[sigla_uf == uf]), .groups = "drop") %>%
               mutate(part = pe / brasil)),

  # ------------------------------------------------------------- encadeamentos
  tar_target(mip, { arq_mip_ativ; calcular_encadeamentos(bq_rais_br, list(mip = par_mip), uf) }),
  tar_target(mip_cadeia, {
    if (is.null(mip)) return(NULL)
    cad$elos %>% mutate(grupo = setor_da_classe(classe, ler_mip_atividades())) %>%
      distinct(cadeia_id, elo, grupo) %>%
      left_join(mip$regional %>% select(grupo, grupo_nome, bl, fl, mult_producao, classificacao, slq, presenca), by = "grupo")
  }),

  # --------------------------------------------------------- lacunas/destaques
  tar_target(ultimos_anos, tibble::tibble(
    fonte = c("RAIS", "Novo CAGED", "ComexStat", "PIB dos Municípios", "Produção agropecuária (SIDRA)"),
    ultimo_ano = c(max(bq_rais_pe$ano), max(bq_caged_pe$ano), max(comex_anos_completos$ano),
                   max(bq_pib_pop$ano[!is.na(bq_pib_pop$pib)]),
                   if (nrow(sidra_bruto)) max(sidra_bruto$ano) else NA_real_))),
  tar_target(lacunas,
             detectar_lacunas(cad_mun, sidra_cad, comex_cad, cad$meta, cad$elos, sidra_bruto, ultimos_anos) %>%
               left_join(bq_municipios %>% select(id_municipio, municipio), by = "id_municipio") %>%
               left_join(cad$meta %>% select(cadeia_id, cadeia), by = "cadeia_id")),
  tar_target(destaques,
             setNames(lapply(cad$meta$cadeia_id, gerar_destaques,
                             cad_mun = cad_mun, municipios = bq_municipios, serie_comp = serie_comp,
                             sidra_cad = sidra_cad, comex_cad = comex_cad, perfil_cad = perfil_cad,
                             perfil_estado = perfil_estado,
                             ql_minimo = par_ind$ql_minimo,
                             emprego_minimo = par_ind$emprego_minimo) %>%
                        map2(cad$meta$cadeia_id, ~ c(.x, destaques_nordeste(.y, cmp_ne))),
                      cad$meta$cadeia_id)),

  # ---------------------------------------------------------------- saídas
  tar_target(saida_rds, {
    objs <- list(par = par, cad = cad, municipios = bq_municipios, cnae = bq_cnae, malha = malha, rd = rd,
                 cad_mun = cad_mun, locais_cadeia = locais_cadeia, locais_elo = locais_elo,
                 concentracao = concentracao, serie_comp = serie_comp, caged_cad = caged_cad,
                 comex_cad = comex_cad, sidra_cad = sidra_cad, perfil_cad = perfil_cad,
                 perfil_estado = perfil_estado, shift_share_cad = shift_share_cad,
                 rais_secao = rais_secao, krugman = krugman, serie_uf = serie_uf,
                 pib_pop = bq_pib_pop, comex_pe = comex_anos_completos, mip = mip, mip_cadeia = mip_cadeia,
                 lacunas = lacunas, lacunas_estruturais = LACUNAS_ESTRUTURAIS,
                 destaques = destaques, ultimos_anos = ultimos_anos,
                 cmp_ne = cmp_ne, janelas_cad = janelas_cad, janelas_uf = janelas_uf,
                 fontes_datas = fontes_datas, ipca = ipca,
                 macro_ibcr = macro_ibcr, macro_pim = macro_pim,
                 pim_setores = pim_setores, pim_resumo = pim_resumo, pim_cadeias = pim_cadeias,
                 atualizado_em = Sys.time())
    unname(imap_chr(objs, salvar_base))
  }, format = "file"),
  tar_target(saida_diag, exportar_diag(locais_cadeia, cad$meta, bq_municipios, malha, rd), format = "file"),
  tar_target(saida_csv, exportar_csv(list(
    emprego_cadeia_municipio = locais_cadeia %>% left_join(bq_municipios, by = "id_municipio"),
    emprego_elo_municipio    = locais_elo %>% left_join(bq_municipios, by = "id_municipio"),
    caged_cadeia             = caged_cad,
    comex_cadeia             = comex_cad,
    producao_ibge_cadeia     = sidra_cad,
    lacunas                  = lacunas
  )), format = "file"),
  tar_target(arq_modelo_cadeia, "site/_modelos/cadeia.qmd", format = "file"),
  tar_target(paginas_cadeias, gerar_paginas_cadeias(cad$meta, modelo = arq_modelo_cadeia), format = "file"),

  # ------------------------------------------------- raio-X dos municípios
  tar_target(raiox,
             montar_raiox(bq_municipios, rd, bq_pib_pop, bq_pop, rais_secao, bq_perfil_pe, bq_caged_pe,
                          comex_anos_completos, bq_sh4_nomes, sidra_bruto, locais_cadeia, sidra_cad,
                          comex_cad, cad$meta, krugman, ipca, locais_elo)),
  tar_target(saida_raiox, exportar_raiox(raiox, malha), format = "file")
)
