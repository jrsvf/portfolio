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
  # mês da rodada: quando vira o mês, o CAGED é consultado de novo e o histórico ganha um retrato novo
  tar_target(mes_ref, format(Sys.Date(), "%Y-%m"), cue = tar_cue(mode = "always")),

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
  tar_target(bq_caged_bd,   { bq_esquema; mes_ref; extrair_caged_uf(projeto, uf, anos$caged_inicio) }),
  # meses recentes que a Base dos Dados ainda não liberou no acesso gratuito: direto do MTE (R/23_caged_mte.R)
  tar_target(bq_caged_pe,   completar_caged_mte(bq_caged_bd, bq_municipios, uf_codigo), cue = tar_cue(mode = "always")),
  # celetistas da RAIS: ponto de partida do estoque mensal de emprego (se falhar, o estoque usa a RAIS sem administração pública)
  tar_target(bq_rais_clt,   { bq_esquema; tentar("RAIS celetistas", extrair_rais_clt_uf(projeto, uf, max(bq_rais_pe$ano) - 1)) }),
  tar_target(bq_comex_pe,   { bq_esquema; extrair_comex_uf(projeto, uf, anos$comex_inicio) }),
  tar_target(bq_pib_bruto,  { bq_esquema; extrair_pib_pop_uf(projeto, uf) }),
  # divisão do PIB por setor: completa pelo SIDRA os anos que a Base dos Dados ainda não tem, se o IBGE já divulgou
  tar_target(bq_pib_pop,    completar_va_sidra(bq_pib_bruto, uf_codigo)),
  # só as tabelas importam: mudar o padrão de um produto em cadeias.yml não refaz o download
  tar_target(sidra_tabelas, sort(unique(cad$sidra$tabela))),
  tar_target(sidra_bruto,
             tryCatch(extrair_sidra(sidra_tabelas, anos$sidra_inicio, uf_codigo, anos$sidra_fim), error = function(e) {
               # API do IBGE fora do ar: mantém a base da última atualização em vez de parar o painel
               anterior <- file.path("_targets", "objects", "sidra_bruto")
               if (!file.exists(anterior)) stop(e)
               msg("SIDRA indisponível (", conditionMessage(e), "). Mantida a base anterior.")
               readRDS(anterior)
             })),
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
  # PMC e PMS: totais (com e sem ajuste) e por atividade (sem ajuste)
  tar_target(macro_pmc, tryCatch(obter_conjuntura(8880, "PMC"), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(macro_pms, tryCatch(obter_conjuntura(5906, "PMS"), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(pmc_ativ, tryCatch(obter_conjuntura(8883, "PMC", atividades = TRUE, com_ajuste = FALSE), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(pmc_ampl, tryCatch(obter_conjuntura(8881, "PMC", com_ajuste = FALSE), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(pms_ativ, tryCatch(obter_conjuntura(8693, "PMS", atividades = TRUE, com_ajuste = FALSE), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(pms_tur, tryCatch(obter_conjuntura(8694, "PMS", com_ajuste = FALSE), error = function(e) tibble::tibble()), cue = tar_cue(mode = "always")),
  tar_target(conj_setores, juntar_conjuntura(pim_setores, pmc_ativ, pmc_ampl, pms_ativ, pms_tur)),
  tar_target(conj_resumo, resumo_pim_setores(conj_setores)),
  tar_target(conj_cadeias, ler_conj_cadeias(arq_cadeias)),

  # ---- território: empresas, qualificação, educação, ICMS, agricultura familiar,
  #      crédito rural, infraestrutura e o corte do ano corrente
  tar_target(bq_dic,          { bq_esquema; tentar("dicionários BD", extrair_dicionarios_bd(projeto)) }),
  tar_target(bq_cnpj_fluxos,  { bq_esquema; tentar("CNPJ fluxos", extrair_cnpj_fluxos(projeto, uf)) }),
  tar_target(bq_cnpj_maiores, { bq_esquema; tentar("CNPJ maiores", extrair_cnpj_maiores(projeto, uf, cad$elos)) }),
  tar_target(bq_rais_qualif,  { bq_esquema; u <- max(bq_rais_pe$ano)
                                tentar("RAIS qualificação", extrair_rais_qualif(projeto, uf, c(u - 10, u - 5, u))) }),
  tar_target(bq_superior,     { bq_esquema; tryCatch(extrair_superior(projeto, uf, 2014), error = function(e) {
                                  msg("INEP superior falhou: ", conditionMessage(e)); list(cursos = tibble::tibble(), ies = tibble::tibble()) }) }),
  tar_target(bq_tecnico,      { bq_esquema; tentar("Censo Escolar técnico", extrair_tecnico(projeto, uf, 2014)) }),
  tar_target(bq_icms,         { bq_esquema; tryCatch(extrair_icms(projeto, uf), error = function(e) {
                                  msg("ICMS falhou: ", conditionMessage(e)); list(mun = tibble::tibble(), uf = tibble::tibble()) }) }),
  tar_target(bq_pronaf,       { bq_esquema; tentar("PRONAF", extrair_pronaf(projeto, uf)) }),
  tar_target(bq_comex_mensal, { bq_esquema; tentar("comex mensal", extrair_comex_mensal(projeto, uf, as.numeric(format(Sys.Date(), "%Y")) - 1)) },
             cue = tar_cue(mode = "always")),
  tar_target(arq_cadeia_cursos, "config/cadeia_cursos.csv", format = "file"),
  tar_target(bq_cursos_nome,  { bq_esquema; tentar("cursos por nome", extrair_cursos_nome(projeto, uf)) }),
  tar_target(bq_escoamento,   { bq_esquema; tentar("escoamento", extrair_comex_escoamento(projeto, uf)) }),
  tar_target(nomes_urf,       { bq_esquema; obter_nomes_comex(projeto) }),
  tar_target(cursos_cad,      cursos_cadeia(bq_cursos_nome, ler_cadeia_cursos(arq_cadeia_cursos), bq_dic, bq_municipios)),
  # formação por município: superior presencial e a distância, técnico (pelo nome do curso) e cursos novos fora do Censo
  tar_target(nomes_tec,       obter_cursos_tecnicos(), cue = tar_cue(mode = "always")),
  tar_target(bq_tecnico_cursos, { bq_esquema; tentar("cursos técnicos", extrair_tecnico_cursos(projeto, uf)) }),
  tar_target(arq_cursos_novos, "config/cursos_novos.csv", format = "file"),
  tar_target(formacao,        montar_formacao(bq_cursos_nome, bq_tecnico_cursos, nomes_tec, ler_cursos_novos(arq_cursos_novos),
                                              ler_cadeia_cursos(arq_cadeia_cursos), bq_dic, bq_municipios, rd)),
  # cooperativas e associações (CNPJ)
  tar_target(bq_associativas, { bq_esquema; tentar("CNPJ cooperativas e associações", extrair_cnpj_associativas(projeto, uf)) }),
  tar_target(arq_assoc_cad,   "config/associativas_cadeias.csv", format = "file"),
  tar_target(associativas,    montar_associativas(bq_associativas, cad$elos, cad$meta, ler_associativas_cadeias(arq_assoc_cad), bq_cnae, bq_municipios, rd)),
  tar_target(escoamento_cad,  escoamento_cadeia(bq_escoamento, cad$sh4, nomes_urf)),
  tar_target(agro_familiar,   tentar("Censo Agro", obter_agro_familiar(uf_codigo))),
  tar_target(empresas,         montar_empresas(bq_cnpj_fluxos, bq_dic, bq_cnae)),
  tar_target(empresas_maiores, montar_maiores(bq_cnpj_maiores, bq_dic, bq_cnae)),
  tar_target(qualificacao,     montar_qualif(bq_rais_qualif)),
  tar_target(educacao,         montar_educacao(bq_superior, bq_tecnico, bq_dic)),
  tar_target(icms,             montar_icms(bq_icms)),
  tar_target(credito_rural,    montar_credito_rural(bq_pronaf, bq_dic)),
  tar_target(arq_infra,        "config/infraestrutura.csv", format = "file"),
  tar_target(infraestrutura,   ler_infraestrutura(arq_infra)),
  tar_target(corte_atual,      corte_ano(macro_ibcr, macro_pim, macro_pmc, macro_pms, bq_caged_pe, bq_comex_mensal, bq_cnae, bq_municipios)),

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
  # camada territorial: semiárido, tipologia rural-urbana, acessibilidade, REGIC e água (R/26_camada_territorial.R)
  tar_target(arq_territorio, baixar_territorio(), cue = tar_cue(mode = "always")),
  tar_target(camada_territorial, montar_camada_territorial(arq_territorio, bq_municipios, rd, uf_codigo)),
  # arranjo produtivo: ICN, LISA e completude dos elos (R/25_arranjo.R)
  tar_target(arranjo, montar_arranjo(cad_mun, total_mun, cad$elos, cad$meta, malha, bq_municipios, rd)),
  # cadeia dominante: a especializada (QL do núcleo >= 1) com maior emprego excedente
  tar_target(dominancia, calcular_dominancia(cad_mun, total_mun, cad$elos, cad$meta, minimo = par$indicadores$emprego_minimo)),
  tar_target(caged_cad, caged_cadeia(bq_caged_pe, cad$elos)),
  tar_target(caged_elo, caged_elos(bq_caged_pe, cad$elos)),
  # estoque mensal de emprego com carteira: RAIS de dezembro + saldos do CAGED
  tar_target(estoque, estoque_emprego(bq_rais_pe, bq_caged_pe, cad$elos, bq_cnae, bq_rais_clt)),
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
  tar_target(fontes_datas, bind_rows(
    fontes_e_datas(bq_rais_pe, bq_caged_pe, comex_anos_completos, bq_pib_pop, sidra_bruto, ipca, par_mip$ano),
    fontes_extras(empresas, cursos_cad, educacao, icms, credito_rural, bq_escoamento, macro_ibcr, macro_pim, macro_pmc, macro_pms))),

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

  # ------------------------------------------- matriz interestadual NEREUS 2019
  tar_target(arq_nereus, { f <- list.files("dados_externos/nereus", pattern = "\\.(rds|xlsx|rar|zip)$", full.names = TRUE)
                           if (length(f)) f else "config/sem_arquivo.txt" }, format = "file"),
  tar_target(nereus, { arq_nereus; arq_mip_ativ; tryCatch(analisar_nereus(obter_nereus(), ler_mip_atividades()), error = function(e) { msg("NEREUS falhou: ", conditionMessage(e)); NULL }) }),
  tar_target(nereus_cad, nereus_cadeia(nereus, mip_cadeia)),

  # ------------------------------------------- QL por emprego, empresas, exportação e produção
  tar_target(bq_cnpj_cadeias, { bq_esquema; tentar("CNPJ por cadeia", extrair_cnpj_cadeias(projeto, uf, cad$elos)) }),
  tar_target(ql_emp, ql_empresas(bq_cnpj_cadeias)),
  tar_target(ql_exp, ql_exportacoes(comex_anos_completos, comex_cad)),
  tar_target(ql_prod, ql_producao_agro(sidra_bruto, sidra_cad)),
  tar_target(ql_mult, montar_ql_multiplo(locais_cadeia, ql_emp, ql_exp, ql_prod)),
  tar_target(ql_vab, ql_vab_municipios(bq_pib_pop)),

  # --------------------------------------- vocações: todas as subclasses CNAE
  tar_target(vocacoes, montar_vocacoes(bq_rais_pe, bq_cnae, cad$elos, cad$meta, rd, bq_municipios, par_ind$emprego_minimo)),
  tar_target(saida_vocacoes, exportar_vocacoes(vocacoes), format = "file"),
  tar_target(saida_retrato, retrato_vocacoes(locais_cadeia, cad$meta, malha, rd, dominancia = dominancia), format = "file"),
  tar_target(producao_terr, montar_producao(sidra_bruto, rd)),
  tar_target(saida_producao, exportar_producao(producao_terr), format = "file"),

  # ------------- vocações por cadeia: consolidadas, emergentes e potenciais (RD e município)
  tar_target(vocacao_cad, montar_vocacao(cad_mun, total_mun, cad$elos, cad$meta, rd, bq_municipios, bq_cnpj_cadeias, comex_cad,
                                         comex_anos_completos, ql_prod, producao_terr, sidra_cad, malha)),

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
                        map2(cad$meta$cadeia_id, ~ c(.x, destaques_extras(.y, escoamento_cad, cursos_cad), destaques_nordeste(.y, cmp_ne))),
                      cad$meta$cadeia_id)),

  # ---------------------------------------------------------------- saídas
  # vigia: acusa fonte vazia ou parada no tempo (R/24_vigia.R)
  tar_target(vigia, vigiar_fontes(list(bq_rais_pe = bq_rais_pe, bq_caged_pe = bq_caged_pe, bq_comex_pe = bq_comex_pe, bq_pib_pop = bq_pib_pop,
                                       sidra_bruto = sidra_bruto, macro_ibcr = macro_ibcr, macro_pim = macro_pim, macro_pmc = macro_pmc,
                                       macro_pms = macro_pms, bq_cnpj_cadeias = bq_cnpj_cadeias, bq_associativas = bq_associativas,
                                       bq_cursos_nome = bq_cursos_nome, bq_tecnico_cursos = bq_tecnico_cursos,
                                       camada_territorial = camada_territorial)),
             cue = tar_cue(mode = "always")),
  tar_target(saida_rds, {
    objs <- list(par = par, cad = cad, municipios = bq_municipios, cnae = bq_cnae, malha = malha, rd = rd,
                 cad_mun = cad_mun, locais_cadeia = locais_cadeia, locais_elo = locais_elo, dominancia = dominancia,
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
                 macro_pmc = macro_pmc, macro_pms = macro_pms, conj_setores = conj_setores,
                 conj_resumo = conj_resumo, conj_cadeias = conj_cadeias,
                 empresas = empresas, empresas_maiores = empresas_maiores, qualificacao = qualificacao,
                 educacao = educacao, icms = icms, credito_rural = credito_rural, agro_familiar = agro_familiar,
                 infraestrutura = infraestrutura, corte_atual = corte_atual, bq_dic = bq_dic,
                 cursos_cad = cursos_cad, escoamento_cad = escoamento_cad,
                 nereus = nereus, nereus_cad = nereus_cad, ql_mult = ql_mult, ql_vab = ql_vab,
                 caged_elo = caged_elo, estoque = estoque, vocacao_cad = vocacao_cad, historico = historico,
                 formacao = formacao, associativas = associativas, vigia = vigia, arranjo = arranjo, camada_territorial = camada_territorial,
                 atualizado_em = Sys.time())
    unname(imap_chr(objs, salvar_base))
  }, format = "file"),
  tar_target(saida_diag, exportar_diag(locais_cadeia, cad$meta, bq_municipios, malha, rd), format = "file"),
  tar_target(saida_csv, exportar_csv(list(
    emprego_cadeia_municipio = locais_cadeia %>% left_join(bq_municipios, by = "id_municipio"),
    emprego_elo_municipio    = locais_elo %>% filter(ano >= max(ano) - 10) %>% left_join(bq_municipios, by = "id_municipio"),
    caged_cadeia             = caged_cad,
    caged_elos_12_meses      = caged_elo,
    estoque_emprego_pe       = if (is.null(estoque)) tibble::tibble() else estoque$pe,
    estoque_emprego_cadeia   = if (is.null(estoque)) tibble::tibble() else estoque$cadeia,
    vocacoes_cadeias_rd      = vocacao_tabela(vocacao_cad),
    formacao_cursos          = if (NROW(formacao)) formacao else tibble::tibble(),
    cooperativas_associacoes = if (is.null(associativas)) tibble::tibble() else associativas$lista,
    arranjo_municipios       = if (is.null(arranjo)) tibble::tibble() else arranjo$municipio %>% filter(!is.na(classe) | emprego > 0),
    arranjo_manchas          = if (is.null(arranjo)) tibble::tibble() else arranjo$manchas,
    camada_territorial       = camada_territorial,
    comex_cadeia             = comex_cad,
    producao_ibge_cadeia     = sidra_cad %>% filter(ano >= max(ano) - 10) %>% select(-any_of(c("valor_txt", "situacao"))),
    lacunas                  = lacunas
  )), format = "file"),
  tar_target(arq_modelo_cadeia, "site/_modelos/cadeia.qmd", format = "file"),
  tar_target(paginas_cadeias, gerar_paginas_cadeias(cad$meta, modelo = arq_modelo_cadeia), format = "file"),

  # ------------------------------------------------- raio-X dos municípios
  tar_target(raiox,
             montar_raiox(bq_municipios, rd, bq_pib_pop, bq_pop, rais_secao, bq_perfil_pe, bq_caged_pe,
                          comex_anos_completos, bq_sh4_nomes, sidra_bruto, locais_cadeia, sidra_cad,
                          comex_cad, cad$meta, krugman, ipca, locais_elo) %>% adicionar_ql_raiox(ql_mult, ql_vab) %>% adicionar_dominancia_raiox(dominancia)),
  tar_target(saida_raiox, exportar_raiox(raiox, malha, rd = rd), format = "file"),
  tar_target(territorio,
             montar_territorio(bq_municipios, rd, bq_pib_pop, bq_pop, rais_secao, cad_mun, cad$meta, empresas,
                               empresas_maiores, qualificacao, educacao, icms, agro_familiar, credito_rural, infraestrutura,
                               formacao, associativas, cad$elos, vocacao_cad, camada_territorial)),
  tar_target(bq_cnae_grupos, { bq_esquema; extrair_cnae_grupos(projeto) }),
  tar_target(setores_base, montar_setores(bq_rais_pe, bq_cnae, bq_cnae_grupos, rd)),
  tar_target(saida_territorio, exportar_territorio(territorio, setores_base), format = "file"),

  # ------------------------------------------------ histórico: um retrato por mês em historico/
  tar_target(retrato, montar_retrato(mes_ref, ultimos_anos, cad_mun, total_mun, cad$elos, caged_cad, estoque, comex_cad,
                                     comex_anos_completos, empresas, bq_cnpj_cadeias, bq_pib_pop, vocacao_cad)),
  tar_target(historico_arq, salvar_retrato(retrato), format = "file"),
  tar_target(historico, { historico_arq; ler_historico() })
)
