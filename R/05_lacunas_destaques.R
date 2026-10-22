# =============================================================================
# Lacunas de dados e destaques automáticos
# =============================================================================
# Lacunas: onde os dados públicos não conseguem enxergar a cadeia, e qual
# informação faltaria para enxergar. As regras são deliberadamente simples e
# transparentes; cada lacuna diz qual fonte a resolveria.
#
# Destaques: frases geradas a partir dos números (peculiaridades que já se
# consegue identificar só com dados públicos).
# =============================================================================

# ---------------------------------------------------------------------------
# Lacunas automáticas
# ---------------------------------------------------------------------------
detectar_lacunas <- function(cad_mun, sidra_cad, comex_cad, cad_meta, elos,
                             sidra_bruto, ultimos_anos, ano_atual = as.numeric(format(Sys.Date(), "%Y"))) {
  ult <- max(cad_mun$ano)
  mun_ult <- cad_mun %>% filter(ano == ult)

  # 1. Produção relevante sem emprego formal no elo primário
  primario <- elos %>%
    filter(substr(classe, 1, 2) %in% c("01", "02", "03")) %>%
    distinct(cadeia_id, elo)
  emp_prim <- mun_ult %>% semi_join(primario, by = c("cadeia_id", "elo")) %>%
    group_by(cadeia_id, id_municipio) %>% summarise(emprego = sum(emprego), .groups = "drop")
  l1 <- tibble()
  if (nrow(sidra_cad) > 0) {
    prod <- sidra_cad %>%
      filter(str_detect(variavel, regex("^Valor da produção", ignore_case = TRUE))) %>%
      group_by(cadeia_id) %>% filter(ano == max(ano)) %>%
      group_by(cadeia_id, ano, id_municipio) %>% summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      group_by(cadeia_id) %>% mutate(part = div0(valor, sum(valor))) %>% ungroup() %>%
      filter(part >= 0.01)
    l1 <- prod %>%
      left_join(emp_prim, by = c("cadeia_id", "id_municipio")) %>%
      mutate(emprego = coalesce(emprego, 0)) %>%
      filter(emprego < 5) %>%
      transmute(tipo = "Produção sem emprego formal",
                cadeia_id, id_municipio,
                detalhe = sprintf("Responde por %s do valor da produção estadual (IBGE, %d) e tem %d vínculos formais no elo primário.",
                                  fmt_pct(part), ano, as.integer(emprego)),
                falta = "Dados de ocupação informal por município e atividade (só o Censo Demográfico e o Censo Agropecuário trazem, a cada 10 anos).")
  }

  # 2. Exportação registrada em município sem emprego formal na cadeia
  l2 <- tibble()
  if (nrow(comex_cad) > 0) {
    ult_cx <- max(comex_cad$ano)
    exp <- comex_cad %>% filter(ano == ult_cx, fluxo == "Exportação") %>%
      group_by(cadeia_id, id_municipio) %>% summarise(valor = sum(valor_fob), .groups = "drop") %>%
      filter(valor >= 1e5)
    emp_tot <- mun_ult %>% group_by(cadeia_id, id_municipio) %>% summarise(emprego = sum(emprego), .groups = "drop")
    l2 <- exp %>% left_join(emp_tot, by = c("cadeia_id", "id_municipio")) %>%
      mutate(emprego = coalesce(emprego, 0)) %>%
      filter(emprego == 0) %>%
      transmute(tipo = "Exportação sem base produtiva formal",
                cadeia_id, id_municipio,
                detalhe = sprintf("Exportou %s em %d, mas não tem vínculo formal nos elos da cadeia.", fmt_usd(valor), ult_cx),
                falta = "Origem produtiva das exportações: o ComexStat registra o domicílio fiscal da empresa, não onde o produto foi feito.")
  }

  # 3. Elos sem presença formal relevante no estado
  l3 <- mun_ult %>% group_by(cadeia_id, elo) %>% summarise(emprego = sum(emprego), .groups = "drop") %>%
    right_join(elos %>% distinct(cadeia_id, elo), by = c("cadeia_id", "elo")) %>%
    mutate(emprego = coalesce(emprego, 0)) %>%
    filter(emprego < 50) %>%
    transmute(tipo = "Elo ausente ou invisível", cadeia_id, id_municipio = NA_character_,
              detalhe = sprintf("O elo \"%s\" tem apenas %d vínculos formais no estado (%d).", elo, as.integer(emprego), ult),
              falta = "Saber se o elo está fora do estado (compra de insumos/serviços de outras UFs) ou na informalidade. Exigiria pesquisa de compras das empresas ou notas fiscais (SEFAZ).")

  # 4. Valores omitidos ou indisponíveis no SIDRA
  l4 <- tibble()
  if (nrow(sidra_bruto) > 0) {
    l4 <- sidra_bruto %>% filter(situacao %in% c("omitido (sigilo)", "não disponível")) %>%
      count(tabela, situacao, name = "n") %>%
      transmute(tipo = "Valor omitido ou indisponível no IBGE", cadeia_id = NA_character_, id_municipio = NA_character_,
                detalhe = sprintf("Tabela %d: %d valores municipais marcados como %s.", tabela, n, situacao),
                falta = "Valores identificados pelo IBGE como sigilosos ou não coletados.")
  }

  # 5. Ressalvas de classificação declaradas em cadeias.yml
  l5 <- cad_meta %>% filter(nchar(nota) > 0) %>%
    transmute(tipo = "Classificação ampla ou informalidade conhecida", cadeia_id, id_municipio = NA_character_,
              detalhe = nota,
              falta = "Recorte por subclasse CNAE, produto (PIA-Produto) ou pesquisa de campo.")

  # 6. Defasagem das fontes
  l6 <- ultimos_anos %>%
    mutate(atraso = ano_atual - ultimo_ano) %>%
    filter(atraso >= 1) %>%
    transmute(tipo = "Defasagem da fonte", cadeia_id = NA_character_, id_municipio = NA_character_,
              detalhe = sprintf("%s: último ano disponível é %d (%d ano(s) de atraso).", fonte, as.integer(ultimo_ano), as.integer(atraso)),
              falta = "Indicadores de alta frequência por município (o Novo CAGED cobre só o emprego formal e começa em 2020).")

  bind_rows(l1, l2, l3, l4, l5, l6)
}

# Lacunas estruturais (conhecidas, não dependem dos dados do mês)
LACUNAS_ESTRUTURAIS <- tibble::tribble(
  ~lacuna, ~consequencia, ~fonte_que_resolveria,
  "Pernambuco não tem matriz insumo-produto oficial",
  "Os encadeamentos são estimados a partir da matriz nacional (FLQ), com tecnologia média do Brasil.",
  "Matriz insumo-produto estadual (IBGE/Condepe-Fidem) ou inter-regional (NEREUS/USP).",
  "Não há produção industrial por município",
  "A indústria municipal só é vista pelo emprego formal; não há valor da produção ou produtividade local.",
  "PIA-Produto e PIA-Empresa com recorte municipal (hoje sob sigilo).",
  "Informalidade não é medida por município e atividade",
  "Polos como o de confecções do Agreste e a ovinocaprinocultura aparecem muito menores do que são.",
  "Censo Demográfico 2022 (microdados de trabalho por município) e pesquisas de campo.",
  "Exportações registradas pelo domicílio fiscal",
  "Empresas com sede fora do município (ou fora do estado) deslocam o registro da exportação.",
  "ComexStat por estabelecimento produtor ou cruzamento com notas fiscais.",
  "Comércio entre estados não é publicado",
  "Não se sabe quanto de cada cadeia é comprado ou vendido para outras UFs.",
  "Notas fiscais eletrônicas agregadas (SEFAZ-PE / CONFAZ).",
  "Serviços exportados não aparecem no ComexStat",
  "Cadeias como TI e audiovisual não têm medida pública de exportação.",
  "Estatísticas de comércio exterior de serviços (Siscoserv foi descontinuado em 2020).",
  "Pesca sem estatística municipal desde 2011",
  "Só a aquicultura é acompanhada pela PPM.",
  "Retomada do Boletim Estatístico da Pesca por município."
)

# ---------------------------------------------------------------------------
# Destaques automáticos por cadeia
# ---------------------------------------------------------------------------
gerar_destaques <- function(id, cad_mun, municipios, serie_comp, sidra_cad, comex_cad,
                            perfil_cad, perfil_estado, ql_minimo = 1, emprego_minimo = 10) {
  out <- character()
  d <- cad_mun %>% filter(cadeia_id == id)
  if (nrow(d) == 0) return("Sem vínculos formais registrados na RAIS para esta cadeia.")
  ult <- max(d$ano)
  ini <- max(min(d$ano), ult - max(CORTES_PADRAO$janelas))   # referência de longo prazo: 10 anos

  tot <- d %>% group_by(ano) %>% summarise(e = sum(emprego), .groups = "drop")
  e_ult <- tot$e[tot$ano == ult]; e_ini <- tot$e[tot$ano == ini]
  # trajetória em todos os cortes (10, 5, 2 anos e desde 2023)
  cortes <- cortes_anos(ult) %>%
    mutate(v = map_dbl(ano_ini, ~ { x <- tot$e[tot$ano == .x]; if (length(x) && x > 0) e_ult / x - 1 else NA_real_ })) %>%
    filter(!is.na(v))
  if (nrow(cortes) > 0) {
    partes <- ifelse(grepl("^Desde", cortes$rotulo),
                     paste(sinal_pct(cortes$v), tolower(cortes$rotulo)),
                     paste(sinal_pct(cortes$v), "em", cortes$rotulo))
    out <- c(out, sprintf("Emprego formal: %s vínculos em %d; %s.", fmt_num(e_ult), ult,
                          paste(partes, collapse = ", ")))
  }

  # concentração territorial
  mun <- d %>% filter(ano == ult) %>% group_by(id_municipio) %>%
    summarise(e = sum(emprego), .groups = "drop") %>% arrange(desc(e)) %>%
    left_join(municipios, by = "id_municipio")
  if (nrow(mun) >= 3 && e_ult > 0) {
    top3 <- sum(head(mun$e, 3)) / e_ult
    out <- c(out, sprintf("%s do emprego formal está em três municípios: %s.",
                          fmt_pct(top3), paste(head(mun$municipio, 3), collapse = ", ")))
  }

  # elo mais e menos dinâmico
  elos <- d %>% filter(ano %in% c(ini, ult)) %>% group_by(elo, ano) %>%
    summarise(e = sum(emprego), .groups = "drop") %>%
    pivot_wider(names_from = ano, values_from = e, names_prefix = "a") %>%
    mutate(var = div0(.data[[paste0("a", ult)]], .data[[paste0("a", ini)]]) - 1) %>%
    filter(.data[[paste0("a", ini)]] >= 50)
  if (nrow(elos) >= 2) {
    mx <- elos[which.max(elos$var), ]; mn <- elos[which.min(elos$var), ]
    out <- c(out, sprintf("Em %d anos, o elo que mais cresceu foi \"%s\" (%s); o de pior desempenho foi \"%s\" (%s).",
                          ult - ini, mx$elo, sinal_pct(mx$var), mn$elo, sinal_pct(mn$var)))
  }

  # peso de PE no Brasil
  sc <- serie_comp %>% filter(cadeia_id == id, recorte == "Pernambuco")
  if (nrow(sc) > 1) {
    a0 <- max(min(sc$ano), max(sc$ano) - max(CORTES_PADRAO$janelas))
    p_ini <- sc$part_brasil[sc$ano == a0]; p_ult <- sc$part_brasil[sc$ano == max(sc$ano)]
    out <- c(out, sprintf("Pernambuco responde por %s do emprego formal da cadeia no Brasil (%s em %d).",
                          fmt_pct(p_ult, 2), fmt_pct(p_ini, 2), a0))
  }

  # salário relativo
  pf <- perfil_cad %>% filter(cadeia_id == id, ano == max(ano))
  if (nrow(pf) > 0 && !is.null(perfil_estado)) {
    ref <- perfil_estado$remuneracao_media[perfil_estado$ano == max(pf$ano)]
    if (length(ref) == 1 && ref > 0) {
      rel <- pf$remuneracao_media[1] / ref - 1
      out <- c(out, sprintf("A remuneração média formal da cadeia é %s %s que a média estadual.",
                            fmt_pct(abs(rel)), ifelse(rel >= 0, "maior", "menor")))
    }
  }

  # produção x emprego (descolamento)
  if (nrow(sidra_cad) > 0) {
    s <- sidra_cad %>% filter(cadeia_id == id, str_detect(variavel, regex("^Valor da produção", ignore_case = TRUE)))
    if (nrow(s) > 0) {
      anos_c <- intersect(c(max(min(s$ano), max(s$ano) - max(CORTES_PADRAO$janelas)), max(s$ano)), tot$ano)
      if (length(anos_c) == 2 && diff(anos_c) > 0) {
        vp <- s %>% filter(ano %in% anos_c) %>% group_by(ano) %>% summarise(v = sum(valor, na.rm = TRUE))
        # valor nominal: compara apenas a direção com o emprego
        g_e <- tot$e[tot$ano == anos_c[2]] / tot$e[tot$ano == anos_c[1]] - 1
        if (nrow(vp) == 2 && vp$v[1] > 0 && g_e < 0) {
          out <- c(out, sprintf("Entre %d e %d o valor da produção (IBGE, nominal) cresceu %s enquanto o emprego formal caiu: sinal de mecanização, ganho de produtividade ou informalização.",
                                anos_c[1], anos_c[2], fmt_pct(vp$v[2] / vp$v[1] - 1)))
        }
      }
    }
  }

  # exportação
  if (nrow(comex_cad) > 0) {
    cx <- comex_cad %>% filter(cadeia_id == id, fluxo == "Exportação")
    if (nrow(cx) > 0) {
      u <- max(cx$ano)
      v <- sum(cx$valor_fob[cx$ano == u])
      dest <- cx %>% filter(ano == u) %>% group_by(pais) %>% summarise(v = sum(valor_fob), .groups = "drop") %>% arrange(desc(v))
      if (v > 0) out <- c(out, sprintf("Exportou %s em %d; principal destino: %s (%s do total).",
                                       fmt_usd(v), u, dest$pais[1], fmt_pct(dest$v[1] / v)))
    }
  }
  out
}

sinal_pct <- function(x) paste0(ifelse(x >= 0, "+", "-"), fmt_pct(abs(x)))
