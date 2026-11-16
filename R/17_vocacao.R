# =============================================================================
# Vocações por cadeia: consolidadas, emergentes e potenciais
#
# Cada combinação de território (RD ou município) e cadeia é medida por quatro
# quocientes locacionais (emprego no núcleo produtivo, empresas, exportações e
# produção agropecuária), pelo peso no estado, pelo porte e pelo crescimento do
# emprego em dez anos, e classificada em um de três estágios:
#   Consolidada: já é especializada e tem escala;
#   Emergente:   cresce bem acima do estado, ainda sem ser especialização;
#   Potencial:   há produção, empresas ou exportação, sem emprego formal correspondente.
# O emprego usa só o núcleo produtivo (cad$elos$nucleo): comércio e distribuição
# existem em todo lugar e escondem a especialização.
# =============================================================================

# setor de cada cadeia para as cores dos mapas (nichos não têm macrossetor próprio)
VOC_SETOR_NICHO <- c(cafeicultura = "Agro", vitivinicultura = "Agro", cacau_chocolate = "Agro", floricultura = "Agro", carcinicultura = "Agro",
                     cachaca = "Agro", cajucultura = "Agro", coco = "Agro", cotonicultura = "Agro",
                     calcados = "Ind", ceramica_vermelha = "Ind", joias = "Ind", bioeconomia_renovaveis = "Ind",
                     economia_criativa = "Serv", economia_do_mar = "Serv", saude = "Serv")
VOC_NOME_SETOR <- c(Agro = "Agropecuária e agroindústria", Ind = "Indústria, mineração e energia", Serv = "Serviços, turismo e economia criativa")
# atividades que atendem sobretudo à demanda local: aparecem em quase toda região
VOC_BASE_LOCAL <- c("saude", "construcao_civil", "logistica")
# recortes que repetem parte de uma cadeia sem estar contidos nela (o etanol da bioeconomia já está no
# sucroenergético): na escolha da vocação líder do município, cedem a vez à cadeia
VOC_SOBREPOE <- c(bioeconomia_renovaveis = "sucroenergetico")
# cadeias sem valor de produção no IBGE: a medida de produção é o rebanho
VOC_REBANHO <- c("ovinocaprinocultura", "suinocultura")

VOC_LIMITES <- list(
  rd  = list(part_cheia = 0.30, emp_cons = 300, emp_emer = 150, ganho = 100, prod = 5000, part_prod = 0.10, firmas = 80, exp = 2e6,
             p_emp = 5000, p_prod = 5e5, p_exp = 1e8, p_reb = 1e6, emp_ancora = Inf, ql_prod = 1.2, reb = 0),
  mun = list(part_cheia = 0.10, emp_cons = 50, emp_emer = 50, ganho = 30, prod = 3000, part_prod = 0, firmas = 30, exp = 5e5,
             p_emp = 1500, p_prod = 1e5, p_exp = 2e7, p_reb = 2e5, emp_ancora = 200, ql_prod = 1.5, reb = 30000))

voc_setor <- function(meta) {
  s <- case_when(grepl("^Agro", meta$macrossetor) ~ "Agro", grepl("^Ind", meta$macrossetor) ~ "Ind", grepl("^Serv", meta$macrossetor) ~ "Serv",
                 TRUE ~ unname(VOC_SETOR_NICHO[meta$cadeia_id]))
  coalesce(s, "Serv")
}

montar_vocacao <- function(cad_mun, total_mun, elos, meta, rd, municipios, cnpj_cadeias, comex_cad, comex_pe, ql_prod, producao_terr,
                           sidra_cad, malha, anos_janela = 10) {
  ANO <- max(cad_mun$ano); ANO0 <- ANO - anos_janela
  rd <- rd %>% mutate(id_municipio = as.character(id_municipio)) %>% select(id_municipio, rd)
  meta <- meta %>% mutate(setor = voc_setor(meta), local = cadeia_id %in% VOC_BASE_LOCAL)
  nuc <- elos %>% distinct(cadeia_id, elo, nucleo)
  # cadeias com elo de comércio: a contagem de empresas é dominada por lojas e pesa menos
  com_comercio <- unique(nuc$cadeia_id[!nuc$nucleo & grepl("Comércio|Varejo|Distribuição", nuc$elo)])
  z <- function(q) coalesce(q, 0)

  # ---- emprego no núcleo produtivo, no ano final e no inicial
  cm <- cad_mun %>% mutate(id_municipio = as.character(id_municipio)) %>% inner_join(nuc %>% filter(nucleo), by = c("cadeia_id", "elo"))
  emp <- cm %>% filter(ano %in% c(ANO0, ANO)) %>% group_by(ano, id_municipio, cadeia_id) %>%
    summarise(e = sum(emprego), est = sum(estabelecimentos), .groups = "drop")
  emp_m <- full_join(emp %>% filter(ano == ANO) %>% select(id_municipio, cadeia_id, emp = e, estab = est),
                     emp %>% filter(ano == ANO0) %>% select(id_municipio, cadeia_id, emp0 = e), by = c("id_municipio", "cadeia_id"))
  tot <- total_mun %>% mutate(id_municipio = as.character(id_municipio))
  totm <- full_join(tot %>% filter(ano == ANO) %>% select(id_municipio, tot = emprego_total),
                    tot %>% filter(ano == ANO0) %>% select(id_municipio, tot0 = emprego_total), by = "id_municipio")

  # ---- empresas ativas (CNPJ, com MEI)
  cn <- if (NROW(cnpj_cadeias)) cnpj_cadeias %>% mutate(id_municipio = as.character(id_municipio)) else tibble(nivel = character(), cadeia_id = character(), id_municipio = character(), mei = logical(), n = numeric())
  fir_m <- cn %>% filter(nivel == "cadeia") %>% group_by(id_municipio, cadeia_id) %>% summarise(firmas = sum(n), mei = sum(n[mei]), .groups = "drop")
  firtot <- cn %>% filter(nivel == "total") %>% group_by(id_municipio) %>% summarise(firmas_tot = sum(n), .groups = "drop")

  # ---- exportações do último ano completo
  cx <- comex_cad %>% mutate(id_municipio = as.character(id_municipio)) %>% filter(fluxo == "Exportação")
  ANOX <- if (nrow(cx)) max(cx$ano) else NA
  exp_m <- cx %>% filter(ano == ANOX) %>% group_by(id_municipio, cadeia_id) %>% summarise(exp = sum(valor_fob, na.rm = TRUE), .groups = "drop")
  exptot <- comex_pe %>% mutate(id_municipio = as.character(id_municipio)) %>% filter(ano == ANOX, fluxo == "Exportação") %>%
    group_by(id_municipio) %>% summarise(exp_tot = sum(valor_fob, na.rm = TRUE), .groups = "drop")

  # ---- produção agropecuária (valor) e rebanhos
  prod_m <- if (NROW(ql_prod)) ql_prod %>% transmute(id_municipio = as.character(id_municipio), cadeia_id, prod = producao_mil) else tibble(id_municipio = character(), cadeia_id = character(), prod = numeric())
  prodtot <- if (!is.null(producao_terr)) producao_terr$municipios %>% mutate(id_municipio = as.character(id_municipio)) %>% group_by(id_municipio) %>%
    summarise(prod_tot = sum(valor_mil, na.rm = TRUE), .groups = "drop") else tibble(id_municipio = character(), prod_tot = numeric())
  reb_m <- if (NROW(sidra_cad)) sidra_cad %>% mutate(id_municipio = as.character(id_municipio)) %>%
    filter(cadeia_id %in% VOC_REBANHO, variavel == "Efetivo dos rebanhos") %>% filter(ano == max(ano)) %>%
    group_by(id_municipio, cadeia_id) %>% summarise(reb = sum(valor, na.rm = TRUE), .groups = "drop") else tibble(id_municipio = character(), cadeia_id = character(), reb = numeric())
  area <- tibble(id_municipio = as.character(malha$id_municipio), area = as.numeric(sf::st_area(malha)) / 1e6)

  medidas <- c("emp", "emp0", "estab", "firmas", "mei", "exp", "prod", "reb")
  base <- tidyr::expand_grid(id_municipio = rd$id_municipio, cadeia_id = meta$cadeia_id) %>% left_join(rd, by = "id_municipio") %>%
    left_join(emp_m, by = c("id_municipio", "cadeia_id")) %>% left_join(fir_m, by = c("id_municipio", "cadeia_id")) %>%
    left_join(exp_m, by = c("id_municipio", "cadeia_id")) %>% left_join(prod_m, by = c("id_municipio", "cadeia_id")) %>%
    left_join(reb_m, by = c("id_municipio", "cadeia_id")) %>% mutate(across(all_of(medidas), ~ coalesce(as.numeric(.x), 0)))
  totais <- rd %>% left_join(area, by = "id_municipio") %>% left_join(totm, by = "id_municipio") %>% left_join(firtot, by = "id_municipio") %>%
    left_join(exptot, by = "id_municipio") %>% left_join(prodtot, by = "id_municipio") %>%
    mutate(across(c(tot, tot0, firmas_tot, exp_tot, prod_tot, area), ~ coalesce(as.numeric(.x), 0)))
  pe_c <- base %>% group_by(cadeia_id) %>% summarise(across(all_of(medidas), sum), .groups = "drop") %>% rename_with(~ paste0(.x, "_pe"), -cadeia_id)
  PE <- totais %>% summarise(across(c(tot, tot0, firmas_tot, exp_tot, prod_tot, area), sum))
  G_PE <- PE$tot / PE$tot0 - 1

  ql <- function(x, xt, c, ct) ifelse(xt > 0 & c > 0 & ct > 0, (x / xt) / (c / ct), NA_real_)
  indicadores <- function(d, tt) {
    d %>% left_join(tt, by = intersect(names(d), names(tt))) %>% left_join(pe_c, by = "cadeia_id") %>%
      mutate(rebanho = reb_pe > 0, com_comercio = cadeia_id %in% com_comercio,
             ql_emp = ql(emp, tot, emp_pe, PE$tot), ql_fir = ql(firmas, firmas_tot, firmas_pe, PE$firmas_tot),
             ql_exp = ql(exp, exp_tot, exp_pe, PE$exp_tot),
             # rebanho: densidade relativa (cabeças por km2 sobre a do estado)
             ql_prod = ifelse(rebanho, ql(reb, area, reb_pe, PE$area), ql(prod, prod_tot, prod_pe, PE$prod_tot)),
             part_emp = ifelse(emp_pe > 0, emp / emp_pe, 0),
             part_prod = ifelse(rebanho, ifelse(reb_pe > 0, reb / reb_pe, 0), ifelse(prod_pe > 0, prod / prod_pe, 0)),
             g10 = ifelse(emp0 >= 30, emp / emp0 - 1, NA_real_), g10_pe = ifelse(emp0_pe > 0, emp_pe / emp0_pe - 1, NA_real_), d_emp = emp - emp0,
             ap_prod = prod_pe >= 5000 | rebanho, ap_exp = exp_pe >= 1e6)
  }
  classificar <- function(d, lim) {
    cap <- function(q) pmin(z(q), 3) / 3
    d %>% mutate(
      w_emp = .40, w_fir = ifelse(com_comercio, .10, .25), w_prod = ifelse(ap_prod, .20, 0), w_exp = ifelse(ap_exp, .15, 0),
      spec = (w_emp * cap(ql_emp) + w_fir * cap(ql_fir) + w_prod * cap(ql_prod) + w_exp * cap(ql_exp)) / (w_emp + w_fir + w_prod + w_exp),
      part = pmax(part_emp, part_prod, na.rm = TRUE), escala = pmin(1, part / lim$part_cheia),
      porte = pmax(pmin(1, emp / lim$p_emp), pmin(1, prod / lim$p_prod), pmin(1, exp / lim$p_exp), pmin(1, reb / lim$p_reb)),
      nova = emp0 < 30 & emp >= lim$emp_emer + 50,
      din = pmin(1, pmax(0, 0.5 + coalesce(g10, G_PE) - G_PE)),
      indice = round(100 * (0.40 * spec + 0.20 * escala + 0.20 * porte + 0.20 * din), 1),
      n_sinais = (z(ql_emp) >= 1) + (!com_comercio & z(ql_fir) >= 1) + (ap_prod & z(ql_prod) >= 1) + (ap_exp & z(ql_exp) >= 1),
      n_medidos = 1 + !com_comercio + ap_prod + ap_exp,
      consolidada = emp >= lim$emp_cons & (estab >= 3 | emp >= lim$emp_ancora) &
        (z(ql_emp) >= 1.5 | (z(ql_emp) >= 1.2 & n_sinais >= 2) | (z(ql_emp) >= 1.05 & part_emp >= 0.5)),
      emergente = !consolidada & emp >= lim$emp_emer & estab >= 3 & z(ql_emp) >= 0.7 &
        ((!is.na(g10) & g10 >= pmax(G_PE + 0.15, coalesce(g10_pe, 0) + 0.10) & d_emp >= lim$ganho) | nova),
      em_queda = !is.na(g10) & g10 <= -0.10,
      s_prod = ap_prod & z(ql_prod) >= lim$ql_prod & part_prod >= lim$part_prod & ((rebanho & reb >= lim$reb) | prod >= lim$prod),
      s_exp = ap_exp & z(ql_exp) >= 1 & exp >= lim$exp & emp >= 20,
      s_fir = !com_comercio & z(ql_fir) >= 1.2 & firmas >= lim$firmas,
      potencial = !consolidada & !emergente & !em_queda & (s_prod | s_exp | s_fir),
      classe = case_when(consolidada ~ "Consolidada", emergente ~ "Emergente", potencial ~ "Potencial", TRUE ~ NA_character_),
      tendencia = case_when(is.na(g10) ~ ifelse(nova, "nova", "sem base"), g10 >= 0.10 ~ "em alta", g10 <= -0.10 ~ "em queda", TRUE ~ "estável"),
      sinal_pot = case_when(!potencial ~ NA_character_, s_prod ~ "produção", s_exp ~ "exportações", TRUE ~ "empresas")) %>%
      select(-starts_with("w_"), -spec, -escala, -porte, -din, -consolidada, -emergente, -potencial, -em_queda, -starts_with("s_"), -nova)
  }

  info <- meta %>% select(cadeia_id, cadeia, grupo, setor, local)
  tot_rd <- totais %>% group_by(rd) %>% summarise(across(c(tot, tot0, firmas_tot, exp_tot, prod_tot, area), sum), .groups = "drop")
  rdc <- base %>% group_by(rd, cadeia_id) %>% summarise(across(all_of(medidas), sum), .groups = "drop") %>%
    indicadores(tot_rd) %>% classificar(VOC_LIMITES$rd) %>% left_join(info, by = "cadeia_id")
  munc <- base %>% indicadores(totais %>% select(-rd)) %>% classificar(VOC_LIMITES$mun) %>% left_join(info, by = "cadeia_id") %>%
    left_join(municipios %>% transmute(id_municipio = as.character(id_municipio), municipio), by = "id_municipio")

  polos <- munc %>% filter(emp > 0 | prod > 0 | reb > 0) %>% group_by(rd, cadeia_id) %>%
    summarise(polos_emp = paste(head(municipio[emp > 0][order(-emp[emp > 0])], 3), collapse = ", "),
              polos_prod = paste(head(municipio[(prod + reb) > 0][order(-(prod + reb)[(prod + reb) > 0])], 3), collapse = ", "),
              n_mun_cons = sum(classe %in% "Consolidada"), .groups = "drop")
  rdc <- rdc %>% left_join(polos, by = c("rd", "cadeia_id"))

  # vocação líder do município: a de maior peso econômico entre as classificadas (emprego no núcleo ou
  # produção em empregos equivalentes: R$ 100 mil ou 200 cabeças por emprego), ponderada pelo QL (teto 5)
  pai <- c(meta %>% filter(!is.na(contido_em)) %>% { setNames(.$contido_em, .$cadeia_id) }, VOC_SOBREPOE)
  lider <- munc %>% filter(!is.na(classe)) %>% group_by(id_municipio) %>%
    filter(!(cadeia_id %in% names(pai) & unname(pai[cadeia_id]) %in% cadeia_id)) %>% ungroup() %>%
    mutate(peso = pmax(emp * pmin(z(ql_emp), 5), (prod / 100 + reb / 200) * pmin(z(ql_prod), 5) * ap_prod)) %>%
    group_by(id_municipio) %>% arrange(desc(peso), desc(indice), .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
    select(id_municipio, municipio, rd, cadeia_id, cadeia, setor, classe, indice, emp, ql_emp, prod, reb, ql_prod, firmas, ql_fir, exp, g10, tendencia)

  list(ano = ANO, ano0 = ANO0, ano_exp = ANOX, g_pe = G_PE, rd = rdc, municipios = munc %>% filter(!is.na(classe) | emp >= 50), lider = lider,
       elos_fora = nuc %>% filter(!nucleo) %>% select(cadeia_id, elo), limites = VOC_LIMITES)
}

# Tabela enxuta para o site e para download
vocacao_tabela <- function(v) {
  if (is.null(v)) return(tibble())
  v$rd %>% filter(!is.na(classe)) %>%
    transmute(rd, cadeia_id, cadeia, grupo, setor = unname(VOC_NOME_SETOR[setor]), estagio = classe, tendencia, indice, emprego_nucleo = emp,
              variacao_10_anos = g10, empresas = firmas, exportacoes_usd = exp, producao_mil_reais = prod, rebanho_cabecas = reb,
              ql_emprego = ql_emp, ql_empresas = ql_fir, ql_exportacoes = ifelse(ap_exp, ql_exp, NA), ql_producao = ifelse(ap_prod, ql_prod, NA),
              part_na_cadeia_pe = part, base_local = local, principais_municipios = ifelse(classe == "Potencial" & polos_prod != "", polos_prod, polos_emp)) %>%
    arrange(rd, match(estagio, c("Consolidada", "Emergente", "Potencial")), base_local, desc(indice))
}
