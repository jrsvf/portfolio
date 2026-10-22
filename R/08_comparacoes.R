# =============================================================================
# Comparações no tempo (janelas de 5 e 10 anos) e com o Nordeste
# =============================================================================

UFS_NORDESTE <- c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")

# Emprego de cada cadeia em cada UF (para comparar PE com os vizinhos)
cadeia_por_uf <- function(rais_br, elos) {
  rais_br %>%
    ligar_elos(elos, "cadeia_id") %>%
    group_by(ano, sigla_uf, cadeia_id) %>%
    summarise(emprego = sum(vinculos, na.rm = TRUE),
              estabelecimentos = sum(estabelecimentos, na.rm = TRUE), .groups = "drop")
}

total_por_uf <- function(rais_br) {
  rais_br %>% group_by(ano, sigla_uf) %>%
    summarise(emprego_total = sum(vinculos, na.rm = TRUE), .groups = "drop")
}

# Variação entre o último ano da série e cada corte (10, 5, 2 anos e "desde 2023").
# df: colunas `ano`, `valor` e as chaves em `por`. O último ano é o da própria
# série, então cada fonte usa o seu.
variacoes_janela <- function(df, por = character(), janelas = CORTES_PADRAO$janelas,
                             desde = CORTES_PADRAO$desde) {
  df <- df %>% filter(!is.na(valor))
  if (nrow(df) == 0) return(tibble())
  ult <- max(df$ano, na.rm = TRUE)
  cortes <- cortes_anos(ult, janelas, desde)
  b <- df %>% filter(ano == ult) %>% select(all_of(por), v_fim = valor)
  pmap_dfr(cortes, function(rotulo, ano_ini, ordem) {
    a <- df %>% filter(ano == ano_ini) %>% select(all_of(por), v_ini = valor)
    out <- if (length(por) == 0) bind_cols(a[1, , drop = FALSE], b[1, , drop = FALSE]) else full_join(a, b, by = por)
    out %>% mutate(rotulo = rotulo, ordem = ordem, janela = ult - ano_ini, ano_ini = ano_ini, ano_fim = ult,
                   var_abs = v_fim - v_ini,
                   var_pct = ifelse(!is.na(v_ini) & v_ini > 0, v_fim / v_ini - 1, NA_real_),
                   cagr = cagr(v_ini, v_fim, ult - ano_ini))
  })
}

# Comparação de cada cadeia de PE com o Nordeste
comparar_nordeste <- function(cad_uf, tot_uf, uf = "PE", janelas = CORTES_PADRAO$janelas, desde = CORTES_PADRAO$desde) {
  ult <- max(cad_uf$ano)
  ne <- cad_uf %>% filter(sigla_uf %in% UFS_NORDESTE)
  tot_ne <- tot_uf %>% filter(sigla_uf %in% UFS_NORDESTE) %>% group_by(ano) %>%
    summarise(total_ne = sum(emprego_total), .groups = "drop")
  tot_br <- tot_uf %>% group_by(ano) %>% summarise(total_br = sum(emprego_total), .groups = "drop")
  cad_br <- cad_uf %>% group_by(ano, cadeia_id) %>% summarise(emp_br = sum(emprego), .groups = "drop")

  # posição de PE entre os 9 estados no último ano
  ranking <- ne %>% filter(ano == ult) %>%
    group_by(cadeia_id) %>% arrange(desc(emprego), .by_group = TRUE) %>%
    mutate(posicao = row_number(), n_estados = n(), lider = first(sigla_uf), emp_lider = first(emprego)) %>%
    ungroup() %>% filter(sigla_uf == uf) %>%
    select(cadeia_id, posicao, n_estados, lider, emp_lider)

  base <- ne %>%
    group_by(ano, cadeia_id) %>%
    summarise(emp_ne = sum(emprego), emp_pe = sum(emprego[sigla_uf == uf]), .groups = "drop") %>%
    left_join(tot_ne, by = "ano") %>%
    left_join(tot_uf %>% filter(sigla_uf == uf) %>% select(ano, total_pe = emprego_total), by = "ano") %>%
    left_join(cad_br, by = c("ano", "cadeia_id")) %>% left_join(tot_br, by = "ano") %>%
    mutate(part_ne = div0(emp_pe, emp_ne),
           ql_ne = div0(div0(emp_pe, total_pe), div0(emp_ne, total_ne)),
           ql_br = div0(div0(emp_pe, total_pe), div0(emp_br, total_br)))

  var_pe <- variacoes_janela(base %>% transmute(ano, cadeia_id, valor = emp_pe), "cadeia_id", janelas, desde) %>%
    select(cadeia_id, rotulo, ordem, janela, ano_ini, ano_fim, var_pe = var_pct)
  var_ne <- variacoes_janela(base %>% transmute(ano, cadeia_id, valor = emp_ne - emp_pe), "cadeia_id", janelas, desde) %>%
    select(cadeia_id, rotulo, var_ne_sem_pe = var_pct)
  ql_ini <- base %>% select(ano, cadeia_id, ql_ne_ini = ql_ne)

  resumo <- base %>% filter(ano == ult) %>%
    select(cadeia_id, ano, emp_pe, emp_ne, part_ne, ql_ne, ql_br) %>%
    left_join(ranking, by = "cadeia_id")

  janelas_tbl <- var_pe %>% left_join(var_ne, by = c("cadeia_id", "rotulo")) %>%
    left_join(ql_ini, by = c("cadeia_id", "ano_ini" = "ano"))

  list(resumo = resumo, janelas = janelas_tbl,
       por_uf = ne %>% filter(ano >= ult - max(janelas)))
}

# Frases sobre a posição de PE no Nordeste
destaques_nordeste <- function(id, cmp, nomes_uf = NOMES_UF) {
  r <- cmp$resumo %>% filter(cadeia_id == id)
  if (nrow(r) == 0 || is.na(r$emp_pe) || r$emp_pe == 0) return(character())
  out <- character()
  if (!is.na(r$posicao)) {
    out <- c(out, if (r$posicao == 1)
      sprintf("Pernambuco é o maior empregador formal da cadeia no Nordeste, com %s do emprego da região.", fmt_pct(r$part_ne))
    else
      sprintf("Pernambuco é o %dº estado do Nordeste em emprego formal na cadeia (%s da região); o líder é %s.",
              r$posicao, fmt_pct(r$part_ne), nomes_uf[[r$lider]] %||% r$lider))
  }
  if (!is.na(r$ql_ne) && r$ql_ne > 0) {
    out <- c(out, if (r$ql_ne >= 1.2)
      sprintf("A cadeia pesa %s vezes mais na economia formal de PE do que na média do Nordeste.", fmt_num(r$ql_ne, 1))
    else if (r$ql_ne <= 0.8)
      sprintf("A cadeia pesa menos em PE do que na média do Nordeste (%s da média regional).", fmt_pct(r$ql_ne, 0))
    else "O peso da cadeia na economia formal de PE é parecido com a média do Nordeste.")
  }
  j <- cmp$janelas %>% filter(cadeia_id == id, ordem == 1)
  if (nrow(j) == 1 && !is.na(j$var_pe) && !is.na(j$var_ne_sem_pe)) {
    dif <- j$var_pe - j$var_ne_sem_pe
    if (abs(dif) >= 0.05) out <- c(out, sprintf(
      "Nos últimos %d anos o emprego da cadeia em PE variou %s, contra %s no restante do Nordeste: PE %s.",
      j$janela, sinal_pct(j$var_pe), sinal_pct(j$var_ne_sem_pe),
      ifelse(dif > 0, "ganhou espaço na região", "perdeu espaço para o restante da região")))
  }
  out
}

NOMES_UF <- list(MA = "Maranhão", PI = "Piauí", CE = "Ceará", RN = "Rio Grande do Norte", PB = "Paraíba",
                 PE = "Pernambuco", AL = "Alagoas", SE = "Sergipe", BA = "Bahia")

# Tabela de janelas (5 e 10 anos) com vários indicadores de uma cadeia
janelas_cadeia <- function(cad_mun, perfil_cad, comex_cad, sidra_cad, ipca, cmp,
                           janelas = CORTES_PADRAO$janelas, desde = CORTES_PADRAO$desde) {
  emp <- cad_mun %>% group_by(ano, cadeia_id) %>%
    summarise(emprego = sum(emprego), estab = sum(estabelecimentos), .groups = "drop")
  defl <- deflator(ipca)
  rem <- perfil_cad %>% left_join(defl, by = "ano") %>%
    transmute(ano, cadeia_id, valor = remuneracao_media * fator)
  exp <- comex_cad %>% filter(fluxo == "Exportação") %>% group_by(ano, cadeia_id) %>%
    summarise(valor = sum(valor_fob), .groups = "drop")
  prod <- sidra_cad %>% filter(str_detect(variavel, regex("^Valor da produção", ignore_case = TRUE))) %>%
    group_by(ano, cadeia_id) %>% summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    left_join(defl, by = "ano") %>% mutate(valor = valor * fator) %>% select(ano, cadeia_id, valor)
  vj <- function(d, nome) variacoes_janela(d, "cadeia_id", janelas, desde) %>% mutate(indicador = nome)

  bind_rows(
    vj(emp %>% transmute(ano, cadeia_id, valor = emprego), "Empregos formais (PE)"),
    vj(emp %>% transmute(ano, cadeia_id, valor = estab), "Estabelecimentos (PE)"),
    vj(rem, "Remuneração média real (PE)"),
    vj(prod, "Valor da produção real, IBGE (PE)"),
    vj(exp, "Exportações em US$ (PE)"),
    cmp$janelas %>% transmute(cadeia_id, rotulo, ordem, janela, ano_ini, ano_fim, var_pct = var_ne_sem_pe,
                              indicador = "Empregos formais (Nordeste sem PE)")
  ) %>% filter(!is.na(var_pct))
}

# Deflator: fator que leva valores de cada ano a preços do último ano (IPCA médio anual)
deflator <- function(ipca) {
  if (is.null(ipca) || nrow(ipca) == 0) return(tibble(ano = numeric(), fator = numeric()))
  ref <- ipca$indice[ipca$ano == max(ipca$ano)]
  ipca %>% transmute(ano, fator = ref / indice)
}

# IPCA: número-índice médio de cada ano (SIDRA tabela 1737, variável 2266)
extrair_ipca <- function() {
  raw <- sidra_baixar("https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2266/p/all")
  cab <- as.character(raw[1, ]); df <- tibble::as_tibble(raw[-1, , drop = FALSE]); names(df) <- cab
  col_mes <- grep("^Mês \\(Código\\)$", cab, value = TRUE)
  df %>% transmute(ano = as.numeric(substr(.data[[col_mes]], 1, 4)), indice = suppressWarnings(as.numeric(Valor))) %>%
    filter(!is.na(indice)) %>% group_by(ano) %>% filter(n() == 12) %>%
    summarise(indice = mean(indice), .groups = "drop")
}

# Fontes e períodos efetivamente usados nesta atualização
fontes_e_datas <- function(rais_pe, caged, comex, pib_pop, sidra, ipca, mip_ano) {
  caged_ult <- caged %>% filter(ano == max(ano)) %>% summarise(m = max(mes)) %>% pull(m)
  comex_ult <- max(comex$ano)
  tibble::tribble(
    ~fonte, ~orgao, ~cobertura, ~onde,
    "RAIS (vínculos e estabelecimentos)", "MTE", sprintf("%d a %d (anual)", min(rais_pe$ano), max(rais_pe$ano)), "Base dos Dados / BigQuery",
    "Novo CAGED", "MTE", sprintf("01/%d a %02d/%d (mensal)", min(caged$ano), caged_ult, max(caged$ano)), "Base dos Dados / BigQuery",
    "ComexStat por município", "MDIC", sprintf("%d a %d", min(comex$ano), comex_ult), "Base dos Dados / BigQuery",
    "PIB dos Municípios", "IBGE", sprintf("%d a %d", min(pib_pop$ano[!is.na(pib_pop$pib)]), max(pib_pop$ano[!is.na(pib_pop$pib)])), "Base dos Dados / BigQuery",
    "PAM, PPM e PEVS", "IBGE", if (nrow(sidra)) sprintf("%d a %d", min(sidra$ano), max(sidra$ano)) else "-", "API SIDRA",
    "IPCA (deflator)", "IBGE", if (!is.null(ipca) && nrow(ipca)) sprintf("%d a %d (médias anuais)", min(ipca$ano), max(ipca$ano)) else "-", "API SIDRA, tabela 1737",
    "Matriz insumo-produto", "IBGE", as.character(mip_ano), "IBGE, nível 67"
  ) %>% mutate(consultado_em = format(Sys.Date(), "%d/%m/%Y"))
}
