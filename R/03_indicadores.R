# =============================================================================
# Indicadores de especialização, concentração e dinâmica
# =============================================================================
# Notação: i = município (ou região), j = atividade, E = emprego formal.
#
#   QL_ij  = (E_ij / E_i.) / (E_.j / E_..)         Quociente locacional
#   PR_ij  = E_ij / E_.j                            Participação relativa do
#                                                   município no emprego estadual
#                                                   da atividade (Suzigan, 2006)
#   PL_ij  = E_ij / E_i.                            Peso da atividade no
#                                                   emprego do município
#   IHHm_ij = E_ij/E_.j - E_i./E_..                 IHH modificado (Suzigan, 2006)
#   HHI_j  = soma_i (E_ij/E_.j)^2                   Concentração espacial
#   K_i    = soma_j |E_ij/E_i. - E_.j/E_..|         Índice de Krugman
#
# Todas as funções recebem tabelas "longas" e são puras (testadas em tests/).
# =============================================================================

#' Indicadores locais para cada (ano, municipio, atividade)
#' @param df colunas: ano, id_municipio, atividade, emprego
#' @param total_mun emprego total de cada município (ano, id_municipio, emprego_total).
#'   Se NULL, usa a soma das atividades presentes em df.
indicadores_locais <- function(df, total_mun = NULL) {
  stopifnot(all(c("ano", "id_municipio", "atividade", "emprego") %in% names(df)))
  if (is.null(total_mun)) {
    total_mun <- df %>% group_by(ano, id_municipio) %>%
      summarise(emprego_total = sum(emprego, na.rm = TRUE), .groups = "drop")
  }
  tot_estado <- total_mun %>% group_by(ano) %>%
    summarise(emprego_estado = sum(emprego_total, na.rm = TRUE), .groups = "drop")
  tot_ativ <- df %>% group_by(ano, atividade) %>%
    summarise(emprego_ativ_estado = sum(emprego, na.rm = TRUE), .groups = "drop")

  df %>%
    left_join(total_mun, by = c("ano", "id_municipio")) %>%
    left_join(tot_estado, by = "ano") %>%
    left_join(tot_ativ, by = c("ano", "atividade")) %>%
    mutate(
      pl   = div0(emprego, emprego_total),
      pr   = div0(emprego, emprego_ativ_estado),
      ql   = div0(pl, emprego_ativ_estado / emprego_estado),
      ihhm = pr - div0(emprego_total, emprego_estado)
    )
}

#' Concentração espacial (HHI) e número de municípios especializados
concentracao_espacial <- function(locais, ql_minimo = 1, emprego_minimo = 10) {
  locais %>%
    group_by(ano, atividade) %>%
    summarise(
      emprego         = sum(emprego, na.rm = TRUE),
      hhi             = sum(pr^2, na.rm = TRUE),
      n_municipios    = sum(emprego > 0, na.rm = TRUE),
      n_especializados = sum(ql >= ql_minimo & emprego >= emprego_minimo, na.rm = TRUE),
      top3_share      = sum(sort(pr, decreasing = TRUE)[1:min(3, n())], na.rm = TRUE),
      .groups = "drop"
    )
}

#' Índice de especialização de Krugman por município
indice_krugman <- function(df_mun_setor) {
  # df: ano, id_municipio, setor, emprego (todos os setores do município)
  tot_mun <- df_mun_setor %>% group_by(ano, id_municipio) %>%
    mutate(s_i = div0(emprego, sum(emprego))) %>% ungroup()
  ref <- df_mun_setor %>% group_by(ano, setor) %>%
    summarise(e = sum(emprego), .groups = "drop") %>%
    group_by(ano) %>% mutate(s_ref = e / sum(e)) %>% ungroup() %>% select(-e)
  # completa setores ausentes no município com zero
  tot_mun %>%
    select(ano, id_municipio, setor, s_i) %>%
    complete(nesting(ano, id_municipio), setor = unique(ref$setor), fill = list(s_i = 0)) %>%
    inner_join(ref, by = c("ano", "setor")) %>%
    group_by(ano, id_municipio) %>%
    summarise(krugman = sum(abs(s_i - s_ref)), .groups = "drop")
}

#' Shift-share clássico (Dunn): região r contra referência n
#' @param reg  emprego da região por atividade: atividade, e0, e1
#' @param ref  emprego da referência por atividade: atividade, e0, e1
#' @return por atividade: variação real, efeito nacional, estrutural, diferencial
shift_share <- function(reg, ref) {
  g_n <- sum(ref$e1) / sum(ref$e0) - 1
  ref <- ref %>% mutate(g_j = div0(e1, e0) - 1)
  reg %>%
    inner_join(ref %>% select(atividade, g_j), by = "atividade") %>%
    mutate(
      g_rj        = div0(e1, e0) - 1,
      variacao    = e1 - e0,
      ef_nacional = e0 * g_n,
      ef_estrutural = e0 * (g_j - g_n),
      ef_diferencial = e0 * (g_rj - g_j)
    ) %>%
    select(atividade, e0, e1, variacao, ef_nacional, ef_estrutural, ef_diferencial)
}

#' Taxa de crescimento anual composta
cagr <- function(v_ini, v_fim, anos) {
  ifelse(v_ini > 0 & v_fim >= 0 & anos > 0, (v_fim / v_ini)^(1 / anos) - 1, NA_real_)
}

#' Divisão que devolve 0 quando o denominador é 0 ou NA (evita Inf/NaN em mapas)
div0 <- function(a, b) {
  r <- a / b                          # recicla a e b no tamanho maior
  r[is.na(b + 0 * a) | (b + 0 * a) == 0] <- 0
  r
}

# ---------------------------------------------------------------------------
# Agregações por cadeia e elo
# ---------------------------------------------------------------------------

#' Emprego por (ano, município, cadeia, elo) a partir da RAIS por classe
agregar_cadeias <- function(rais, elos) {
  rais %>%
    ligar_elos(elos) %>%
    group_by(ano, id_municipio, cadeia_id, elo_ordem, elo) %>%
    summarise(emprego = sum(vinculos, na.rm = TRUE),
              estabelecimentos = sum(estabelecimentos, na.rm = TRUE),
              .groups = "drop")
}

#' Série estadual da cadeia contra Brasil e Nordeste (índice ano-base = 100)
serie_comparada <- function(rais_br, elos, uf = "PE") {
  ne <- c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")
  rais_br %>%
    ligar_elos(elos, "cadeia_id") %>%
    mutate(recorte = case_when(sigla_uf == uf ~ "Pernambuco",
                               sigla_uf %in% ne ~ "Nordeste (sem PE)",
                               TRUE ~ "Resto do Brasil")) %>%
    group_by(ano, cadeia_id, recorte) %>%
    summarise(emprego = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
    group_by(cadeia_id, recorte) %>%
    arrange(ano, .by_group = TRUE) %>%
    mutate(indice = 100 * div0(emprego, first(emprego))) %>%
    ungroup() %>%
    group_by(ano, cadeia_id) %>%
    mutate(part_brasil = emprego / sum(emprego)) %>%
    ungroup()
}

#' Saldo do CAGED em 12 meses móveis por cadeia
caged_cadeia <- function(caged, elos) {
  out <- caged %>%
    ligar_elos(elos, "cadeia_id") %>%
    group_by(ano, mes, cadeia_id) %>%
    summarise(salario_admissao = weighted.mean(salario_admissao, admissoes, na.rm = TRUE),
              saldo = sum(saldo, na.rm = TRUE),
              admissoes = sum(admissoes, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)))
  if (nrow(out) == 0) return(out)
  # meses sem movimentação entram com saldo zero (a soma móvel exige meses contínuos)
  meses <- seq(min(out$data), max(out$data), by = "month")
  tidyr::expand_grid(cadeia_id = unique(out$cadeia_id), data = meses) %>%
    left_join(out %>% select(-ano, -mes), by = c("cadeia_id", "data")) %>%
    mutate(saldo = coalesce(saldo, 0), admissoes = coalesce(admissoes, 0)) %>%
    mutate(ano = as.numeric(format(data, "%Y")), mes = as.numeric(format(data, "%m"))) %>%
    group_by(cadeia_id) %>%
    arrange(data, .by_group = TRUE) %>%
    mutate(saldo_12m = as.numeric(stats::filter(saldo, rep(1, 12), sides = 1))) %>%
    ungroup()
}

#' Comércio exterior por cadeia
comex_cadeia <- function(comex, sh4) {
  comex %>% inner_join(sh4, by = "sh4", relationship = "many-to-many")
}
