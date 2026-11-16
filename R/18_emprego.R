# =============================================================================
# Estoque de emprego formal (estimativa mensal) e CAGED por elo
#
# A RAIS dá o estoque de vínculos em 31/12, uma vez por ano. O Novo CAGED dá o
# saldo (admissões menos desligamentos) de cada mês, só para empregos com
# carteira assinada (CLT). Juntando os dois se chega a um estoque mês a mês:
#
#   estoque(mês) = estoque da RAIS em dezembro do ano-base
#                  + saldos do CAGED depois de dezembro do ano-base
#                  - saldos do CAGED entre o mês e dezembro do ano-base
#
# O ano-base é o último ano da RAIS que o CAGED cobre até dezembro. Como o
# CAGED só acompanha celetistas, o ponto de partida é o estoque de celetistas
# da RAIS (`rais_clt`). Se essa consulta não estiver disponível, usa-se o total
# de vínculos fora da administração pública (divisão 84 da CNAE), que é onde
# estão quase todos os estatutários.
#
# É uma estimativa: o saldo do CAGED usado aqui é o declarado no prazo, sem o
# ajuste que o MTE faz depois com as declarações atrasadas, e a RAIS e o CAGED
# nem sempre fecham entre si. Não substitui o estoque oficial do MTE.
# =============================================================================

# Celetistas em 31/12 por município e subclasse (RAIS estabelecimentos)
extrair_rais_clt_uf <- function(projeto, uf, ano_ini) {
  sql <- glue::glue("
    SELECT ano, id_municipio, {SQL_SUBCLASSE} AS subclasse,
           SUM(SAFE_CAST(quantidade_vinculos_clt AS INT64)) AS vinculos_clt
    FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
    WHERE sigla_uf = '{uf}' AND ano >= {ano_ini}
    GROUP BY 1, 2, 3")
  consultar(sql, projeto) %>%
    mutate(across(c(ano, vinculos_clt), as.numeric), id_municipio = as.character(id_municipio)) %>% com_classe()
}

# Série mensal de estoque a partir de um estoque em dezembro do ano-base (`e0`)
# e dos saldos mensais. `saldos` tem as colunas de grupo, `data` e `saldo`.
.serie_estoque <- function(saldos, e0, grupos, meses, ano_base) {
  dez <- as.Date(sprintf("%d-12-01", ano_base))
  chaves <- bind_rows(saldos %>% distinct(across(all_of(grupos))), e0 %>% distinct(across(all_of(grupos)))) %>% distinct()
  tidyr::expand_grid(chaves, data = meses) %>%
    left_join(saldos, by = c(grupos, "data")) %>% left_join(e0, by = grupos) %>%
    mutate(saldo = coalesce(saldo, 0), e0 = coalesce(e0, 0)) %>%
    group_by(across(all_of(grupos))) %>% arrange(data, .by_group = TRUE) %>%
    mutate(acum = cumsum(saldo),
           # acum em dezembro do ano-base: tudo o que entrou até ali
           estoque = e0 + acum - acum[data == dez][1],
           saldo_12m = as.numeric(stats::filter(saldo, rep(1, 12), sides = 1)),
           var_12m = estoque / lag(estoque, 12) - 1) %>%
    ungroup() %>% select(-acum, -e0)
}

estoque_emprego <- function(rais_pe, caged_pe, elos, cnae, rais_clt = NULL) {
  if (!NROW(caged_pe) || !NROW(rais_pe)) return(NULL)
  if (!"nucleo" %in% names(elos)) elos$nucleo <- TRUE
  cg <- caged_pe %>% mutate(id_municipio = as.character(id_municipio), data = as.Date(sprintf("%d-%02d-01", ano, mes)))
  meses <- seq(min(cg$data), max(cg$data), by = "month")
  # ano-base: último ano da RAIS com dezembro coberto pelo CAGED
  anos_ok <- intersect(unique(rais_pe$ano), unique(cg$ano[cg$mes == 12]))
  if (!length(anos_ok)) return(NULL)
  ano_base <- max(anos_ok)

  clt_ok <- NROW(rais_clt) > 0 && ano_base %in% rais_clt$ano && sum(rais_clt$vinculos_clt[rais_clt$ano == ano_base], na.rm = TRUE) > 0
  base <- if (clt_ok) {
    rais_clt %>% filter(ano == ano_base) %>% transmute(id_municipio = as.character(id_municipio), subclasse, classe, e0 = coalesce(vinculos_clt, 0))
  } else {
    rais_pe %>% filter(ano == ano_base, substr(classe, 1, 2) != "84") %>%
      transmute(id_municipio = as.character(id_municipio), subclasse, classe, e0 = coalesce(vinculos, 0))
  }
  # sem a consulta de celetistas, o CAGED da administração pública também sai, para comparar igual com igual
  if (!clt_ok) cg <- cg %>% filter(substr(classe, 1, 2) != "84")
  secoes <- cnae %>% distinct(classe, secao, descricao_secao) %>% distinct(classe, .keep_all = TRUE)

  # ---- Pernambuco
  pe <- .serie_estoque(cg %>% group_by(data) %>% summarise(saldo = sum(saldo, na.rm = TRUE), admissoes = sum(admissoes, na.rm = TRUE),
                                                             desligamentos = sum(desligamentos, na.rm = TRUE), .groups = "drop") %>% mutate(uf = "PE"),
                       tibble(uf = "PE", e0 = sum(base$e0)), "uf", meses, ano_base) %>% select(-uf)

  # ---- seções da CNAE
  sec <- .serie_estoque(cg %>% left_join(secoes, by = "classe") %>% filter(!is.na(secao)) %>% group_by(secao, data) %>% summarise(saldo = sum(saldo, na.rm = TRUE), .groups = "drop"),
                        base %>% left_join(secoes, by = "classe") %>% filter(!is.na(secao)) %>% group_by(secao) %>% summarise(e0 = sum(e0), .groups = "drop"),
                        "secao", meses, ano_base) %>%
    left_join(secoes %>% distinct(secao, descricao_secao), by = "secao")

  # ---- municípios (só o último mês: estoque, saldo e variação em 12 meses)
  mun <- .serie_estoque(cg %>% group_by(id_municipio, data) %>% summarise(saldo = sum(saldo, na.rm = TRUE), .groups = "drop"),
                        base %>% group_by(id_municipio) %>% summarise(e0 = sum(e0), .groups = "drop"), "id_municipio", meses, ano_base) %>%
    filter(data == max(data)) %>% mutate(estoque = pmax(estoque, 0)) %>% select(id_municipio, data, estoque, saldo_12m, var_12m)

  # ---- cadeias: tudo e só o núcleo produtivo
  lig <- c("cadeia_id", "nucleo")
  cg_c <- cg %>% ligar_elos(elos, lig) %>% group_by(cadeia_id, nucleo, data) %>% summarise(saldo = sum(saldo, na.rm = TRUE), .groups = "drop")
  e0_c <- base %>% ligar_elos(elos, lig) %>% group_by(cadeia_id, nucleo) %>% summarise(e0 = sum(e0), .groups = "drop")
  por_nucleo <- .serie_estoque(cg_c, e0_c, lig, meses, ano_base)
  cadeia <- por_nucleo %>% group_by(cadeia_id, data) %>%
    summarise(estoque_nucleo = sum(estoque[nucleo]), estoque = sum(estoque), saldo_nucleo = sum(saldo[nucleo]), saldo = sum(saldo), .groups = "drop") %>%
    group_by(cadeia_id) %>% arrange(data, .by_group = TRUE) %>%
    mutate(var_12m = estoque / lag(estoque, 12) - 1, var_12m_nucleo = estoque_nucleo / lag(estoque_nucleo, 12) - 1) %>% ungroup() %>%
    mutate(across(c(estoque, estoque_nucleo), ~ pmax(.x, 0)))

  list(ano_base = ano_base, ate = max(meses), metodo = if (clt_ok) "celetistas" else "sem_adm_publica",
       pe = pe, secao = sec, municipio = mun, cadeia = cadeia)
}

# Rótulo do ponto de partida, para as notas das páginas
estoque_nota <- function(est) {
  if (is.null(est)) return("")
  partida <- if (identical(est$metodo, "celetistas")) paste0("celetistas da RAIS em dezembro de ", est$ano_base)
             else paste0("vínculos da RAIS em dezembro de ", est$ano_base, ", sem a administração pública")
  paste0("Estimativa: ", partida, ", mais os saldos mensais do Novo CAGED (declarados no prazo, sem ajuste)")
}

# Novo CAGED dos últimos 12 meses por elo de cada cadeia
caged_elos <- function(caged, elos) {
  if (!NROW(caged)) return(tibble())
  if (!"nucleo" %in% names(elos)) elos$nucleo <- TRUE
  cg <- caged %>% mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)))
  ate <- max(cg$data); desde <- seq(ate, by = "-11 months", length.out = 2)[2]
  ordem <- elos %>% distinct(cadeia_id, elo_ordem, elo, nucleo)
  mov <- cg %>% filter(data >= desde) %>% ligar_elos(elos, c("cadeia_id", "elo_ordem", "elo")) %>% group_by(cadeia_id, elo_ordem, elo) %>%
    summarise(salario_admissao = weighted.mean(salario_admissao, admissoes, na.rm = TRUE), admissoes = sum(admissoes, na.rm = TRUE),
              desligamentos = sum(desligamentos, na.rm = TRUE), saldo = sum(saldo, na.rm = TRUE), .groups = "drop")
  ordem %>% left_join(mov, by = c("cadeia_id", "elo_ordem", "elo")) %>%
    mutate(across(c(admissoes, desligamentos, saldo), ~ coalesce(.x, 0)), desde = desde, ate = ate) %>% arrange(cadeia_id, elo_ordem)
}
