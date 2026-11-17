# =============================================================================
# Cooperativas e entidades associativas (CNPJ, Receita Federal)
#
# Estabelecimentos ativos cuja natureza jurídica é cooperativa (2143),
# associação privada (3999) ou entidade sindical (3131). Ficam de fora
# organizações religiosas, condomínios e partidos pela atividade principal.
# A ligação com as cadeias vem por dois caminhos:
#   pela atividade (CNAE) principal, quando ela está nos elos da cadeia;
#   pelo nome, com as palavras de config/associativas_cadeias.csv, porque
#   associação de produtores costuma usar um CNAE genérico (9430-8, 9499-5).
# A ligação pelo nome é aproximada e vem marcada como tal.
# =============================================================================

NATUREZA_ASSOC <- c("2143" = "Cooperativa", "3999" = "Associação", "3131" = "Entidade sindical")
CNAE_FORA_ASSOC <- c("9491000", "8112500", "9492800")   # religiosas, condomínios, partidos
# nomes que indicam entidade de quem produz (sem cadeia específica)
PADRAO_PRODUTIVO <- "produtor|agricult|pecuarist|criador|apicult|pescador|marisqueir|artesa|artesanato|costureir|rendeir|bordadeir|extrativ|assentad|agroind|trabalhadores rurais|feirante|catador|reciclad|leite|leiteir"

extrair_cnpj_associativas <- function(projeto, uf) {
  nat <- paste0("'", names(NATUREZA_ASSOC), "'", collapse = ", ")
  fora <- paste0("'", CNAE_FORA_ASSOC, "'", collapse = ", ")
  consultar(glue::glue("
    WITH e AS (
      SELECT cnpj, cnpj_basico, id_municipio, cnae_fiscal_principal AS cnae, data_inicio_atividade,
             identificador_matriz_filial AS matriz_filial, nome_fantasia
      FROM `basedosdados.br_rf_cnpj.estabelecimentos`
      WHERE data_referencia = {SQL_CNPJ_ULT} AND sigla_uf = '{uf}' AND situacao_cadastral IN ('2', '02')),
    emp AS (
      SELECT cnpj_basico, razao_social, natureza_juridica, capital_social
      FROM `basedosdados.br_rf_cnpj.empresas`
      WHERE data_referencia = (SELECT MAX(data_referencia) FROM `basedosdados.br_rf_cnpj.empresas` WHERE data_referencia >= DATE_SUB(CURRENT_DATE(), INTERVAL 540 DAY))
        AND natureza_juridica IN ({nat}))
    SELECT e.*, emp.razao_social, emp.natureza_juridica, emp.capital_social
    FROM e JOIN emp USING (cnpj_basico)
    WHERE e.cnae NOT IN ({fora})"), projeto) %>%
    mutate(id_municipio = as.character(id_municipio))
}

ler_associativas_cadeias <- function(arq = caminho("config", "associativas_cadeias.csv")) {
  if (!file.exists(arq)) return(tibble(cadeia_id = character(), padrao = character()))
  readr::read_csv(arq, col_types = "cc")
}

montar_associativas <- function(raw, elos, meta, padroes, dir_cnae, municipios, rd) {
  if (!NROW(raw)) return(NULL)
  a <- raw %>% mutate(tipo = unname(NATUREZA_ASSOC[as.character(natureza_juridica)]),
                      subclasse = cnae, classe = substr(cnae, 1, 5),
                      nome = stringr::str_squish(coalesce(na_if(stringr::str_squish(nome_fantasia), ""), razao_social)),
                      razao_social = stringr::str_squish(razao_social),
                      nome_n = normalizar_txt(paste(razao_social, coalesce(nome_fantasia, ""))),
                      abertura = as.numeric(format(as.Date(data_inicio_atividade), "%Y"))) %>%
    filter(!is.na(tipo), !grepl("[0-9]{11}", razao_social))
  # pela atividade
  por_cnae <- a %>% select(cnpj, classe, subclasse) %>% ligar_elos(elos, "cadeia_id") %>% distinct(cnpj, cadeia_id) %>% mutate(via = "atividade")
  # pelo nome
  por_nome <- if (nrow(padroes)) purrr::map_dfr(seq_len(nrow(padroes)), function(i)
    a %>% filter(grepl(padroes$padrao[i], nome_n, perl = TRUE)) %>% transmute(cnpj, cadeia_id = padroes$cadeia_id[i], via = "nome")) else tibble()
  lig <- bind_rows(por_cnae, por_nome) %>% arrange(cnpj, cadeia_id, via) %>% distinct(cnpj, cadeia_id, .keep_all = TRUE)
  cad_txt <- lig %>% group_by(cnpj) %>% summarise(cadeias = paste(cadeia_id, collapse = ";"), via = paste(sort(unique(via)), collapse = " e "), .groups = "drop")
  sec <- substr(a$cnae, 1, 2)
  a <- a %>% left_join(cad_txt, by = "cnpj") %>%
    mutate(cadeias = coalesce(cadeias, ""),
           atividade = dir_cnae$descricao_subclasse[match(cnae, dir_cnae$subclasse)],
           credito = substr(cnae, 1, 2) == "64",   # crédito; planos de saúde (65, como as Unimed) ficam como cooperativa
           # produtiva: ligada a cadeia, com atividade de produção (agro, indústria, extração) ou com nome de entidade de produtores
           produtiva = !credito & (nzchar(cadeias) | as.numeric(sec) <= 33 | grepl(PADRAO_PRODUTIVO, nome_n)),
           grupo = case_when(tipo == "Cooperativa" & credito ~ "Cooperativa de crédito",
                             tipo == "Cooperativa" ~ "Cooperativa",
                             tipo == "Entidade sindical" ~ "Entidade sindical",
                             produtiva ~ "Associação produtiva",
                             TRUE ~ "Outra associação")) %>%
    left_join(municipios %>% select(id_municipio, municipio), by = "id_municipio") %>%
    left_join(rd %>% mutate(id_municipio = as.character(id_municipio)) %>% select(id_municipio, rd), by = "id_municipio")
  contagem <- a %>% count(id_municipio, grupo) %>% tidyr::pivot_wider(names_from = grupo, values_from = n, values_fill = 0)
  if (!"capital_social" %in% names(a)) a$capital_social <- NA_real_
  a <- a %>% mutate(capital_social = suppressWarnings(as.numeric(capital_social)), cnpj_basico = coalesce(cnpj_basico, substr(cnpj, 1, 8)))
  lista <- a %>% filter(grupo %in% c("Cooperativa", "Associação produtiva", "Cooperativa de crédito") | nzchar(cadeias)) %>%
    select(id_municipio, municipio, rd, nome, razao_social, tipo, grupo, atividade, cadeias, via, abertura)
  # uma linha por entidade (raiz do CNPJ): sede na matriz, unidades e municípios onde está em PE
  principais <- a %>% filter(grupo %in% c("Cooperativa", "Cooperativa de crédito", "Associação produtiva", "Entidade sindical")) %>%
    mutate(ordem = ifelse(matriz_filial %in% c("1", "01"), 0, 1)) %>% arrange(cnpj_basico, ordem, abertura) %>%
    group_by(cnpj_basico) %>%
    # unidades e municípios primeiro: no summarise, id_municipio passaria a ser só o da sede nas linhas seguintes
    summarise(unidades = n(), municipios = n_distinct(id_municipio), nome = first(nome), razao_social = first(razao_social), grupo = first(grupo), tipo = first(tipo),
              id_municipio = first(id_municipio), municipio = first(municipio), rd = first(rd), atividade = first(atividade),
              cadeias = paste(setdiff(unique(unlist(strsplit(cadeias, ";"))), c("", NA)), collapse = ";"), via = first(via),
              capital_social = suppressWarnings(max(capital_social, na.rm = TRUE)),
              abertura = min(abertura, na.rm = TRUE), .groups = "drop") %>%
    mutate(capital_social = ifelse(is.finite(capital_social), capital_social, NA_real_), abertura = ifelse(is.finite(abertura), abertura, NA_real_)) %>%
    select(-cnpj_basico)
  list(data_base = Sys.Date(), contagem = contagem, lista = lista, principais = principais, total = a %>% count(grupo))
}

# Pedaço para o JSON do território
pedaco_associativas <- function(ass, ids, nomes_cad, n = 40) {
  if (is.null(ass)) return(NULL)
  ct <- ass$contagem %>% filter(id_municipio %in% ids)
  l <- ass$lista %>% filter(id_municipio %in% ids)
  if (!nrow(ct) && !nrow(l)) return(NULL)
  soma <- function(k) if (k %in% names(ct)) sum(ct[[k]]) else 0
  ordem <- c("Cooperativa", "Associação produtiva", "Cooperativa de crédito", "Entidade sindical", "Outra associação")
  l <- l %>% arrange(match(grupo, ordem), !nzchar(cadeias), nome)
  list(n = setNames(lapply(ordem, soma), ordem),
       lista = l %>% head(n) %>% mutate(cadeias = lapply(strsplit(cadeias, ";"), function(k) if (length(k)) lapply(k, function(i) list(id = i, nome = unname(nomes_cad[i]))) else list())) %>%
         select(nome, grupo, atividade, municipio, cadeias, via),
       total_lista = nrow(l))
}
