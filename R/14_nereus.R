# =============================================================================
# Matriz interestadual de insumo-produto do NEREUS/USP, 2019
# Haddad, E. A.; Araújo, I. A.; Rocha, A. A.; Vale, V. A. (2025). Matriz
# Interestadual de Insumo-Produto para o Brasil, 2019. Revista Brasileira de
# Estudos Regionais e Urbanos, v. 19, n. 4.
#
# 68 setores x 27 UFs (setor x setor), com valor da produção, VAB, ocupações,
# emissões, uso de água e consumo de energia por setor e UF. Aqui ela é usada
# ao lado da MIP nacional regionalizada (FLQ), não no lugar dela.
#
# O arquivo original (IIOAS_BRUF_2019.rar ou .xlsx, ~120 MB) fica em
# dados_externos/nereus/. Na primeira leitura ele vira nereus_2019.rds, bem
# menor e rápido de ler.
# =============================================================================

NEREUS_PASTA <- function() caminho("dados_externos", "nereus")

preparar_nereus <- function(xlsx) {
  msg("NEREUS: lendo ", basename(xlsx), " (pode levar alguns minutos)")
  x <- as.data.frame(readxl::read_excel(xlsx, "MIIP SS", col_names = FALSE, .name_repair = "minimal"))
  num <- function(v) suppressWarnings(as.numeric(v))
  # linhas e colunas do bloco intermediário: região na coluna 2 / linha 4, setor na coluna 3 / linha 6
  lin <- which(grepl("^R\\d{2}:", x[[2]]) & grepl("^S\\d{2}$", x[[3]]))
  col <- which(grepl("^R\\d{2}:", unlist(x[4, ])) & grepl("^S\\d{2}$", unlist(x[6, ])))
  if (length(lin) != length(col) || !length(lin)) stop("NEREUS: não reconheci o bloco setor x setor da aba MIIP SS")
  reg_l <- sub("^R\\d{2}:\\s*", "", x[[2]][lin]); set_l <- x[[3]][lin]
  rot <- paste(reg_l, set_l, sep = "_")
  Z <- matrix(num(unlist(x[lin, col])), nrow = length(lin), dimnames = list(rot, rot))
  Z[is.na(Z)] <- 0
  # demanda final: blocos INV, HH, GOV, NPISH e EXP, uma coluna por UF
  cfd <- which(unlist(x[5, ]) %in% c("INV", "HH", "GOV", "NPISH", "EXP"))
  FD <- matrix(num(unlist(x[lin, cfd])), nrow = length(lin),
               dimnames = list(rot, paste(unlist(x[5, cfd]), sub("^R\\d{2}:\\s*", "", unlist(x[6, cfd])), sep = "_")))
  FD[is.na(FD)] <- 0
  # linhas de baixo (por coluna do bloco intermediário)
  rotulo <- trimws(x[[4]])
  pega <- function(padrao) { i <- which(grepl(padrao, rotulo))[1]; if (is.na(i)) rep(NA_real_, length(col)) else setNames(num(unlist(x[i, col])), rot) }
  linhas <- list(importacoes = pega("^Importações internacionais"), vab = pega("^Valor adicionado bruto"),
                 remuneracoes = pega("^Remunerações"), vp = pega("^Valor da produção"), ocupacoes = pega("^Fator trabalho"),
                 co2 = pega("^Emissões de CO2"), agua = pega("^Uso de Água"), energia = pega("^Consumo Energético"))
  setores <- tibble(setor = unlist(x[6, col[1:68]]), setor_nome = unlist(x[5, col[1:68]]))
  list(Z = Z, FD = FD, linhas = linhas, setores = setores, ufs = unique(reg_l), ano = 2019,
       fonte = "Haddad, Araújo, Rocha e Vale (2025), Matriz Interestadual de Insumo-Produto para o Brasil, 2019 (NEREUS/USP)")
}

# Lê a versão compacta; se não houver, prepara a partir do xlsx (ou do .rar,
# quando o pacote archive está instalado). Sem arquivo, devolve NULL.
obter_nereus <- function(pasta = NEREUS_PASTA()) {
  rds <- file.path(pasta, "nereus_2019.rds")
  if (file.exists(rds)) return(readRDS(rds))
  xlsx <- list.files(pasta, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(xlsx)) {
    rar <- list.files(pasta, pattern = "\\.(rar|zip)$", full.names = TRUE)
    if (length(rar) && requireNamespace("archive", quietly = TRUE)) {
      archive::archive_extract(rar[1], dir = pasta)
      xlsx <- list.files(pasta, pattern = "\\.xlsx$", full.names = TRUE)
    }
  }
  if (!length(xlsx)) { msg("NEREUS: arquivo não encontrado em ", pasta, "; módulo pulado."); return(NULL) }
  nr <- preparar_nereus(xlsx[1])
  saveRDS(nr, rds, compress = "xz")
  nr
}

# Indicadores de Pernambuco a partir da matriz interestadual
analisar_nereus <- function(nr, mip_ativ, uf = "PE") {
  if (is.null(nr)) return(NULL)
  Z <- nr$Z; vp <- nr$linhas$vp; n <- nrow(Z)
  uf_de <- sub("_S\\d{2}$", "", rownames(Z)); set_de <- sub("^.*_", "", rownames(Z))
  pe <- uf_de == uf
  A <- sweep(Z, 2, ifelse(vp > 0, vp, 1), "/")
  L <- solve(diag(n) - A)
  e_coef <- ifelse(vp > 0, nr$linhas$ocupacoes / vp, 0)
  # multiplicadores (coluna de cada setor de PE): efeito total, dentro de PE e vazamento para outras UFs
  Lpe <- L[, pe]
  mult_tot <- colSums(Lpe); mult_pe <- colSums(Lpe[pe, , drop = FALSE])
  emp_tot <- colSums(e_coef * Lpe); emp_pe <- colSums((e_coef * Lpe)[pe, , drop = FALSE])
  # compras de insumos dos setores de PE: de PE, de outras UFs e do exterior
  cpe <- colSums(Z[pe, pe]); cout <- colSums(Z[!pe, pe]); cimp <- nr$linhas$importacoes[pe]
  # vendas dos setores de PE: insumos para PE e para outras UFs, demanda final em PE, em outras UFs, exportações
  FD <- nr$FD; fd_uf <- sub("^[A-Z]+_", "", colnames(FD)); fd_tipo <- sub("_.*$", "", colnames(FD))
  vend <- tibble(
    v_int_pe = rowSums(Z[pe, pe]), v_int_out = rowSums(Z[pe, !pe]),
    v_fd_pe = rowSums(FD[pe, fd_tipo != "EXP" & fd_uf == uf, drop = FALSE]),
    v_fd_out = rowSums(FD[pe, fd_tipo != "EXP" & fd_uf != uf, drop = FALSE]),
    v_exp = rowSums(FD[pe, fd_tipo == "EXP", drop = FALSE]))
  # principal UF de onde PE compra insumos, por setor
  orig <- rowsum(Z[!pe, pe], uf_de[!pe])
  top_uf <- ifelse(apply(orig, 2, max) > 0, rownames(orig)[apply(orig, 2, which.max)], NA_character_)
  # tamanho de PE frente ao Brasil
  br <- function(v) c(tapply(v, set_de, sum, na.rm = TRUE))
  ql <- function(v) { p <- v[pe]; b <- br(v)[set_de[pe]]; as.numeric((p / sum(p, na.rm = TRUE)) / (b / sum(b, na.rm = TRUE))) }
  li <- nr$linhas
  setores <- nr$setores %>% mutate(
    vp = li$vp[pe], vab = li$vab[pe], ocupacoes = li$ocupacoes[pe], energia_mil_tep = li$energia[pe], co2_t = li$co2[pe],
    part_br_vp = li$vp[pe] / br(li$vp)[setor],
    ql_producao = ql(li$vp), ql_vab = ql(li$vab), ql_ocupacoes = ql(li$ocupacoes),
    part_br_energia = li$energia[pe] / br(li$energia)[setor],
    tep_por_mi = ifelse(vp > 0, 1000 * energia_mil_tep / vp, NA),
    tep_por_mi_br = 1000 * br(li$energia)[setor] / br(li$vp)[setor],
    mult_producao = mult_tot, mult_producao_pe = mult_pe, vazamento = 1 - mult_pe / mult_tot,
    mult_emprego = emp_tot, mult_emprego_pe = emp_pe,
    insumos_pe = cpe / (cpe + cout + cimp), insumos_outras_uf = cout / (cpe + cout + cimp), insumos_exterior = cimp / (cpe + cout + cimp),
    principal_uf_insumos = top_uf) %>%
    bind_cols(vend) %>%
    mutate(vendas_total = v_int_pe + v_int_out + v_fd_pe + v_fd_out + v_exp,
           vendas_pe = (v_int_pe + v_fd_pe) / vendas_total, vendas_outras_uf = (v_int_out + v_fd_out) / vendas_total,
           vendas_exterior = v_exp / vendas_total) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA)))
  # setores NEREUS (S01..S68) na mesma agregação da MIP do IBGE usada no painel (coluna setor)
  ativ <- mip_ativ %>% filter(!grepl("nível 67", descricao))
  nrm <- function(z) tolower(iconv(gsub("[^[:alnum:] ]", "", z), to = "ASCII//TRANSLIT"))
  setores$grupo <- ativ$setor[match(nrm(setores$setor_nome), nrm(ativ$descricao))]
  # ligações de Rasmussen-Hirschman com o bloco de PE (coeficientes intra-estaduais), na agregação do painel
  g <- setores$grupo; ok <- !is.na(g)
  S <- outer(unique(g[ok]), g, `==`) * 1
  Zg <- S %*% Z[pe, pe] %*% t(S); vpg <- as.vector(S %*% vp[pe])
  Ag <- sweep(Zg, 2, ifelse(vpg > 0, vpg, 1), "/"); dimnames(Ag) <- list(unique(g[ok]), unique(g[ok]))
  rh <- indices_ligacao(Ag) %>% rename(bl_nereus = bl, fl_nereus = fl, mult_nereus = mult_producao, class_nereus = classificacao)
  # com quem PE troca: vendas e compras (insumos e demanda final) com cada UF
  comp_uf <- rowsum(rowSums(Z[!pe, pe]) + rowSums(FD[!pe, fd_tipo != "EXP" & fd_uf == uf, drop = FALSE]), uf_de[!pe])
  vend_uf <- sapply(setdiff(nr$ufs, uf), function(u) sum(Z[pe, uf_de == u]) + sum(FD[pe, fd_tipo != "EXP" & fd_uf == u]))
  parceiros <- tibble(sigla_uf = names(vend_uf), vendas_pe = unname(vend_uf), compras_pe = comp_uf[names(vend_uf), 1]) %>%
    mutate(saldo = vendas_pe - compras_pe) %>% arrange(desc(vendas_pe + compras_pe))
  tot <- function(v) sum(v[pe], na.rm = TRUE)
  list(setores = setores, rh = rh, parceiros = parceiros, ano = nr$ano, fonte = nr$fonte,
       resumo = list(vp = tot(li$vp), vab = tot(li$vab), ocupacoes = tot(li$ocupacoes),
                     part_br_vab = tot(li$vab) / sum(li$vab, na.rm = TRUE),
                     vazamento_medio = weighted.mean(setores$vazamento, setores$vp, na.rm = TRUE),
                     insumos_outras_uf = sum(cout) / sum(cpe + cout + cimp), insumos_exterior = sum(cimp) / sum(cpe + cout + cimp),
                     energia_mil_tep = tot(li$energia), part_br_energia = tot(li$energia) / sum(li$energia, na.rm = TRUE)))
}

# Indicadores NEREUS para os setores da MIP ligados a cada cadeia (via mip_cadeia)
nereus_cadeia <- function(nereus, mip_cadeia) {
  if (is.null(nereus) || is.null(mip_cadeia)) return(NULL)
  s <- nereus$setores %>% filter(!is.na(grupo)) %>% group_by(grupo) %>%
    summarise(across(c(ql_producao, ql_ocupacoes, mult_producao, vazamento, mult_emprego, insumos_outras_uf, insumos_exterior,
                       vendas_outras_uf, vendas_exterior), ~ weighted.mean(.x, vp, na.rm = TRUE)),
              part_br_vp = sum(vp, na.rm = TRUE) / sum(vp / part_br_vp, na.rm = TRUE),
              across(c(vp, energia_mil_tep, ocupacoes), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop")
  mip_cadeia %>% distinct(cadeia_id, elo, grupo, grupo_nome) %>% inner_join(s, by = "grupo") %>%
    left_join(nereus$rh %>% select(grupo, bl_nereus, fl_nereus, class_nereus), by = "grupo")
}

NOMES_UF_BR <- c(RO = "Rondônia", AC = "Acre", AM = "Amazonas", RR = "Roraima", PA = "Pará", AP = "Amapá", TO = "Tocantins",
                 MA = "Maranhão", PI = "Piauí", CE = "Ceará", RN = "Rio Grande do Norte", PB = "Paraíba", PE = "Pernambuco",
                 AL = "Alagoas", SE = "Sergipe", BA = "Bahia", MG = "Minas Gerais", ES = "Espírito Santo", RJ = "Rio de Janeiro",
                 SP = "São Paulo", PR = "Paraná", SC = "Santa Catarina", RS = "Rio Grande do Sul", MS = "Mato Grosso do Sul",
                 MT = "Mato Grosso", GO = "Goiás", DF = "Distrito Federal")
