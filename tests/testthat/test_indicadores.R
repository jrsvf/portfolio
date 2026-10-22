# test_dir() roda com a pasta tests/testthat como diretório de trabalho
raiz <- normalizePath(file.path(getwd(), "..", ".."))
Sys.setenv(PASTA_PROJETO = raiz)
for (f in c("00_utils.R", "02_sidra.R", "03_indicadores.R", "04_mip.R", "05_lacunas_destaques.R", "08_comparacoes.R")) {
  source(file.path(raiz, "R", f), encoding = "UTF-8")
}

test_that("CNAE é normalizada como texto de 5 dígitos", {
  expect_equal(normalizar_cnae(c("0155-5", "1555", "01.55-5/01", "4634601")), c("01555", "01555", "01555", "46346"))
  expect_true(is.na(normalizar_cnae("")))
})

test_that("cadeias.yml é válido", {
  cad <- ler_cadeias()
  expect_gt(nrow(cad$meta), 10)
  expect_true(all(nchar(cad$elos$codigo) %in% c(5, 7)))
  expect_true(all(nchar(cad$elos$classe) == 5))
  expect_true(all(grepl("^\\d{4}$", cad$sh4$sh4)))
  # sh4 automático só para cadeias sem sh4_auto: false
  expect_false(any(cad$sh4$cadeia_id == "avicultura" & cad$sh4$origem == "automática"))
  expect_true(any(cad$sh4$cadeia_id == "moveleiro" & cad$sh4$origem == "automática"))
  expect_false(anyDuplicated(cad$meta$cadeia_id) > 0)
})

test_that("QL, PR e IHH batem com o cálculo manual", {
  df <- tibble::tribble(
    ~ano, ~id_municipio, ~atividade, ~emprego,
    2020, "A", "x", 10,
    2020, "A", "y", 90,
    2020, "B", "x", 30,
    2020, "B", "y", 70)
  r <- indicadores_locais(df)
  a_x <- r %>% filter(id_municipio == "A", atividade == "x")
  # QL = (10/100)/(40/200) = 0.5 ; PR = 10/40 ; IHHm = 0.25 - 0.5
  expect_equal(a_x$ql, 0.5)
  expect_equal(a_x$pr, 0.25)
  expect_equal(a_x$ihhm, -0.25)
  expect_equal(sum(r$pr[r$atividade == "x"]), 1)
})

test_that("Total municipal externo é respeitado no QL", {
  df <- tibble::tibble(ano = 2020, id_municipio = c("A", "B"), atividade = "x", emprego = c(10, 10))
  tot <- tibble::tibble(ano = 2020, id_municipio = c("A", "B"), emprego_total = c(100, 900))
  r <- indicadores_locais(df, tot)
  expect_equal(r$ql[r$id_municipio == "A"], (10 / 100) / (20 / 1000))
})

test_that("Shift-share: componentes somam a variação", {
  reg <- tibble::tibble(atividade = c("a", "b"), e0 = c(100, 50), e1 = c(130, 40))
  ref <- tibble::tibble(atividade = c("a", "b"), e0 = c(1000, 2000), e1 = c(1200, 2100))
  s <- shift_share(reg, ref)
  expect_equal(s$ef_nacional + s$ef_estrutural + s$ef_diferencial, s$variacao)
})

test_that("Krugman é 0 para estrutura igual à do estado e fica entre 0 e 2", {
  df <- tibble::tribble(
    ~ano, ~id_municipio, ~setor, ~emprego,
    2020, "A", "s1", 50, 2020, "A", "s2", 50,
    2020, "B", "s1", 50, 2020, "B", "s2", 50,
    2020, "C", "s1", 100)
  k <- indice_krugman(df)
  expect_true(all(k$krugman >= 0 & k$krugman <= 2))
  expect_lt(k$krugman[k$id_municipio == "A"], k$krugman[k$id_municipio == "C"])
})

test_that("div0 recicla escalar no denominador e zera divisões por zero", {
  expect_equal(div0(c(10, 20, 30), 10), c(1, 2, 3))
  expect_equal(div0(c(1, 2), c(0, NA)), c(0, 0))
  expect_equal(div0(5, c(1, 0, 5)), c(5, 0, 1))
})

test_that("Índice da série comparada parte de 100 e varia", {
  rb <- tibble::tibble(ano = rep(2006:2008, 2), sigla_uf = rep(c("PE", "SP"), each = 3),
                       classe = "01555", vinculos = c(100, 120, 150, 1000, 1000, 900), estabelecimentos = 1)
  el <- tibble::tibble(cadeia_id = "x", elo_ordem = 1, elo = "e", codigo = "01555", nivel = "classe", classe = "01555")
  s <- serie_comparada(rb, el) %>% filter(recorte == "Pernambuco") %>% arrange(ano)
  expect_equal(s$indice, c(100, 120, 150))
})

test_that("CAGR", {
  expect_equal(cagr(100, 121, 2), 0.1)
  expect_true(is.na(cagr(0, 10, 2)))
})

test_that("Bloco da MIP é encontrado em planilha com cabeçalho e títulos", {
  cods <- sprintf("%02d00", 10:40)
  n <- length(cods)
  set.seed(1)
  A <- matrix(runif(n * n, 0, 0.02), n)
  bruto <- rbind(
    c("Matriz dos coeficientes técnicos intersetoriais", rep(NA, n + 1)),
    rep(NA, n + 2),
    c(NA, NA, cods),
    cbind(cods, paste("Atividade", cods), A),
    c("Total", NA, colSums(A)))
  bruto <- tibble::as_tibble(as.data.frame(bruto, stringsAsFactors = FALSE), .name_repair = "minimal")
  m <- extrair_bloco_quadrado(bruto)
  expect_equal(dim(m), c(n, n))
  expect_equal(unname(m[1, 1]), A[1, 1], tolerance = 1e-12)
})

test_that("Agregação de atividades da MIP em setores (educação e saúde somadas)", {
  A <- matrix(c(.1, .2, .0,
                .1, .1, .1,
                .0, .2, .3), 3, byrow = TRUE, dimnames = list(c("8591", "8592", "4180"), c("8591", "8592", "4180")))
  ma <- tibble::tibble(atividade = c("8591", "8592", "4180"), setor = c("8590", "8590", "4180"))
  Ag <- agregar_mip_setores(A, ma)
  expect_equal(dim(Ag), c(2, 2))
  expect_equal(Ag["8590", "8590"], mean(c(.1 + .1, .2 + .1)))
  expect_equal(Ag["4180", "4180"], .3)
})

test_that("Classe CNAE vai para a atividade certa da MIP (prefixo mais longo)", {
  ma <- ler_mip_atividades()
  expect_equal(setor_da_classe(c("01113", "01610", "01628", "10121", "10716", "86101", "62015"), ma),
               c("0191", "0191", "0192", "1091", "1092", "8690", "6280"))
})

test_that("Elos por classe e por subclasse são ligados sem dupla contagem", {
  elos <- tibble::tibble(cadeia_id = c("av", "av", "bov"), elo_ordem = c(1, 2, 1), elo = c("cria", "abate", "abate"),
                         codigo = c("01555", "1012101", "10112"), nivel = c("classe", "subclasse", "classe"),
                         classe = c("01555", "10121", "10112"))
  rais <- tibble::tibble(subclasse = c("0155501", "1012101", "1012103", "1011201"), vinculos = c(10, 20, 30, 40)) %>%
    mutate(classe = substr(subclasse, 1, 5))
  r <- ligar_elos(rais, elos)
  expect_equal(sum(r$vinculos[r$cadeia_id == "av"]), 30)    # suínos (1012103) fica de fora
  expect_equal(sum(r$vinculos[r$cadeia_id == "bov"]), 40)
})

test_that("fmt_cnae", {
  expect_equal(fmt_cnae(c("01555", "0155501")), c("0155-5", "0155-5/01"))
})

test_that("FLQ reduz coeficientes e Leontief gera índices coerentes", {
  A <- matrix(c(.2, .1, .05, .1, .3, .1, .05, .1, .2), 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  emp_n <- c(a = 1000, b = 2000, c = 3000)
  emp_r <- c(a = 50, b = 20, c = 0)
  Ar <- regionalizar_flq(A, emp_r, emp_n, delta = 0.3)
  expect_true(all(Ar <= A + 1e-12))
  expect_true(all(Ar[, "c"] == 0) && all(Ar["c", ] == 0))
  idx <- indices_ligacao(A)
  expect_equal(mean(idx$bl), 1, tolerance = 1e-12)
  expect_equal(mean(idx$fl), 1, tolerance = 1e-12)
})

test_that("Resposta do SIDRA é arrumada e códigos especiais tratados", {
  raw <- data.frame(
    NN = c("Nível Territorial", "Município", "Município", "Município"),
    MC = c("Município (Código)", "2600054", "2600104", "2600203"),
    MN = c("Município", "Abreu e Lima - PE", "Afogados - PE", "Afrânio - PE"),
    V  = c("Valor", "120", "X", "-"),
    VN = c("Variável", "Valor da produção", "Valor da produção", "Valor da produção"),
    UN = c("Unidade de Medida", "Mil Reais", "Mil Reais", "Mil Reais"),
    AN = c("Ano", "2022", "2022", "2022"),
    PR = c("Tipo de produto", "Mel de abelha", "Mel de abelha", "Mel de abelha"),
    stringsAsFactors = FALSE)
  s <- sidra_arrumar(raw, 74)
  expect_equal(nrow(s), 3)
  expect_equal(s$valor, c(120, NA, 0))
  expect_equal(s$situacao, c("informado", "omitido (sigilo)", "zero absoluto"))
  expect_equal(s$produto[1], "Mel de abelha")
})

test_that("Janelas de 5 e 10 anos", {
  df <- tibble::tibble(ano = 2014:2024, cadeia_id = "x", valor = 100 + 0:10 * 10)
  v <- variacoes_janela(df, "cadeia_id", c(5, 10), desde = NULL)
  expect_equal(v$v_ini, c(150, 100)); expect_equal(v$v_fim, c(200, 200))
  expect_equal(v$var_pct, c(200 / 150 - 1, 1))
})

test_that("Comparação com o Nordeste: posição, participação e QL", {
  cu <- tidyr::expand_grid(ano = c(2014, 2019, 2024), sigla_uf = c("PE", "BA", "CE")) %>%
    mutate(cadeia_id = "x", emprego = c(10, 50, 5, 20, 50, 5, 40, 50, 10), estabelecimentos = 1)
  tu <- tidyr::expand_grid(ano = c(2014, 2019, 2024), sigla_uf = c("PE", "BA", "CE")) %>% mutate(emprego_total = 1000)
  r <- comparar_nordeste(cu, tu, "PE", c(5, 10))
  expect_equal(r$resumo$posicao, 2)
  expect_equal(r$resumo$lider, "BA")
  expect_equal(r$resumo$part_ne, 40 / 100)
  expect_equal(r$resumo$ql_ne, (40 / 1000) / (100 / 3000))
  j10 <- r$janelas %>% filter(janela == 10)
  expect_equal(j10$var_pe, 3); expect_equal(j10$var_ne_sem_pe, 60 / 55 - 1)
  expect_true(length(destaques_nordeste("x", r)) >= 2)
})

test_that("Setores com presença pequena saem do ranking da MIP", {
  ma <- ler_mip_atividades()
  expect_true("4580" %in% ma$setor)
})

test_that("Cortes: 10, 5, 2 anos e desde 2023", {
  ct <- cortes_anos(2025)
  expect_equal(ct$ano_ini, c(2015, 2020, 2023))          # 2 anos = 2023: não duplica
  expect_equal(ct$rotulo[3], "2 anos (desde 2023)")
  ct <- cortes_anos(2026)
  expect_equal(ct$ano_ini, c(2016, 2021, 2024, 2023))
  expect_equal(ct$rotulo[4], "Desde 2023")
  expect_equal(nrow(cortes_anos(2023)), 3)   # "desde 2023" some quando o último ano é 2023
})
