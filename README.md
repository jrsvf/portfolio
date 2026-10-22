# Portfólio: economia regional e cadeias produtivas de Pernambuco

Site e painéis gerados automaticamente a partir de dados públicos (RAIS, Novo CAGED, IBGE, ComexStat), consultados no BigQuery pelo projeto `analises1`. Todo mês um robô no GitHub refaz as consultas, recalcula os indicadores, regenera as páginas e republica.

## O que o pipeline produz

| Página | Conteúdo |
|---|---|
| Início | Vitrine: apresentação, números do portfólio, destaques das cadeias |
| Panorama | Emprego formal desde 2006, participação de PE no Brasil, estrutura por seção, concentração territorial, índice de Krugman, cadeia dominante por município, comércio exterior |
| Cadeias (uma página cada) | Elos da cadeia, série histórica por elo, mapas de QL e emprego, ranking, PE x Nordeste x Brasil, shift-share, Novo CAGED mensal, produção física do IBGE, exportações e destinos, encadeamentos, lacunas |
| Encadeamentos | Matriz insumo-produto nacional regionalizada para PE (FLQ), ligações para trás e para frente, setores-chave, comparação com o Brasil |
| Lacunas de dados | Onde os dados públicos não fecham e o que resolveria; defasagem das fontes; dados abertos em CSV |
| Painéis | Explorador de cadeias (Shiny novo) e o Diagnóstico Setorial (seu app `diag`, agora alimentado pelo pipeline) |
| Estudos e Metodologia | Lista editável de estudos; fórmulas e regras |

## Estrutura

```
config/
  cadeias.yml          <- as 21 cadeias, elos (classe ou subclasse CNAE), SH4 e produtos do IBGE. É aqui que você mexe.
  parametros.yml       <- anos, projeto do BigQuery, QL mínimo, URL da MIP, delta do FLQ
  mip_atividades.csv   <- correspondência oficial IBGE atividades da MIP x CNAE 2.0
  sh4_cnae.csv         <- correspondência NCM x CNAE (sua planilha de EXPORTAÇÃO), resumida em SH4
  rd_municipios.csv    <- 185 municípios e as 12 RDs (da base municipal do PE Produz)
R/
  00_utils.R           <- configuração e utilitários
  01_bigquery.R        <- consultas (RAIS, CAGED, Comex, PIB, população) com conferência de esquema
  02_sidra.R           <- API do SIDRA (PAM, PPM, PEVS)
  03_indicadores.R     <- QL, PR, IHH, HHI, Krugman, shift-share, agregação por cadeia
  04_mip.R             <- insumo-produto regionalizado (FLQ) e índices de ligação
  05_lacunas_destaques.R
  06_exportar.R        <- malha, base do app diag, CSVs
  07_site.R            <- gráficos, mapas e gerador das páginas das cadeias
_targets.R             <- o pipeline
site/                  <- site Quarto
shiny/cadeias/app.R    <- explorador de cadeias
shiny/diag/app.R       <- seu app diag (com uma correção no gráfico de PR)
tests/                 <- testes das funções de cálculo
.github/workflows/atualizar.yml  <- automação mensal
```

## 1. Primeira execução no seu computador

1. Instale R (4.3 ou mais recente), RStudio e o [Quarto](https://quarto.org/docs/get-started/).
2. Abra a pasta do projeto no RStudio e instale os pacotes:
   ```r
   install.packages(c("targets", "bigrquery", "dplyr", "tidyr", "purrr", "stringr", "yaml",
                      "glue", "readr", "readxl", "jsonlite", "sf", "geobr", "leaflet",
                      "plotly", "DT", "htmltools", "knitr", "rmarkdown", "shiny", "bslib",
                      "rsconnect", "testthat"))
   ```
3. Rode o pipeline:
   ```r
   targets::tar_make()
   ```
   Na primeira vez o `bigrquery` abre o navegador para você autorizar com a conta Google que tem acesso ao `analises1`.
4. Veja o site:
   ```r
   quarto::quarto_preview("site")   # ou, no terminal: quarto preview site
   ```

### O que conferir na primeira execução

Algumas coisas não puderam ser testadas contra as fontes reais, porque o ambiente onde o código foi escrito não acessa o BigQuery nem o IBGE. Os cálculos foram testados com dados simulados (33 testes passando e site renderizado de ponta a ponta), mas confira:

- **Esquema da Base dos Dados.** O primeiro passo (`bq_esquema`) confere se todas as colunas existem. Se alguma mudou de nome, o erro diz qual tabela e qual coluna ajustar em `R/01_bigquery.R`.
- **Códigos da RAIS.** As colunas e formatos batem com o seu relatório `schema_inspecao.txt` do artigo de convergência. Só o código de sexo (`'2'` = mulheres) não estava lá: confira no dicionário da Base dos Dados.
- **Códigos CNAE das cadeias.** O pipeline confere cada classe e subclasse do `cadeias.yml` contra o diretório CNAE oficial e para com a lista do que não existir.
- **Matriz insumo-produto.** A URL em `parametros.yml` aponta para a MIP 2015 do IBGE. Se o download falhar, baixe o arquivo das 68 atividades no site do IBGE e coloque o .xls/.xlsx em `dados_externos/mip/`. O leitor procura sozinho a tabela de coeficientes técnicos intersetoriais. Se não encontrar, só a parte de encadeamentos fica de fora e o resto do site sai normal.

## 2. Automação no GitHub (uma vez só)

### 2.1 Repositório e Pages
1. Crie um repositório (por exemplo `jrsvf/portfolio`) e suba esta pasta.
2. Em **Settings > Pages**, em *Source*, escolha **GitHub Actions**.
3. O endereço do site fica `https://jrsvf.github.io/portfolio`. Se usar outro nome de repositório, ajuste `site-url` em `site/_quarto.yml`.

### 2.2 Conta de serviço do Google Cloud (para o robô acessar o BigQuery)
O ideal é uma conta só para o robô, para poder revogar sem afetar seus outros scripts:
1. No console do Google Cloud, com o projeto **analises1** selecionado: **IAM e administrador > Contas de serviço > Criar conta de serviço** (nome: `portfolio-github`).
2. Em **IAM**, dê a ela os papéis **Usuário de jobs do BigQuery** (`roles/bigquery.jobUser`) e **Leitor de dados do BigQuery** (`roles/bigquery.dataViewer`).
3. Na conta criada: **Chaves > Adicionar chave > JSON**. O arquivo baixa no seu computador.
   (Alternativa: usar a conta `analises1@analises1.iam.gserviceaccount.com` que já existe, criando uma chave nova para o GitHub.)
4. No GitHub: **Settings > Secrets and variables > Actions > New repository secret**, nome `GCP_SA_KEY`, e cole o conteúdo inteiro do JSON.
5. Apague o arquivo JSON do computador depois de colar. Nunca suba esse arquivo no repositório (o `.gitignore` já bloqueia `*.json`).

### 2.3 shinyapps.io (opcional, para os painéis se atualizarem junto)
Em shinyapps.io: **Account > Tokens > Show**. Crie os secrets `SHINYAPPS_ACCOUNT` (`jrsvf`), `SHINYAPPS_TOKEN` e `SHINYAPPS_SECRET`. Sem eles o site é publicado normalmente e só a etapa dos painéis é pulada.

### 2.4 Rodar
Vá em **Actions > Atualizar portfólio > Run workflow**. Depois ele roda sozinho todo dia 5.

## Como incluir uma cadeia nova

Copie um bloco em `config/cadeias.yml`, ajuste nome, elos (classes CNAE de 5 dígitos), SH4 e produtos do IBGE, e rode o pipeline. A página da cadeia é criada automaticamente, entra na listagem, no explorador Shiny e no app diag.

## Custos

As consultas leem só as partições de PE (exceto a agregação Brasil por classe da RAIS estabelecimentos, que é leve). Uma execução completa deve ficar bem abaixo de 1 TB, a franquia mensal gratuita do BigQuery. Acompanhe em **BigQuery > Histórico de jobs** na primeira vez.
