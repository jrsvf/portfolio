# =============================================================================
# Explorador de cadeias produtivas de Pernambuco
# Lê as bases geradas pelo pipeline (pasta dados/ ao lado deste arquivo).
# Para rodar localmente: copie as .rds de ../../dados para ./dados e
# rode shiny::runApp().
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(DT)
library(sf)

ler <- function(n) {
  arq <- file.path("dados", paste0(n, ".rds"))
  if (!file.exists(arq)) arq <- file.path("..", "..", "dados", paste0(n, ".rds"))
  if (!file.exists(arq)) stop("Base não encontrada: ", n, ". Rode o pipeline (targets::tar_make()).")
  readRDS(arq)
}

cad           <- ler("cad")
municipios    <- ler("municipios")
malha         <- ler("malha")
rd            <- ler("rd")
locais_cadeia <- ler("locais_cadeia")
locais_elo    <- ler("locais_elo")
atualizado_em <- ler("atualizado_em")

municipios <- municipios %>% left_join(rd, by = "id_municipio") %>%
  mutate(rd = coalesce(rd, "Não informada"))

base <- bind_rows(
  locais_cadeia %>% rename(cadeia_id = atividade) %>% mutate(elo = "Todos os elos"),
  locais_elo
) %>% left_join(municipios %>% select(id_municipio, municipio, rd), by = "id_municipio")

cadeias <- setNames(cad$meta$cadeia_id, cad$meta$cadeia)
anos <- sort(unique(base$ano))
INDICADORES <- c("Vínculos formais" = "emprego", "Quociente locacional (QL)" = "ql",
                 "Participação no estado (%)" = "pr", "Estabelecimentos" = "estabelecimentos")

fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
COR <- "#1F4E79"

ui <- page_sidebar(
  title = "Cadeias produtivas de Pernambuco",
  theme = bs_theme(version = 5, primary = COR, base_font = font_google("Inter", local = FALSE)),
  sidebar = sidebar(
    width = 300,
    selectInput("cadeia", "Cadeia", cadeias),
    uiOutput("ui_elo"),
    selectInput("indicador", "Indicador", INDICADORES),
    sliderInput("ano", "Ano", min = min(anos), max = max(anos), value = max(anos), step = 1, sep = ""),
    selectInput("rd", "Região de Desenvolvimento", c("Todas", sort(unique(municipios$rd)))),
    numericInput("minimo", "Vínculos mínimos para o QL", 10, min = 0, step = 5),
    hr(),
    downloadButton("baixar", "Baixar dados (CSV)"),
    tags$small(class = "text-muted", paste("Fonte: RAIS. Atualizado em", format(atualizado_em, "%d/%m/%Y")))
  ),
  layout_columns(
    fill = FALSE,
    value_box("Vínculos formais", textOutput("vb_emp"), theme = "primary"),
    value_box("Municípios especializados (QL > 1)", textOutput("vb_esp")),
    value_box("Maior polo", textOutput("vb_polo"))
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE, card_header("Mapa"), leafletOutput("mapa", height = 520)),
    card(full_screen = TRUE, card_header("Ranking"), DTOutput("ranking"))
  ),
  card(full_screen = TRUE, card_header(textOutput("titulo_serie")), plotlyOutput("serie", height = 340))
)

server <- function(input, output, session) {
  mun_sel <- reactiveVal(NULL)

  output$ui_elo <- renderUI({
    e <- cad$elos %>% filter(cadeia_id == input$cadeia) %>% distinct(elo_ordem, elo) %>% arrange(elo_ordem)
    selectInput("elo", "Elo", c("Todos os elos", e$elo))
  })

  filtrado <- reactive({
    req(input$elo)
    d <- base %>% filter(cadeia_id == input$cadeia, elo == input$elo)
    if (input$rd != "Todas") d <- d %>% filter(rd == input$rd)
    d
  })

  no_ano <- reactive({
    filtrado() %>% filter(ano == input$ano) %>%
      mutate(ql = ifelse(emprego >= input$minimo, ql, NA), pr = 100 * pr)
  })

  output$vb_emp  <- renderText(fmt(sum(no_ano()$emprego, na.rm = TRUE)))
  output$vb_esp  <- renderText(sum(no_ano()$ql > 1, na.rm = TRUE))
  output$vb_polo <- renderText({
    d <- no_ano() %>% arrange(desc(emprego)); if (nrow(d) == 0) "-" else d$municipio[1]
  })

  output$mapa <- renderLeaflet({
    d <- no_ano(); v_nome <- input$indicador
    m <- malha %>% left_join(d %>% select(id_municipio, valor = all_of(v_nome)), by = "id_municipio")
    if (input$rd != "Todas") m <- m %>% filter(id_municipio %in% municipios$id_municipio[municipios$rd == input$rd])
    v <- m$valor; pos <- v[!is.na(v) & v > 0]
    validate(need(length(pos) > 0, "Sem vínculos formais para esta seleção."))
    pal <- if (v_nome == "ql") colorBin("YlGnBu", v, bins = c(0, 0.5, 1, 2, 5, Inf), na.color = "#F2F3F4")
           else colorBin("Blues", v, bins = unique(signif(quantile(pos, c(0, .2, .4, .6, .8, 1)), 2)), na.color = "#F2F3F4")
    rot <- sprintf("<strong>%s</strong><br/>%s: %s", m$name_muni, names(INDICADORES)[INDICADORES == v_nome],
                   ifelse(is.na(v), "sem dado", fmt(v, ifelse(v_nome %in% c("ql", "pr"), 2, 0)))) %>% lapply(HTML)
    leaflet(m) %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(layerId = ~id_municipio, fillColor = ~pal(valor), weight = 0.6, color = "white",
                  fillOpacity = 0.85, label = rot,
                  highlightOptions = highlightOptions(weight = 2, color = "#2C3E50", bringToFront = TRUE)) %>%
      addLegend("bottomright", pal = pal, values = v, title = names(INDICADORES)[INDICADORES == v_nome])
  })

  observeEvent(input$mapa_shape_click, mun_sel(input$mapa_shape_click$id))
  observeEvent(list(input$cadeia, input$elo, input$rd), mun_sel(NULL), ignoreInit = TRUE)

  output$ranking <- renderDT({
    d <- no_ano() %>% filter(emprego > 0) %>% arrange(desc(.data[[input$indicador]])) %>%
      transmute(Município = municipio, RD = rd, Vínculos = emprego, QL = round(ql, 2),
                `% estado` = round(pr, 1), Estab. = estabelecimentos)
    datatable(d, rownames = FALSE, selection = "single",
              options = list(pageLength = 12, dom = "ftp", scrollY = 430, paging = FALSE))
  })

  observeEvent(input$ranking_rows_selected, {
    d <- no_ano() %>% filter(emprego > 0) %>% arrange(desc(.data[[input$indicador]]))
    mun_sel(d$id_municipio[input$ranking_rows_selected])
  })

  output$titulo_serie <- renderText({
    if (is.null(mun_sel())) "Série histórica: estado (clique em um município no mapa ou no ranking)"
    else paste("Série histórica:", municipios$municipio[municipios$id_municipio == mun_sel()])
  })

  output$serie <- renderPlotly({
    v_nome <- input$indicador
    # QL e participação não fazem sentido para o estado inteiro: mostra o emprego
    if (is.null(mun_sel()) && v_nome %in% c("ql", "pr")) v_nome <- "emprego"
    d <- if (is.null(mun_sel())) {
      filtrado() %>% group_by(ano) %>%
        summarise(emprego = sum(emprego), estabelecimentos = sum(estabelecimentos),
                  pr = 100, ql = NA_real_, .groups = "drop")
    } else {
      filtrado() %>% filter(id_municipio == mun_sel()) %>% mutate(pr = 100 * pr)
    }
    validate(need(nrow(d) > 0, "Sem dados."))
    plot_ly(d, x = ~ano, y = d[[v_nome]], type = "scatter", mode = "lines+markers",
            line = list(color = COR, width = 2.5), marker = list(color = COR)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = names(INDICADORES)[INDICADORES == v_nome]),
             separators = ",.") %>%
      config(displaylogo = FALSE)
  })

  output$baixar <- downloadHandler(
    filename = function() paste0("cadeia_", input$cadeia, ".csv"),
    content = function(f) write.csv2(filtrado() %>% select(-any_of("geom")), f, row.names = FALSE)
  )
}

shinyApp(ui, server)
