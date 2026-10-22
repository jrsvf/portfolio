library(shiny)
library(dplyr)
library(leaflet)
library(plotly)
library(DT)
library(stringr)
library(RColorBrewer)
library(sf)
library(readxl)
library(readr)

load("dados_ambiente.RData")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body, label, select, button {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #2c3e50;
      }
      h4 {
        font-weight: 600;
        color: #3f6791;
        margin-bottom: 15px;
      }
      .stat-box {
        background: #f7f9fc;
        border-radius: 6px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        text-align: center;
      }
      .stat-box .value {
        font-size: 1.6em;
        font-weight: 700;
        color: #2c3e50;
        display: block;
        margin-top: 5px;
      }
      .ranking-panel {
        background-color: #f7f9fc;
        border: 1px solid #d1dbe5;
        border-radius: 6px;
        padding: 20px;
        height: 600px;
        overflow-y: auto;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        max-width: 470px;
      }
      .dataTables_wrapper .dataTables_scrollBody {
        max-height: none !important;
        overflow-y: auto !important;
      }
      .shiny-html-output, .leaflet, .dataTables_wrapper {
        margin-top: 0;
      }
      .leaflet-container {
        box-shadow: none !important;
      }
      .info-box {
        background: #e8f0fe;
        border-left: 6px solid #3f6791;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 4px;
      }
      .info-box h5 {
        color: #2c3e50;
        margin-top: 0;
      }
      .info-box p {
        font-size: 14px;
        color: #34495e;
        line-height: 1.5;
      }
    "))
  ),
  
  titlePanel("Diagnóstico Setorial de Pernambuco"),
  
  tabsetPanel(
    
    # === ABA: HOME ===
    tabPanel("Home",
             fluidRow(
               column(12,
                      h3("Bem-vindo ao Diagnóstico Setorial de Pernambuco"),
                      
                      p("Este dashboard apresenta uma análise detalhada das principais atividades econômicas do estado de Pernambuco, focando em dados de emprego, estabelecimentos e indicadores econômicos."),
                      
                      div(class = "info-box",
                          h5("O que é QL (Quociente Locacional)?"),
                          p("O Quociente Locacional (QL) é um indicador que mede o grau de especialização econômica de um município em uma determinada atividade em relação à média estadual. Valores maiores que 1 indicam que o município possui uma concentração relativa maior que a média, sinalizando especialização nesse setor.")
                      ),
                      
                      div(class = "info-box",
                          h5("O que é PR (Participação Relativa)?"),
                          p("A Participação Relativa (PR) indica a proporção que uma determinada atividade representa no total do município ou região. É útil para identificar a importância relativa de cada segmento na economia local.")
                      ),
                      
                      div(class = "info-box",
                          h5("Sobre os Dados e Indicadores"),
                          p("Os dados utilizados neste diagnóstico foram coletados de fontes oficiais como RAIS e IBGE e abrangem o número de empregos, estabelecimentos e participação dos setores econômicos nos municípios de Pernambuco. As análises permitem identificar as regiões com maior potencial produtivo e especialização econômica.")
                      ),
                      
                      div(class = "info-box",
                          h5("Como Navegar no Dashboard"),
                          tags$ul(
                            tags$li(strong("Análises:"), " filtros por região/setor com gráficos e tabela."),
                            tags$li(strong("Mapa por Município:"), " exibe a especialização econômica e atividades dominantes."),
                            tags$li(strong("Mapa com detalhamento por Atividade:"), " traz indicadores específicos com ranking.")
                          )
                      ),
                      
                      p("Este dashboard foi desenvolvido com o objetivo de contribuir para o entendimento do cenário econômico regional, sendo uma ferramenta útil para pesquisadores, gestores públicos e demais interessados no desenvolvimento de Pernambuco."),
                      
                      tags$hr(),
                      
                      # PERFIL DO AUTOR
                      div(style = "margin-top: 20px;",
                          h4("👤 Sobre o autor"),
                          tags$ul(
                            tags$li(HTML('<strong>GitHub:</strong> <a href="https://github.com/jrsvf" target="_blank">https://github.com/jrsvf</a>')),
                            tags$li(HTML('<strong>LinkedIn:</strong> <a href="https://www.linkedin.com/in/jrsvf" target="_blank">https://www.linkedin.com/in/jrsvf</a>')),
                            tags$li(HTML('<strong>Lattes:</strong> <a href="https://lattes.cnpq.br/1066993955257243" target="_blank">https://lattes.cnpq.br/1066993955257243</a>'))
                          )
                      )
               )
             )
    ),
    
    # === ABA: ANÁLISES INTEGRADAS ===
    tabPanel("Análises",
             fluidRow(
               column(6,
                      selectInput("filtro_regiao", "Selecione a Região:",
                                  choices = c("Todas", sort(unique(dados_apl$regiao))),
                                  selected = "Todas")
               ),
               column(6,
                      selectInput("filtro_setor", "Selecione o Setor:",
                                  choices = c("Todos", sort(unique(dados_apl$setor))),
                                  selected = "Todos")
               )
             ),
             tabsetPanel(
               tabPanel("Top Segmentos por Emprego", plotlyOutput("grafico_emprego")),
               tabPanel("Participação Relativa", plotlyOutput("grafico_pr")),
               tabPanel("Tabela Detalhada", DTOutput("tabela_apl"))
             )
    ),
    
    # === ABA: MAPA POR MUNICÍPIO ===
    tabPanel("Mapa por Município",
             fluidRow(
               column(4,
                      wellPanel(
                        selectInput("filtro_regiao_mapa", "Selecione a Região para o Mapa:",
                                    choices = c("Todas", sort(unique(dados_apl$regiao))),
                                    selected = "Todas"),
                        uiOutput("atividade_mapa_ui"),
                        helpText("Mapa mostra QL (Especificação) por município para a atividade e região selecionadas.")
                      )
               ),
               column(8, leafletOutput("mapa_apl", height = 600))
             )
    ),
    
    # === ABA: MAPA DETALHADO COM RANKING ===
    tabPanel("Mapa com detalhamento por Atividade",
             fluidRow(
               column(12,
                      wellPanel(
                        fluidRow(
                          column(4,
                                 selectInput("var_map", "Variável para exibir no mapa:",
                                             choices = c("QL", "PR", "Emprego", "Estabelecimentos"),
                                             selected = "QL")
                          ),
                          column(4,
                                 selectInput("atividade_map", "Atividade:",
                                             choices = c("Todas", sort(unique(dados_apl_final$atividade))),
                                             selected = "Todas")
                          ),
                          column(4,
                                 selectInput("setor_map", "Setor:",
                                             choices = c("Todos", sort(unique(dados_apl_final$setor))),
                                             selected = "Todos")
                          )
                        )
                      )
               )
             ),
             fluidRow(
               column(7,
                      leafletOutput("mapa_detalhado", height = "500px")
               ),
               column(5,
                      div(class = "ranking-panel",
                          h4("🔝 Ranking por Município"),
                          DTOutput("ranking_municipios")
                      )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # === REATIVO GLOBAL PARA FILTROS ===
  dados_filtrados <- reactive({
    df <- dados_apl
    if (input$filtro_regiao != "Todas") df <- df %>% filter(regiao == input$filtro_regiao)
    if (input$filtro_setor != "Todos") df <- df %>% filter(setor == input$filtro_setor)
    df
  })
  
  # === GRÁFICO: Top Segmentos por Emprego ===
  output$grafico_emprego <- renderPlotly({
    dados_filtrados() %>%
      group_by(atividade) %>%
      summarise(Empregos = sum(Emprego, na.rm = TRUE)) %>%
      arrange(desc(Empregos)) %>%
      head(10) %>%
      plot_ly(
        x = ~reorder(atividade, Empregos), y = ~Empregos,
        type = "bar", marker = list(color = "steelblue")
      ) %>%
      layout(xaxis = list(title = "Segmento"), yaxis = list(title = "Empregos"))
  })
  
  # === GRÁFICO: Participação Relativa ===
  output$grafico_pr <- renderPlotly({
    # Somar PR de vários municípios não tem interpretação; a fatia de cada
    # segmento no emprego do recorte filtrado é o que o gráfico deve mostrar.
    dados_filtrados() %>%
      group_by(atividade) %>%
      summarise(Empregos = sum(Emprego, na.rm = TRUE)) %>%
      arrange(desc(Empregos)) %>%
      head(10) %>%
      plot_ly(labels = ~atividade, values = ~Empregos, type = "pie", textinfo = "label+percent") %>%
      layout(title = "Top 10 segmentos: participação no emprego do recorte (%)")
  })
  
  # === TABELA DETALHADA ===
  output$tabela_apl <- renderDT({
    datatable(
      dados_filtrados() %>%
        select(Município = municipio_nome,
               Região = regiao,
               Setor = setor,
               Segmento = atividade,
               Estabelecimentos = estabelecimentos,
               Empregos = Emprego,
               QL,
               `PR (%)` = PR),
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 10
      ),
      rownames = FALSE
    )
  })
  
  # === UI DINÂMICO PARA ATIVIDADE NO MAPA ===
  output$atividade_mapa_ui <- renderUI({
    if (input$filtro_regiao_mapa == "Todas") {
      atividades <- unique(dados_apl$atividade)
    } else {
      atividades <- dados_apl %>%
        filter(regiao == input$filtro_regiao_mapa) %>%
        pull(atividade) %>%
        unique()
    }
    selectInput("atividade_mapa", "Selecione a Atividade para o Mapa:",
                choices = c("Todas", sort(atividades)),
                selected = "Todas")
  })
  
  # === DADOS FILTRADOS PARA O MAPA MUNICIPAL ===
  dados_mapa_filtrado <- reactive({
    df <- dados_apl %>%
      mutate(municipio_nome = toupper(str_trim(municipio_nome)))
    if (input$filtro_regiao_mapa != "Todas") {
      df <- df %>% filter(regiao == input$filtro_regiao_mapa)
    }
    df
  })
  
  mapa_dados <- reactive({
    df <- dados_mapa_filtrado()
    mapa_pe_mod <- mapa_pe %>% mutate(name_muni = toupper(str_trim(name_muni)))
    
    if (input$atividade_mapa == "Todas") {
      df_dom <- df %>%
        group_by(municipio_nome, atividade) %>%
        summarise(QL = sum(QL, na.rm = TRUE), .groups = "drop") %>%
        group_by(municipio_nome) %>%
        slice_max(QL, n = 1) %>%
        ungroup()
      mapa_pe_mod %>%
        left_join(df_dom, by = c("name_muni" = "municipio_nome"))
    } else {
      df_atividade <- df %>%
        filter(atividade == input$atividade_mapa)
      mapa_pe_mod %>%
        left_join(df_atividade, by = c("name_muni" = "municipio_nome"))
    }
  })
  
  # === MAPA POR MUNICÍPIO ===
  output$mapa_apl <- renderLeaflet({
    df_mapa <- mapa_dados()
    validate(need(nrow(df_mapa) > 0, "Sem dados para exibir."))
    
    if (input$atividade_mapa == "Todas") {
      atividades_ativas <- unique(na.omit(df_mapa$atividade))
      pal <- colorFactor(palette = RColorBrewer::brewer.pal(min(length(atividades_ativas), 8), "Set2"),
                         domain = atividades_ativas)
      leaflet(df_mapa) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(atividade),
          weight = 1, color = "white", fillOpacity = 0.7,
          label = ~paste0(name_muni, "<br>Atividade: ", atividade, "<br>QL: ", round(QL, 2))
        ) %>%
        addLegend("bottomright", pal = pal, values = ~atividade, title = "Atividade Dominante")
    } else {
      pal <- colorNumeric("YlGnBu", domain = df_mapa$QL, na.color = "transparent")
      leaflet(df_mapa) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(QL),
          weight = 1, color = "white", fillOpacity = 0.7,
          label = ~paste0(name_muni, "<br>Empregos: ", Emprego, "<br>QL: ", round(QL, 2))
        ) %>%
        addLegend("bottomright", pal = pal, values = ~QL, title = "Quociente Locacional")
    }
  })
  
  # === DADOS DETALHADOS PARA MAPA DE ATIVIDADE ===
  dados_detalhado <- reactive({
    df <- dados_apl_final %>%
      mutate(
        municipio_nome = toupper(trimws(municipio_nome)),
        atividade = toupper(trimws(atividade)),
        setor = trimws(setor)
      )
    
    if (toupper(input$atividade_map) != "TODAS") {
      df <- df %>% filter(atividade == toupper(input$atividade_map))
    }
    if (input$setor_map != "Todos") {
      df <- df %>% filter(setor == input$setor_map)
    }
    
    df
  })
  
  
  # === MAPA DETALHADO COM VARIÁVEL ===
  output$mapa_detalhado <- renderLeaflet({
    df_mapa <- dados_detalhado()
    validate(need(nrow(df_mapa) > 0, "Sem dados para exibir."))
    
    var_sel <- input$var_map
    coluna_verdadeira <- switch(var_sel,
                                "Emprego" = "Emprego",
                                "Estabelecimentos" = "Estabelecimentos",
                                "QL" = "QL",
                                "PR" = "PR")
    
    valores <- df_mapa[[coluna_verdadeira]]
    
    pal <- if (coluna_verdadeira %in% c("QL", "PR")) {
      if (coluna_verdadeira == "QL") {
        colorBin("Blues", domain = valores, bins = c(0, 1, 400), na.color = "grey")
      } else {
        colorBin("Blues", domain = valores, bins = c(0, 10, 20, 30, 60, 100), na.color = "grey")
      }
    } else {
      colorNumeric("Blues", domain = valores, na.color = "grey")
    }
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      df_mapa$municipio_nome,
      var_sel,
      ifelse(is.na(valores), "Sem dado", round(valores, 2))
    ) %>% lapply(htmltools::HTML)
    
    leaflet(df_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(valores),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(textsize = "14px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = valores, title = var_sel, position = "bottomright")
  })
  
  # === RANKING POR MUNICÍPIO ===
  output$ranking_municipios <- renderDT({
    req(input$var_map)
    df <- dados_detalhado()
    
    coluna_verdadeira <- switch(input$var_map,
                                "Emprego" = "Emprego",
                                "Estabelecimentos" = "Estabelecimentos",
                                "QL" = "QL",
                                "PR" = "PR")
    
    df <- df %>%
      st_drop_geometry() %>%
      mutate(valor = ifelse(is.na(.data[[coluna_verdadeira]]), 0, .data[[coluna_verdadeira]])) %>%
      arrange(desc(valor)) %>%
      mutate(Rank = row_number()) %>%
      select(Rank,
             Município = municipio_nome,
             Atividade = atividade,
             !!input$var_map := valor)
    
    datatable(df,
              options = list(
                dom = 'Bfrtip',
                pageLength = nrow(df),
                paging = FALSE
              ),
              rownames = FALSE
    )
  })
}
shinyApp(ui, server)

