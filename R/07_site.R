# =============================================================================
# Funções usadas pelas páginas do site (gráficos, mapas, cartões) e pelo
# gerador de páginas das cadeias.
# =============================================================================

PALETA <- list(
  principal = "#1F4E79",
  destaque  = "#D9822B",
  neutro    = "#7F8C8D",
  claro     = "#EAF0F6",
  categorias = c("#1F4E79", "#D9822B", "#2E8B57", "#8E44AD", "#C0392B", "#16A085", "#B7950B", "#5D6D7E")
)

# ---------------------------------------------------------------------------
# Gerador de páginas: uma página por cadeia a partir de site/_modelos/cadeia.qmd
# ---------------------------------------------------------------------------
gerar_paginas_cadeias <- function(meta,
                                  modelo = caminho("site", "_modelos", "cadeia.qmd"),
                                  pasta  = caminho("site", "cadeias")) {
  exigir_arquivo(modelo, "site/_modelos/cadeia.qmd faz parte do repositório")
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  txt <- readLines(modelo, encoding = "UTF-8")
  # remove páginas de cadeias que saíram do cadeias.yml
  antigas <- setdiff(list.files(pasta, pattern = "\\.qmd$"), c("index.qmd", paste0(meta$cadeia_id, ".qmd")))
  file.remove(file.path(pasta, antigas))
  arqs <- pmap_chr(meta, function(cadeia_id, cadeia, macrossetor, descricao, nota, ...) {
    out <- txt
    out <- gsub("{{id}}", cadeia_id, out, fixed = TRUE)
    out <- gsub("{{nome}}", cadeia, out, fixed = TRUE)
    out <- gsub("{{macrossetor}}", macrossetor, out, fixed = TRUE)
    out <- gsub("{{descricao}}", gsub('"', "'", descricao), out, fixed = TRUE)
    arq <- file.path(pasta, paste0(cadeia_id, ".qmd"))
    writeLines(out, arq, useBytes = TRUE)
    arq
  })
  unname(arqs)
}

# ---------------------------------------------------------------------------
# Cartões de indicadores (HTML simples, estilizado em site/estilo.scss)
# ---------------------------------------------------------------------------
cartoes <- function(itens) {
  # itens: lista de list(rotulo, valor, nota)
  htmltools::div(class = "cartoes",
    lapply(itens, function(x) htmltools::div(class = "cartao",
      htmltools::div(class = "cartao-valor", x$valor),
      htmltools::div(class = "cartao-rotulo", x$rotulo),
      if (!is.null(x$nota)) htmltools::div(class = "cartao-nota", x$nota))))
}

lista_html <- function(frases, classe = "destaques") {
  htmltools::tags$ul(class = classe, lapply(frases, htmltools::tags$li))
}

# ---------------------------------------------------------------------------
# Gráficos (plotly direto, sem ggplotly, para controle fino e leveza)
# ---------------------------------------------------------------------------
layout_padrao <- function(p, titulo_y = NULL, titulo_x = NULL, legenda = TRUE) {
  plotly::layout(p,
    font = list(family = "Inter, Segoe UI, sans-serif", size = 13, color = "#2C3E50"),
    # título do eixo x vazio por padrão (o ano dispensa rótulo e não briga com a legenda)
    xaxis = list(title = list(text = titulo_x %||% ""), showgrid = FALSE, zeroline = FALSE, automargin = TRUE),
    yaxis = list(title = list(text = titulo_y %||% "", standoff = 8), gridcolor = "#E5E8EB", zeroline = FALSE,
                 separatethousands = TRUE, automargin = TRUE),
    # legenda acima do gráfico, alinhada à esquerda: não cobre eixo nem rótulos
    legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
    showlegend = legenda,
    hovermode = "x unified",
    margin = list(l = 10, r = 10, t = if (legenda) 40 else 10, b = 10),
    separators = ",."
  ) %>% plotly::config(displaylogo = FALSE, locale = "pt-BR",
                       modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

grafico_linhas <- function(df, x, y, cor = NULL, titulo_y = NULL, formato = ",.0f") {
  cores <- if (is.null(cor)) PALETA$principal else PALETA$categorias
  p <- plotly::plot_ly(df, x = df[[x]], y = df[[y]],
                       color = if (!is.null(cor)) df[[cor]] else NULL,
                       colors = cores, type = "scatter", mode = "lines+markers",
                       line = list(width = 2.5), marker = list(size = 5),
                       hovertemplate = paste0("%{y:", formato, "}<extra>%{fullData.name}</extra>"))
  if (is.null(cor)) p <- plotly::style(p, line = list(color = PALETA$principal, width = 2.5),
                                       marker = list(color = PALETA$principal))
  layout_padrao(p, titulo_y, legenda = !is.null(cor))
}

grafico_area_empilhada <- function(df, x, y, cor, titulo_y = NULL) {
  p <- plotly::plot_ly(df, x = df[[x]], y = df[[y]], color = df[[cor]], colors = PALETA$categorias,
                       type = "scatter", mode = "lines", stackgroup = "um",
                       hovertemplate = "%{y:,.0f}<extra>%{fullData.name}</extra>")
  layout_padrao(p, titulo_y)
}

altura_barras <- function(n) max(220, 60 + 34 * n)

grafico_barras_h <- function(df, rotulo, valor, titulo_x = NULL, cor = PALETA$principal, formato = ",.0f",
                             hover = NULL) {
  df$.hover <- if (is.null(hover)) df[[rotulo]] else hover
  df <- df %>% arrange(.data[[valor]])
  p <- plotly::plot_ly(df, y = factor(df[[rotulo]], levels = df[[rotulo]]), x = df[[valor]], height = altura_barras(nrow(df)),
                       type = "bar", orientation = "h", marker = list(color = cor), customdata = df$.hover,
                       hovertemplate = paste0("%{customdata}: %{x:", formato, "}<extra></extra>"))
  layout_padrao(p, legenda = FALSE) %>%
    plotly::layout(hovermode = "closest", xaxis = list(title = list(text = titulo_x %||% ""), gridcolor = "#E5E8EB", automargin = TRUE),
                   yaxis = list(title = list(text = ""), gridcolor = NULL, automargin = TRUE),
                   margin = list(l = 10, r = 10, t = 10, b = 55))
}

# Barras horizontais com uma categoria destacada (ex.: Pernambuco entre os estados)
grafico_barras_destaque <- function(df, rotulo, valor, destaque, titulo_x = NULL, formato = ",.0f") {
  df <- df %>% arrange(.data[[valor]])
  cores <- ifelse(df[[rotulo]] == destaque, PALETA$destaque, "#9FB3C8")
  p <- plotly::plot_ly(df, y = factor(df[[rotulo]], levels = df[[rotulo]]), x = df[[valor]], height = altura_barras(nrow(df)),
                       type = "bar", orientation = "h", marker = list(color = cores),
                       hovertemplate = paste0("%{x:", formato, "}<extra></extra>"))
  layout_padrao(p, legenda = FALSE) %>%
    plotly::layout(hovermode = "closest", xaxis = list(title = list(text = titulo_x %||% ""), gridcolor = "#E5E8EB", automargin = TRUE),
                   yaxis = list(title = list(text = ""), automargin = TRUE),
                   margin = list(l = 10, r = 10, t = 10, b = 55))
}

# Tabela de variações nos cortes de tempo (10, 5, 2 anos e desde 2023)
tabela_janelas <- function(jc) {
  if (nrow(jc) == 0) return(htmltools::p("Sem dados suficientes para as comparações."))
  ordem_ind <- c("Empregos formais (PE)", "Empregos formais (Nordeste sem PE)", "Estabelecimentos (PE)",
                 "Remuneração média real (PE)", "Valor da produção real, IBGE (PE)", "Exportações em US$ (PE)")
  cols <- jc %>% distinct(ordem, rotulo) %>% arrange(ordem) %>% pull(rotulo) %>% unique()
  t <- jc %>%
    # cada fonte tem seu último ano, então o período vai na própria célula
    mutate(txt = paste0(sinal_pct(var_pct), " (", ano_ini, "-", ano_fim, ")")) %>%
    select(indicador, rotulo, txt) %>%
    pivot_wider(names_from = rotulo, values_from = txt, values_fill = "-") %>%
    select(indicador, any_of(cols)) %>%
    mutate(indicador = factor(indicador, levels = ordem_ind)) %>% arrange(indicador) %>%
    rename(Indicador = indicador)
  htmltools::HTML(knitr::kable(t, format = "html", table.attr = "class='table table-sm'"))
}

grafico_saldo_caged <- function(df) {
  p <- plotly::plot_ly(df, x = ~data) %>%
    plotly::add_bars(y = ~saldo, name = "Saldo do mês",
                     marker = list(color = ifelse(df$saldo >= 0, PALETA$principal, PALETA$destaque)),
                     hovertemplate = "%{y:,.0f}<extra>Saldo do mês</extra>") %>%
    plotly::add_lines(y = ~saldo_12m, name = "Acumulado em 12 meses", yaxis = "y2",
                      line = list(color = "#2C3E50", width = 2),
                      hovertemplate = "%{y:,.0f}<extra>12 meses</extra>")
  layout_padrao(p, "Saldo mensal") %>%
    plotly::layout(yaxis2 = list(overlaying = "y", side = "right", title = list(text = "Acumulado 12 meses"),
                                 showgrid = FALSE, automargin = TRUE, separatethousands = TRUE),
                   xaxis = list(title = list(text = "")),
                   shapes = list(list(type = "line", x0 = paste0(CORTES_PADRAO$desde, "-01-01"), x1 = paste0(CORTES_PADRAO$desde, "-01-01"),
                                      y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "#7F8C8D"))),
                   annotations = list(list(x = paste0(CORTES_PADRAO$desde, "-01-01"), y = 1, yref = "paper",
                                           text = paste("Desde", CORTES_PADRAO$desde), showarrow = FALSE,
                                           xanchor = "left", yanchor = "bottom", font = list(size = 11, color = "#7F8C8D"))))
}

grafico_ligacoes <- function(df) {
  # Dispersão BL x FL com quadrantes (setores-chave no canto superior direito)
  # rótulo visível só nos setores destacados; os demais aparecem ao passar o mouse
  df$rotulo <- ifelse(df$na_cadeia, df$grupo_nome, "")
  p <- plotly::plot_ly(df, x = ~bl, y = ~fl, type = "scatter", mode = "markers+text",
                       text = ~rotulo, customdata = ~grupo_nome, textposition = "top center",
                       marker = list(size = 11, color = ifelse(df$na_cadeia, PALETA$destaque, "#BFC9D1"),
                                     line = list(color = "white", width = 1)),
                       textfont = list(size = 10, color = ifelse(df$na_cadeia, "#2C3E50", "#95A5A6")),
                       hovertemplate = "%{customdata}<br>Para trás: %{x:.2f}<br>Para frente: %{y:.2f}<extra></extra>")
  layout_padrao(p, "Ligação para frente", legenda = FALSE) %>%
    plotly::layout(hovermode = "closest",
                   xaxis = list(title = "Ligação para trás", gridcolor = "#E5E8EB"),
                   shapes = list(
                     list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "#7F8C8D")),
                     list(type = "line", y0 = 1, y1 = 1, x0 = 0, x1 = 1, xref = "paper", line = list(dash = "dot", color = "#7F8C8D"))))
}

# ---------------------------------------------------------------------------
# Mapa coroplético (leaflet)
# ---------------------------------------------------------------------------
mapa_municipios <- function(malha, dados, valor, rotulo_valor, municipios, formato = function(x) fmt_num(x, 2),
                            paleta = "Blues", quebras = NULL) {
  m <- malha %>% left_join(dados, by = "id_municipio")
  v <- m[[valor]]
  if (is.null(quebras)) {
    pos <- v[!is.na(v) & v > 0]
    quebras <- unique(signif(stats::quantile(pos, c(0, .2, .4, .6, .8, 1), names = FALSE), 2))
    if (length(quebras) < 3) quebras <- pretty(c(0, max(pos, 1)), n = 4)
    quebras[1] <- min(quebras[1], min(pos, 0)); quebras[length(quebras)] <- max(pos, quebras[length(quebras)])
  }
  pal <- leaflet::colorBin(paleta, domain = v, bins = quebras, na.color = "#F2F3F4")
  rot <- sprintf("<strong>%s</strong><br/>%s: %s", m$name_muni, rotulo_valor,
                 ifelse(is.na(v), "sem vínculo formal", formato(v))) %>% lapply(htmltools::HTML)
  leaflet::leaflet(m, options = leaflet::leafletOptions(zoomSnap = 0.25)) %>%
    leaflet::addPolygons(fillColor = ~pal(v), weight = 0.6, color = "white", fillOpacity = 0.85,
                         label = rot,
                         highlightOptions = leaflet::highlightOptions(weight = 2, color = "#2C3E50", bringToFront = TRUE)) %>%
    leaflet::addLegend("bottomright", pal = pal, values = v[!is.na(v) & v > 0], title = rotulo_valor,
                       labFormat = function(type, cuts, p) {
                         fim <- ifelse(is.infinite(cuts[-1]), "ou mais", paste("a", formato(cuts[-1])))
                         paste(formato(head(cuts, -1)), fim)
                       })
}

tabela <- function(df, ...) {
  DT::datatable(df, rownames = FALSE, extensions = "Buttons",
                options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"), pageLength = 10,
                               language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json")), ...)
}
