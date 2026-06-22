# =============================================================
#  server.R
#  Conecta el motor de cálculo (pert_logic.R) con la interfaz (ui.R)
# =============================================================

# library(shiny)
# library(readxl)
# library(reactable)
# library(echarts4r)
# library(dplyr)

server <- function(input, output, session) {
  
  # ---- Leer Excel y calcular el modelo PERT-CPM ----
  modelo <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    calcular_pert(as.data.frame(df))
  })
  
  observeEvent(modelo(), {
    updateNumericInput(session, "ts_opt", value = round(modelo()$a_rc, 2))
    updateNumericInput(session, "ts_pes", value = round(modelo()$b_rc, 2))
    updateNumericInput(session, "ts_libre", value = round(modelo()$TPy, 2))
  })
  
  # =================== VALUE BOXES ===================
  output$vb_opt <- renderText({ req(modelo()); sprintf("%.2f", modelo()$TPy - modelo()$var_rc) })
  output$vb_tpy <- renderText({ req(modelo()); sprintf("%.2f", modelo()$TPy) })
  output$vb_pes <- renderText({ req(modelo()); sprintf("%.2f", modelo()$TPy + modelo()$var_rc) })
  output$vb_ruta <- renderText({ req(modelo()); paste(modelo()$ruta_real, collapse = "-") })
  output$vb_ruta_n <- renderText({ req(modelo()); sprintf("%d actividades críticas", length(modelo()$ruta_real)) })
  output$vb_var <- renderText({ req(modelo()); sprintf("%.3f", modelo()$var_rc) })
  output$vb_sigma <- renderText({ req(modelo()); sprintf("σ = %.3f", modelo()$sigma_rc) })
  
  # =================== TABLA 1: CÁLCULOS (Reactable) ===================
  output$tbl_calc <- renderReactable({
    m <- modelo()$tabla
    reactable(
      m[, c("Codigo","Actividad","Precedentes","a","m","b","t","Varianza")],
      columns = list(
        Codigo = colDef(name = "Cód.", width = 70, align = "center",
                        style = list(fontWeight = 700)),
        Actividad = colDef(name = "Actividad", minWidth = 220),
        Precedentes = colDef(name = "Prec.", width = 90, align = "center"),
        a = colDef(name = "a", width = 75, align = "center", format = colFormat(digits = 2)),
        m = colDef(name = "m", width = 75, align = "center", format = colFormat(digits = 2)),
        b = colDef(name = "b", width = 75, align = "center", format = colFormat(digits = 2)),
        t = colDef(name = "t̄", width = 90, align = "center",
                   format = colFormat(digits = 2),
                   style = list(color = "#1F6FBF", fontWeight = 600)),
        Varianza = colDef(name = "σ²", width = 90, align = "center", format = colFormat(digits = 2))
      ),
      defaultPageSize = 20, highlight = TRUE, striped = TRUE, bordered = FALSE,
      compact = TRUE,
      theme = reactableTheme(
        headerStyle = list(background = "#F4F6F9", fontWeight = 600, fontSize = "12px",
                           textTransform = "uppercase", color = "#6B7C93"),
        borderColor = "#EEF1F4"
      )
    )
  })
  
  # =================== TABLA 2: CPM (Reactable) ===================
  output$tbl_cpm <- renderReactable({
    m <- modelo()$tabla
    reactable(
      m[, c("Codigo","Actividad","ES","EF","LS","LF","Holgura","Critica")],
      columns = list(
        Codigo = colDef(name = "Cód.", width = 70, align = "center", style = list(fontWeight = 700)),
        Actividad = colDef(name = "Actividad", minWidth = 200),
        ES = colDef(name = "ES", width = 80, align = "center", format = colFormat(digits = 2)),
        EF = colDef(name = "EF", width = 80, align = "center", format = colFormat(digits = 2)),
        LS = colDef(name = "LS", width = 80, align = "center", format = colFormat(digits = 2)),
        LF = colDef(name = "LF", width = 80, align = "center", format = colFormat(digits = 2)),
        Holgura = colDef(name = "Holgura", width = 90, align = "center", format = colFormat(digits = 2)),
        Critica = colDef(
          name = "Crítica", width = 100, align = "center",
          cell = function(value) {
            color <- if (value == "Sí") "#E74C3C" else "#90A4B8"
            bg <- if (value == "Sí") "rgba(231,76,60,.12)" else "rgba(144,164,184,.12)"
            htmltools::span(
              style = sprintf("color:%s;background:%s;padding:2px 10px;border-radius:6px;font-weight:600;font-size:12px;", color, bg),
              value
            )
          }
        )
      ),
      defaultPageSize = 20, highlight = TRUE, bordered = FALSE, compact = TRUE,
      rowStyle = function(index) {
        if (m$Critica[index] == "Sí") list(background = "rgba(231,76,60,.04)")
      },
      theme = reactableTheme(
        headerStyle = list(background = "#F4F6F9", fontWeight = 600, fontSize = "12px",
                           textTransform = "uppercase", color = "#6B7C93"),
        borderColor = "#EEF1F4"
      )
    )
  })
  
  # =================== GANTT (echarts4r) ===================
  output$gantt <- renderEcharts4r({
    m <- modelo()$tabla
    m <- m[order(-m$ES), ]
    m$cat <- paste0(m$Codigo, " · ", substr(m$Actividad, 1, 28))
    m$cat <- factor(m$cat, levels = unique(m$cat))
    
    dfg <- data.frame(
      cat = as.character(m$cat),
      base = m$ES,
      dur = m$EF - m$ES,
      ES = m$ES, EF = m$EF, Holgura = m$Holgura, t = m$t,
      critica = m$Critica == "Sí",
      stringsAsFactors = FALSE
    )
    
    # Tooltip por actividad: ES, EF, t y holgura
    tip_map <- setNames(
      sprintf("ES: %.2f &nbsp; EF: %.2f<br/>t&#772;: %.2f &nbsp; Holgura: %.2f",
              dfg$ES, dfg$EF, dfg$t, dfg$Holgura),
      dfg$cat
    )
    
    p <- dfg |>
      e_charts(cat) |>
      e_bar(base, stack = "g", name = "_base",
            itemStyle = list(color = "transparent"),
            tooltip = list(show = FALSE), legend = FALSE) |>
      e_bar(dur, stack = "g", name = "Duración", legend = FALSE,
            itemStyle = list(borderRadius = 4)) |>
      e_flip_coords() |>
      e_x_axis(name = "Tiempo", nameLocation = "middle", nameGap = 28,
               axisLine = list(lineStyle = list(color = "#90A4B8")),
               splitLine = list(lineStyle = list(color = "#EEF1F4"))) |>
      e_y_axis(axisLine = list(show = FALSE), axisTick = list(show = FALSE)) |>
      e_legend(show = FALSE) |>
      e_grid(left = "26%", right = "5%") |>
      e_tooltip(
        formatter = htmlwidgets::JS(sprintf(
          "function(params){
             var tips = %s;
             return '<b>' + params.name + '</b><br/>' + (tips[params.name] || '');
           }",
          jsonlite::toJSON(as.list(tip_map), auto_unbox = TRUE)
        ))
      )
    
    # Colorea cada barra de la serie "Duración" (2da serie) según criticidad de la actividad
    for (i in seq_len(nrow(dfg))) {
      color_i <- if (dfg$critica[i]) "#E74C3C" else "#1F6FBF"
      p$x$opts$series[[2]]$data[[i]] <- list(
        value = dfg$dur[i],
        itemStyle = list(color = color_i, borderRadius = 4)
      )
    }
    p
  })
  
  # =================== RED DEL PROYECTO (echarts4r graph) ===================
  output$red <- renderEcharts4r({
    M <- modelo()
    tt <- M$tabla
    rownames(tt) <- tt$Codigo
    
    # ---- Nivel topológico de cada nodo (a partir de preds) ----
    get_level <- function(n, memo) {
      if (!is.null(memo[[n]])) return(memo[[n]])
      ps <- M$preds[[n]]
      if (length(ps) == 0) { memo[[n]] <- 0; return(0) }
      l <- max(sapply(ps, get_level, memo = memo)) + 1
      memo[[n]] <- l
      l
    }
    memo <- new.env()
    level <- setNames(sapply(M$nodes, get_level, memo = memo), M$nodes)
    
    # ---- Posición: x = nivel (columna), y = orden dentro del nivel (centrado) ----
    nodes_by_lvl <- split(names(level), level)
    pos_x <- setNames(numeric(length(M$nodes)), M$nodes)
    pos_y <- setNames(numeric(length(M$nodes)), M$nodes)
    for (l in names(nodes_by_lvl)) {
      ns <- nodes_by_lvl[[l]]
      k <- length(ns)
      for (i in seq_along(ns)) {
        pos_x[ns[i]] <- as.numeric(l) * 220
        pos_y[ns[i]] <- (i - (k + 1) / 2) * 110
      }
    }
    
    # ---- Tooltip: solo ES, EF, LS, LF, Holgura ----
    tooltip_for <- function(n) {
      if (n %in% c("INICIO", "FIN")) return(sprintf("<div class='net-tooltip-title'>%s</div>", n))
      r <- tt[n, ]
      sprintf(
        "<div class='net-tooltip-title'>%s</div>
         <div class='net-tooltip-row'><span class='net-tooltip-label'>ES</span><b>%.2f</b></div>
         <div class='net-tooltip-row'><span class='net-tooltip-label'>EF</span><b>%.2f</b></div>
         <div class='net-tooltip-row'><span class='net-tooltip-label'>LS</span><b>%.2f</b></div>
         <div class='net-tooltip-row'><span class='net-tooltip-label'>LF</span><b>%.2f</b></div>
         <div class='net-tooltip-row'><span class='net-tooltip-label'>Holgura</span><b style='color:%s'>%.2f</b></div>",
        r$Codigo, r$ES, r$EF, r$LS, r$LF,
        if (r$Holgura < 1e-6) "#E74C3C" else "#1B2631", r$Holgura
      )
    }
    tips <- setNames(sapply(M$nodes, tooltip_for), M$nodes)
    
    # ---- data.frame de nodos (columnas: name, x, y, value, size, category) ----
    nodes_df <- data.frame(
      name = M$nodes,
      x = unname(pos_x[M$nodes]),
      y = unname(pos_y[M$nodes]),
      value = 1,
      size = ifelse(M$nodes %in% c("INICIO", "FIN"), 60, 60),
      category = ifelse(M$nodes %in% c("INICIO", "FIN"), "Extremo",
                        ifelse(M$crit[M$nodes], "Crítica", "No crítica")),
      stringsAsFactors = FALSE
    )
    
    edges_df <- do.call(rbind, lapply(M$nodes, function(n) {
      if (length(M$preds[[n]]))
        data.frame(source = M$preds[[n]], target = n, stringsAsFactors = FALSE)
    }))
    
    e_charts() |>
      e_graph(layout = "none", roam = TRUE, focusNodeAdjacency = TRUE,
              draggable = TRUE,
              label = list(show = TRUE, fontSize = 12, fontWeight = 700, color = "#fff"),
              edgeSymbol = c("none", "arrow"), edgeSymbolSize = c(0, 8),
              lineStyle = list(color = "#000000", curveness = 0, width = 1.6)) |>
      e_graph_nodes(nodes_df, names = name, value = value, size = size,
                    category = category, xpos = x, ypos = y) |>
      e_graph_edges(edges_df, source = source, target = target) |>
      e_color(color = c("#000000", "#E74C3C", "#1F6FBF")) |>
      e_tooltip(
        formatter = htmlwidgets::JS(sprintf(
          "function(params){
             if(params.dataType === 'node'){
               var tips = %s;
               return tips[params.data.name] || params.data.name;
             }
             return '';
           }",
          jsonlite::toJSON(as.list(tips), auto_unbox = TRUE)
        ))
      ) |>
      e_legend(show = FALSE)
  })
  
  # =================== PROBABILIDADES: cards horizontales ===================
  output$prob_cards <- renderUI({
    M <- modelo()
    s <- M$sigma_rc; TPy <- M$TPy
    if (s == 0) return(div(class = "alert alert-warning", "La varianza de la ruta crítica es 0; no se puede estandarizar."))
    
    z1 <- (input$ts_opt - TPy) / s
    z2 <- (input$ts_pes - TPy) / s
    z3 <- qnorm(input$prob3)
    ts3 <- TPy + z3 * s
    zL <- (input$ts_libre - TPy) / s
    
    mk_card <- function(titulo, formula_txt, z_txt, p_val, nota, color = "#00BFA5") {
      div(
        class = "prob-card",
        tags$h6(titulo),
        div(class = "prob-z", formula_txt),
        div(class = "prob-z", z_txt),
        div(class = "prob-p", style = sprintf("color:%s", color), p_val),
        div(style = "font-size:.78rem;color:#6B7C93;", nota)
      )
    }
    
    layout_columns(
      col_widths = breakpoints(sm = c(12), md = c(6,6), lg = c(3,3,3,3)),
      mk_card(
        "P1 · Antes del tiempo optimista",
        sprintf("z = (%.2f − %.2f) / %.3f", input$ts_opt, TPy, s),
        sprintf("z = %.4f", z1),
        sprintf("%.2f%%", 100 * pnorm(z1)),
        sprintf("P(T ≤ %.2f)", input$ts_opt),
        "#00BFA5"
      ),
      mk_card(
        "P2 · Después del tiempo pesimista",
        sprintf("z = (%.2f − %.2f) / %.3f", input$ts_pes, TPy, s),
        sprintf("z = %.4f", z2),
        sprintf("%.2f%%", 100 * (1 - pnorm(z2))),
        sprintf("P(T > %.2f)", input$ts_pes),
        "#E74C3C"
      ),
      mk_card(
        sprintf("P3 · Duración al %.0f%%", 100 * input$prob3),
        sprintf("TS = %.2f + %.4f × %.3f", TPy, z3, s),
        sprintf("z(%.0f%%) = %.4f", 100*input$prob3, z3),
        sprintf("%.2f", ts3),
        "Unidades de tiempo estimadas",
        "#FDD365"
      ),
      mk_card(
        "Consulta libre",
        sprintf("z = (%.2f − %.2f) / %.3f", input$ts_libre, TPy, s),
        sprintf("z = %.4f", zL),
        sprintf("%.2f%%", 100 * pnorm(zL)),
        sprintf("P(T ≤ %.2f)", input$ts_libre),
        "#1F6FBF"
      )
    )
  })
  
  # ---- Curva normal con marcas (echarts4r) ----
  output$norm_curve <- renderEcharts4r({
    M <- modelo()
    s <- M$sigma_rc; TPy <- M$TPy
    req(s > 0)
    
    z1 <- (input$ts_opt - TPy) / s
    z2 <- (input$ts_pes - TPy) / s
    z3 <- qnorm(input$prob3)
    zL <- (input$ts_libre - TPy) / s
    
    zs <- seq(-4, 4, length.out = 400)
    phi <- dnorm(zs)
    df <- data.frame(z = zs, phi = phi)
    
    marks <- data.frame(
      z = c(z1, z2, z3, zL),
      label = c("P1", "P2", "P3", "Libre"),
      color = c("#00BFA5", "#E74C3C", "#FDD365", "#1F6FBF")
    )
    
    p <- df |>
      e_charts(z) |>
      e_line(phi, smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#1E3A5F", width = 2),
             areaStyle = list(opacity = 0.08, color = "#1E3A5F"),
             legend = FALSE) |>
      e_x_axis(name = "z", min = -4, max = 4,
               axisLine = list(lineStyle = list(color = "#90A4B8")),
               splitLine = list(show = FALSE)) |>
      e_y_axis(show = FALSE) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = FALSE) |>
      e_grid(top = 30, bottom = 30, left = 20, right = 20, containLabel = TRUE)
    
    for (i in seq_len(nrow(marks))) {
      p <- p |> e_mark_line(
        data = list(xAxis = marks$z[i]),
        title = marks$label[i],
        lineStyle = list(color = marks$color[i], type = "dashed"),
        label = list(formatter = marks$label[i], color = marks$color[i])
      )
    }
    p
  })
}