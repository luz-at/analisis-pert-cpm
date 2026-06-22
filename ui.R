# =============================================================
#  ui.R
#  INTERFAZ — estructura tipo "Tablero de actividad del agente"
#  (page_navbar + cards con título/subtítulo + navset_card_pill)
#
#  Este archivo no calcula nada, solo define lo visual.
#  La lógica vive en pert_logic.R y la conexión en server.R.
# =============================================================

ui <- page_navbar(
  fillable = FALSE,
  fillable_mobile = FALSE,
  lang = "es",
  theme = tema,
  
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    tags$div(
      style = "width:34px;height:34px;border-radius:8px;
               background:linear-gradient(135deg,#00BFA5,#1F6FBF);
               display:flex;align-items:center;justify-content:center;
               color:#fff;font-weight:700;font-size:15px;flex-shrink:0;",
      "P"
    ),
    tags$span("Análisis PERT-CPM", style = "font-weight:600;")
  ),
  
  bg = "#1E3A5F",
  inverse = TRUE,
  
  header = tagList(
    tags$style(HTML("
      .titulo{font-weight:600;font-size:1.02rem;margin-bottom:2px;color:#1B2631;}
      .subtitulo{font-size:.82rem;color:#6B7C93;margin-bottom:0;font-weight:400;}
      .upload-card .form-control, .upload-card .btn{font-size:.85rem;}
      .vbox-sub{font-size:.72rem;opacity:.85;margin-top:2px;}
      .legend-dot{width:14px;height:14px;border-radius:4px;flex-shrink:0;}
      .legend-item{display:flex;align-items:center;gap:8px;}
      .net-tooltip-title{font-weight:700;font-size:.95rem;}
      .net-tooltip-row{display:flex;justify-content:space-between;gap:14px;font-size:.82rem;margin-top:3px;}
      .net-tooltip-label{color:#90A4B8;}
      .prob-card{background:#F9FAFB;border-radius:12px;padding:1rem 1.2rem;border:1px solid #E5E9EF;height:100%;}
      .prob-card h6{font-size:.74rem;text-transform:uppercase;letter-spacing:.04em;color:#6B7C93;margin-bottom:.5rem;font-weight:600;}
      .prob-z{font-family:'Space Mono',monospace;font-size:.92rem;color:#1E3A5F;}
      .prob-p{font-family:'Space Mono',monospace;font-size:1.7rem;font-weight:700;color:#00BFA5;margin:.25rem 0;}
    "))
  ),
  
  # ================= Pestaña: Análisis PERT-CPM =================
  nav_panel(
    title = "Análisis",
    #icon = bs_icon("diagram-3-fill"),
    
    div(
      
      # ---- Encabezado + carga de Excel (logo a la izq, upload a la der) ----
      card(
        class = "upload-card",
        card_body(
          layout_columns(
            col_widths = breakpoints(sm = c(12), md = c(7, 5)),
            
            tags$div(
              tags$p(class = "titulo", "Estudio diagnóstico"),
              tags$p(class = "subtitulo",
                     "Desde la programación de la cita hasta la entrega de resultados al paciente. ",
                     "Carga el Excel con las actividades para generar el análisis PERT-CPM completo.")
            ),
            
            div(
              style = "display:flex; align-items:flex-end; gap:10px;",
              div(style="flex:1",
                  fileInput("file", "Cargar Excel (.xlsx)", accept = c(".xlsx", ".xls"), width = "100%")
              )
            )
          )
        )
      ),
      
      # ---- 5 Value boxes ----
      layout_columns(
        col_widths = breakpoints(
          sm = c(12),
          md = c(6, 6),
          lg = c(c(2,2,2,3,3))
        ),
        value_box(
          title = "Duración optimista (Σa ruta crítica)",
          value = textOutput("vb_opt"),
          showcase = bs_icon("rocket-takeoff-fill"),
          theme = value_box_theme(bg = "#00BFA5", fg = "white"),
          p(class = "vbox-sub", "Mejor escenario posible")
        ),
        value_box(
          title = "Duración esperada (TPy)",
          value = textOutput("vb_tpy"),
          showcase = bs_icon("calendar-check-fill"),
          theme = value_box_theme(bg = "#1E3A5F", fg = "white"),
          p(class = "vbox-sub", "Estimación PERT del proyecto")
        ),
        value_box(
          title = "Duración pesimista (Σb ruta crítica)",
          value = textOutput("vb_pes"),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = value_box_theme(bg = "#E74C3C", fg = "white"),
          p(class = "vbox-sub", "Peor escenario posible")
        ),
        value_box(
          title = "Ruta crítica",
          value = textOutput("vb_ruta"),
          showcase = bs_icon("signpost-split-fill"),
          theme = value_box_theme(bg = "#1F6FBF", fg = "white"),
          p(class = "vbox-sub", textOutput("vb_ruta_n", inline = TRUE))
        ),
        value_box(
          title = "Σ Varianza (ruta crítica)",
          value = textOutput("vb_var"),
          showcase = bs_icon("graph-up"),
          theme = value_box_theme(bg = "#002261", fg = "white"),
          p(class = "vbox-sub", textOutput("vb_sigma", inline = TRUE))
        )
      ),
      
      # ---- Sub-pestañas ----
      navset_card_pill(
        
        # 1. Cálculos ----------------------------------------------------
        nav_panel(
          title = "Cálculos (t̄, σ²)",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Duración esperada y varianza por actividad"),
              tags$p(class = "subtitulo",
                     "Calculado con la distribución beta: t̄ = (a + 4m + b) / 6   y   σ² = ((b − a) / 6)²")
            )),
            card_body(
              reactableOutput("tbl_calc", height = "400px")
            ) 
          )
        ),
        
        # 2. CPM -----------------------------------------------------------
        nav_panel(
          title = "CPM y Holguras",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Tiempos tempranos/tardíos y ruta crítica"),
              tags$p(class = "subtitulo",
                     "ES/EF = recorrido hacia adelante. LS/LF = recorrido hacia atrás. Holgura = LS − ES. Filas en rojo = actividades críticas.")
            )),
            card_body(
              reactableOutput("tbl_cpm", height = "400px")
            )
          )
        ),
        
        # 3. Gantt -----------------------------------------------------------
        nav_panel(
          title = "Diagrama de Gantt",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Diagrama de Gantt"),
              tags$p(class = "subtitulo",
                     "Barras según tiempos tempranos (ES → EF). Rojo = actividad crítica, azul = no crítica.")
            )),
            card_body(
              echarts4rOutput("gantt", height = "560px"),
              tags$div(
                style = "display:flex; gap:18px; margin-top:10px; flex-wrap:wrap;",
                tags$div(class = "legend-item",
                         tags$div(class = "legend-dot", style = "background:#E74C3C;"),
                         tags$span(style="font-size:12px;color:#444;", tags$strong("Crítica"), " — holgura = 0")),
                tags$div(class = "legend-item",
                         tags$div(class = "legend-dot", style = "background:#1F6FBF;"),
                         tags$span(style="font-size:12px;color:#444;", tags$strong("No crítica"), " — tiene holgura disponible"))
              )
            ) 
          )
        ),
        
        # 4. Red ------------------------------------------------------------
        nav_panel(
          title = "Red del Proyecto",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Red del proyecto (PERT-CPM)"),
              tags$p(class = "subtitulo",
                     "Nodos en rojo = ruta crítica. Pasa el mouse sobre un nodo para ver ES, EF, LS, LF y holgura.")
            )),
            card_body(
              echarts4rOutput("red", height = "600px")
            )
          )
        ),
        
        # 5. Probabilidades --------------------------------------------------
        nav_panel(
          title = "Probabilidades",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Análisis de probabilidades"),
              tags$p(class = "subtitulo",
                     "Estandarización con z = (TS − TPy) / √(Σσ²RC). Ajusta los valores de TS para recalcular en tiempo real.")
            )),
            card_body(
              layout_columns(
                col_widths = breakpoints(sm = c(12), md = c(6,6), lg = c(3,3,3,3)),
                div(
                  numericInput("ts_opt", "TS para P1 (tiempo optimista)", value = NA, width = "100%"),
                ),
                div(
                  numericInput("ts_pes", "TS para P2 (tiempo pesimista)", value = NA, width = "100%"),
                ),
                div(
                  sliderInput("prob3", "Probabilidad para P3", min = 0.5, max = 0.99, value = 0.90, step = 0.01, width = "100%")
                ),
                div(
                  numericInput("ts_libre", "Consulta libre — P(T ≤ TS)", value = NA, width = "100%")
                )
              ),
              hr(),
              uiOutput("prob_cards"),
              br(),
              echarts4rOutput("norm_curve", height = "260px")
            )
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(
    tags$span(class = "navbar-text text-light", style="font-size:.78rem;",
              "Investigación de Operaciones II")
  )
)