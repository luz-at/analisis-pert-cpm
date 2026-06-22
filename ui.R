# =============================================================
#  ui.R
# =============================================================

ui <- page_navbar(
  fillable = FALSE,
  fillable_mobile = FALSE,
  lang = "es",
  theme = tema,
  
  window_title = "Análisis PERT-CPM",
  title = tags$span("Análisis PERT-CPM", style = "font-weight:600; color:white;"),
  
  bg = "#1D2C44",
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
      .prob-z{font-family:'Space Mono',monospace;font-size:.92rem;color:#1D2C44;}
      .prob-p{font-family:'Space Mono',monospace;font-size:1.7rem;font-weight:700;color:#00BFA5;margin:.25rem 0;}
      .bslib-value-box { min-height: 100px !important; }
      .bslib-value-box .value-box-showcase { padding: 0.5rem !important; }
      .bslib-value-box .bi { font-size: 1.5rem !important; }
    "))
  ),
  
  # ================= Pestaña: Análisis PERT-CPM =================
  nav_panel(
    title = "Inicio",
    #icon = bs_icon("diagram-3-fill"),
    
    div(
      
      # ---- Encabezado + carga de Excel (logo a la izq, upload a la der) ----
      card(
        class = "upload-card",
        card_body(
          layout_columns(
            col_widths = breakpoints(sm = c(12), md = c(7, 5)),
            
            tags$div(
              tags$p(class = "titulo", "Instrucciones"),
              tags$p(class = "subtitulo",
                     "Carga tu archivo Excel con las actividades del proyecto y 
                     obtén automáticamente los cálculos, la red, el Gantt y el 
                     análisis de probabilidades.")
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
          lg = c(c(2,2,2,2,4))
        ),
        value_box(
          title = "Duración optimista",
          value = textOutput("vb_opt"),
          #showcase = bs_icon("rocket-takeoff-fill"),
          theme = value_box_theme(bg = "#1F6FBF", fg = "white"),
          p(class = "vbox-sub", "Mejor escenario posible")
        ),
        value_box(
          title = "Duración esperada",
          value = textOutput("vb_tpy"),
          #showcase = bs_icon("calendar-check-fill"),
          theme = value_box_theme(bg = "#1E3A5F", fg = "white"),
          p(class = "vbox-sub", "Estimación PERT del proyecto")
        ),
        value_box(
          title = "Duración pesimista",
          value = textOutput("vb_pes"),
          #showcase = bs_icon("exclamation-triangle-fill"),
          theme = value_box_theme(bg = "#002261", fg = "white"),
          p(class = "vbox-sub", "Peor escenario posible")
        ),
        value_box(
          title = "Σ Varianza (ruta crítica)",
          value = textOutput("vb_var"),
          #showcase = bs_icon("graph-up"),
          theme = value_box_theme(bg = "#90A4B8", fg = "white"),
          p(class = "vbox-sub", textOutput("vb_sigma", inline = TRUE))
        ),
        value_box(
          title = "Ruta crítica",
          value = textOutput("vb_ruta"),
          #showcase = bs_icon("signpost-split-fill"),
          theme = value_box_theme(bg = "#0D47A1", fg = "white"),
          p(class = "vbox-sub", textOutput("vb_ruta_n", inline = TRUE))
        )

      ),
      
      # ---- Sub-pestañas ----
      navset_card_pill(
        
        # 1. Cálculos ----------------------------------------------------
        nav_panel(
          title = "Tiempos y varianza",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Duración esperada y varianza por actividad"),
              tags$p(class = "subtitulo",
                     "Esta tabla muestra la duración esperada (t̄) y la varianza (σ²) de cada actividad,
                      calculadas a partir de los tres tiempos estimados: optimista (a), más probable (m) 
                      y pesimista (b).")
            )),
            card_body(
              reactableOutput("tbl_calc", height = "400px"),
              tags$p(
                class = "subtitulo",
                "Calculado con la distribución beta: t̄ = (a + 4m + b) / 6  y  σ² = ((b − a) / 6)²"
              )
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
                     "Esta tabla presenta los resultados del algoritmo CPM: tiempos de inicio y fin tempranos (ES, EF), 
                      tardíos (LS, LF), holgura de cada actividad y si pertenece a la ruta crítica. 
                      Las filas resaltadas indican actividades con holgura cero, es decir, sin margen de retraso.")
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
                     "Representación gráfica de las actividades en el tiempo según sus tiempos tempranos.")
            )),
            card_body(
              echarts4rOutput("gantt", height = "400px"),
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
          title = "Red del proceso",
          card(
            card_header(tags$div(
              tags$p(class = "titulo", "Red del proceso (PERT-CPM)"),
              tags$p(class = "subtitulo",
                     "Diagrama de nodos que representa la secuencia y dependencias entre actividades. 
                      Los nodos en rojo forman la ruta crítica.")
            )),
            card_body(
              echarts4rOutput("red", height = "400px"),
              tags$p(
                class = "subtitulo",
                "Pasa el cursor sobre cada nodo para ver ES, EF, LS, LF y holgura. 
                Puedes arrastrar los nodos para reorganizar la vista."
              )
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
                     "Análisis probabilístico basado en la varianza acumulada de la ruta crítica. 
                      Ajusta los valores de TS para recalcular en tiempo real la probabilidad de finalizar 
                      el proyecto en un plazo determinado.")
            )),
            card_body(
              layout_columns(
                col_widths = breakpoints(sm = c(12), md = c(6,6), lg = c(3,3,3,3)),
                div(
                  numericInput("ts_opt", "Probabilidad de terminar antes de TS", value = NA, width = "100%"),
                ),
                div(
                  numericInput("ts_pes", "Probabilidad de terminar después de TS", value = NA, width = "100%"),
                ),
                div(
                  sliderInput("prob3", "Nivel de confianza deseado", min = 0.5, max = 0.99, value = 0.90, step = 0.01, width = "100%")
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