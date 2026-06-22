# =============================================================
#  global.R
#  PUNTO DE ENTRADA
#
#  Shiny carga automáticamente, en este orden, los 3 archivos de
#  esta carpeta: global.R -> ui.R -> server.R
#
#  Este archivo solo se encarga de:
#   1. Instalar/cargar los paquetes necesarios
#   2. Cargar la lógica de cálculo (pert_logic.R) para que esté
#      disponible tanto en ui.R como en server.R
#
#  Para correr la app: abrir esta carpeta en RStudio y presionar
#  "Run App" (o ejecutar shiny::runApp() desde la consola estando
#  en esta carpeta).
# =============================================================


library(readxl)
library(visNetwork)
library(shiny)
library(bslib)
library(bsicons)
library(reactable)
library(echarts4r)
library(dplyr)

# Motor de cálculo PERT-CPM (función calcular_pert()) — no cambia con la UI
source("calcular_pert.R")

# ---- Tema visual (paleta consistente con el resto de la app) ----
tema <- bslib::bs_theme(
  version = 5,
  bg = "#F4F6F9",
  fg = "#1B2631",
  primary = "#1E3A5F",
  secondary = "#1F6FBF",
  success = "#00BFA5",
  danger = "#E74C3C",
  warning = "#FDD365",
  base_font = bslib::font_google("Space Grotesk"),
  code_font = bslib::font_google("Space Mono"),
  "navbar-bg" = "#1E3A5F",
  "card-border-radius" = "14px",
  "card-box-shadow" = "0 2px 10px rgba(15,23,42,.06)"
)

# Paleta compartida para los gráficos (ECharts)
PAL_TEAL   <- "#00BFA5"
PAL_BLUE   <- "#1F6FBF"
PAL_NAVY   <- "#1E3A5F"
PAL_RED    <- "#E74C3C"
PAL_GRAY   <- "#90A4B8"
PAL_YELLOW <- "#FDD365"