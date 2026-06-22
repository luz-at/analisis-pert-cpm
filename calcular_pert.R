# =============================================================
#  pert_logic.R
#  MOTOR DE CÁLCULO PERT-CPM (independiente de la interfaz)
#
#  Solo recibe una tabla de actividades y devuelve los resultados
#  numéricos: t, varianza, ES/EF/LS/LF, holgura y ruta crítica.
#
#  Se puede probar solo, desde la consola de R, sin abrir la app:
#     source("pert_logic.R")
#     datos <- readxl::read_excel("datos_prueba.xlsx")
#     resultado <- calcular_pert(datos)
#     resultado$TPy
# =============================================================

INICIO <- "INICIO"
FIN <- "FIN"

# ---- Convierte el texto de la columna Precedentes en un vector ----
# Ej: "H,E,G" -> c("H","E","G")  |  "-" o vacío -> character(0)
parse_prec <- function(x) {
  x <- as.character(x)
  if (is.na(x) || trimws(x) %in% c("", "-")) return(character(0))
  p <- unlist(strsplit(x, "[,; ]+"))
  toupper(trimws(p[p != ""]))
}

# ---- Función principal: recibe un data.frame y devuelve TODO el análisis ----
# Columnas requeridas en df: Codigo, Precedentes, a, m, b  (Actividad es opcional)
calcular_pert <- function(df) {
  names(df) <- trimws(names(df))
  req_cols <- c("Codigo", "Precedentes", "a", "m", "b")
  if (!all(req_cols %in% names(df)))
    stop(paste("El Excel debe tener las columnas:", paste(req_cols, collapse = ", ")))
  
  df$Codigo <- toupper(trimws(as.character(df$Codigo)))
  df <- df[df$Codigo != "" & !is.na(df$Codigo), ]
  if (!"Actividad" %in% names(df)) df$Actividad <- df$Codigo
  df$a <- as.numeric(df$a); df$m <- as.numeric(df$m); df$b <- as.numeric(df$b)
  if (any(is.na(df$a) | is.na(df$m) | is.na(df$b)))
    stop("Faltan valores numéricos en a, m o b para alguna actividad.")
  
  # ---- Paso 2: duración esperada y varianza (distribución beta) ----
  df$t   <- (df$a + 4 * df$m + df$b) / 6
  df$var <- ((df$b - df$a) / 6)^2
  
  preds <- setNames(lapply(df$Precedentes, parse_prec), df$Codigo)
  
  todos <- unlist(preds)
  faltantes <- setdiff(todos, df$Codigo)
  if (length(faltantes))
    stop(paste("Precedentes no definidos como actividad:", paste(unique(faltantes), collapse = ", ")))
  
  # Nodos ficticios INICIO y FIN
  nodes <- c(INICIO, df$Codigo, FIN)
  preds[[INICIO]] <- character(0)
  sin_pred <- df$Codigo[sapply(df$Codigo, function(n) length(preds[[n]]) == 0)]
  for (n in sin_pred) preds[[n]] <- INICIO
  con_suc <- unique(unlist(preds))
  sin_suc <- setdiff(df$Codigo, con_suc)
  preds[[FIN]] <- if (length(sin_suc)) sin_suc else df$Codigo
  
  dur <- setNames(c(0, df$t, 0), nodes)
  
  succ <- setNames(vector("list", length(nodes)), nodes)
  for (n in nodes) for (p in preds[[n]]) succ[[p]] <- c(succ[[p]], n)
  
  # Orden topológico (Kahn) + detección de ciclos
  indeg <- setNames(sapply(nodes, function(n) length(preds[[n]])), nodes)
  q <- nodes[indeg == 0]; topo <- character(0)
  while (length(q)) {
    n <- q[1]; q <- q[-1]; topo <- c(topo, n)
    for (s in succ[[n]]) { indeg[s] <- indeg[s] - 1; if (indeg[s] == 0) q <- c(q, s) }
  }
  if (length(topo) < length(nodes)) stop("Hay un ciclo en las precedencias. Revisa la columna Precedentes.")
  
  # ---- Paso 3a: recorrido hacia adelante (ES, EF) ----
  ES <- setNames(rep(0, length(nodes)), nodes); EF <- ES
  for (n in topo) {
    if (length(preds[[n]])) ES[n] <- max(EF[preds[[n]]])
    EF[n] <- ES[n] + dur[n]
  }
  TPy <- EF[FIN]
  
  # ---- Paso 3b: recorrido hacia atrás (LS, LF) ----
  LF <- setNames(rep(TPy, length(nodes)), nodes); LS <- LF
  for (n in rev(topo)) {
    if (length(succ[[n]])) LF[n] <- min(LS[succ[[n]]])
    LS[n] <- LF[n] - dur[n]
  }
  H <- LS - ES
  crit <- abs(H) < 1e-9
  
  # ---- Ruta crítica continua INICIO -> FIN ----
  path <- INICIO; cur <- INICIO
  while (cur != FIN) {
    nx <- succ[[cur]]
    cand <- nx[crit[nx] & abs(ES[nx] - EF[cur]) < 1e-9]
    if (!length(cand)) cand <- nx[crit[nx]]
    if (!length(cand)) break
    cur <- cand[1]; path <- c(path, cur)
  }
  ruta_real <- path[!path %in% c(INICIO, FIN)]
  var_rc <- sum(df$var[df$Codigo %in% ruta_real])
  
  # ---- Tabla de resultados (una fila por actividad) ----
  H_clean <- ifelse(abs(H) < 1e-9, 0, H)  # evita -0 por error de redondeo de punto flotante
  res <- data.frame(
    Codigo = df$Codigo, Actividad = df$Actividad,
    Precedentes = sapply(df$Codigo, function(n) {
      p <- setdiff(preds[[n]], INICIO); if (length(p)) paste(p, collapse = ",") else "-"
    }),
    a = df$a, m = df$m, b = df$b,
    t = round(df$t, 3), Varianza = round(df$var, 3),
    ES = round(ES[df$Codigo], 2), EF = round(EF[df$Codigo], 2),
    LS = round(LS[df$Codigo], 2), LF = round(LF[df$Codigo], 2),
    Holgura = round(H_clean[df$Codigo], 2),
    Critica = ifelse(crit[df$Codigo], "Sí", "No"),
    stringsAsFactors = FALSE
  )
  
  # ---- Devuelve TODO lo que necesita la interfaz para graficar ----
  list(
    tabla = res,
    TPy = as.numeric(TPy),
    var_rc = var_rc,
    sigma_rc = sqrt(var_rc),
    ruta = paste(c(INICIO, ruta_real, FIN), collapse = " → "),
    ruta_real = ruta_real,
    nodes = nodes, preds = preds, succ = succ, crit = crit, dur = dur,
    a_rc = sum(df$a[df$Codigo %in% ruta_real]),
    b_rc = sum(df$b[df$Codigo %in% ruta_real])
  )
}