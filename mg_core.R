# ===================================================
# mg_core.R - Funciones para cálculo de medias generalizadas
# ===================================================
# Laboratorio de Análisis Distributivo (LAD)
# Por Alberto Carreto Nieto
# Versión 1.0 | Mayo 2026
# ===================================================

library(data.table)

# ===================================================
# Calcular medias generalizadas (versión flexible)
# ===================================================
# dt           : data.table con los datos
# año          : nombre del periodo (ej. "2020_2022")
# alphas       : vector de parámetros alpha
# col_ingreso  : variable de ingreso (por defecto "ictpc")
# ===================================================

calcular_mg_flexible <- function(dt, año, alphas, col_ingreso = "ictpc") {
  
  # Validaciones
  if (is.null(dt)) stop("Error: dt es NULL para ", año)
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
  
  # Verificar columnas requeridas
  if (!col_ingreso %in% names(dt)) stop("No se encuentra columna '", col_ingreso, "' en ", año)
  if (!"factor" %in% names(dt)) stop("No se encuentra columna 'factor' en ", año)
  
  # Extraer y limpiar datos
  ingresos <- as.numeric(dt[[col_ingreso]])
  ponderadores <- as.numeric(dt[["factor"]])
  
  # Filtrar observaciones válidas
  validos <- !is.na(ingresos) & !is.na(ponderadores) & 
    is.finite(ingresos) & is.finite(ponderadores) &
    ingresos > 0 & ponderadores > 0
  
  ingresos <- ingresos[validos]
  ponderadores <- ponderadores[validos]
  
  if (length(ingresos) == 0) {
    warning("No hay observaciones válidas en ", año)
    return(data.table(periodo = NA_character_, base = NA_character_))
  }
  
  # Normalizar ponderadores
  ponderadores <- ponderadores / sum(ponderadores)
  
  # Calcular medias para cada alpha
  resultados <- list()
  for (alpha in alphas) {
    if (abs(alpha) < 1e-10) {
      mg <- exp(sum(ponderadores * log(ingresos)))           # Media geométrica
    } else if (abs(alpha - 1) < 1e-10) {
      mg <- sum(ponderadores * ingresos)                     # Media aritmética
    } else {
      mg <- (sum(ponderadores * (ingresos^alpha)))^(1/alpha) # Media generalizada
    }
    resultados[[paste0("α = ", sprintf("%.4f", alpha))]] <- round(mg, 2)
  }
  
  # Extraer periodo y base
  periodo <- sub("_.*", "", año)
  base <- sub(".*_", "", año)
  
  # Resultado final
  data.table(periodo = periodo, base = base, as.data.table(resultados))
}

# ===================================================
# Función de compatibilidad (usa ictpc por defecto)
# ===================================================

calcular_mg_año <- function(dt, año, alphas) {
  calcular_mg_flexible(dt, año, alphas, col_ingreso = "ictpc")
}

# ===================================================
# Función de prueba
# ===================================================

probar_funcion <- function() {
  cat("=== Prueba de calcular_mg_flexible ===\n")
  if (file.exists("lad_2020_base_2020.rds")) {
    datos <- readRDS("lad_2020_base_2020.rds")
    resultado <- calcular_mg_flexible(datos, "2020_2020", c(0, 1, 2), "ictpc")
    print(resultado)
    cat("\n✓ La función opera correctamente.\n")
  } else {
    cat("No se encontró archivo de prueba\n")
  }
}