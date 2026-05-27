# ===================================================
# Laboratorio de Análisis Distributivo (LAD)
# Alberto Carreto Nieto
# Coyoacán, Ciudad de México
# Mayo de 2026
# ===================================================

cat("\014")

# ===================================================
# 1. Detección automática de directorio de trabajo
# ===================================================

obtener_directorio_app <- function() {
  if (interactive()) {
    return(getwd())
  } else {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      return(dirname(sub("--file=", "", file_arg)))
    }
  }
  return(getwd())
}

app_dir <- obtener_directorio_app()
setwd(app_dir)

message("Directorio de la app: ", getwd())

# ===================================================
# 2. Paquetes
# ===================================================

required_packages <- c("shiny", "data.table", "ggplot2", "sortable", "shinyjs", "readxl")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, quiet = TRUE, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# ===================================================
# 3. Funciones y datos
# ===================================================

if (file.exists("mg_core.R")) {
  source("mg_core.R")
} else {
  stop("No se encuentra mg_core.R")
}

# Cargar bases ENIGH
archivos_bases <- c(
  "data/lad_2020_base_2022.rds",
  "data/lad_2022_base_2022.rds",
  "data/lad_2024_base_2022.rds",
  "data/lad_2020_base_2020.rds",
  "data/lad_2024_base_2024.rds"
)

nombres_bases <- c("2020_2022", "2022_2022", "2024_2022", "2020_2020", "2024_2024")

bases <- list()
for (i in seq_along(archivos_bases)) {
  if (file.exists(archivos_bases[i])) {
    bases[[nombres_bases[i]]] <- readRDS(archivos_bases[i])
    message("✓ Cargada: ", archivos_bases[i])
  }
}

opciones_bases <- c(
  "2020_2020" = "2020 · Precios corrientes",
  "2020_2022" = "2020 · Precios constantes 2022",
  "2022_2022" = "2022 · Precios corrientes",
  "2024_2024" = "2024 · Precios corrientes",
  "2024_2022" = "2024 · Precios constantes 2022"
)

opciones_bases <- opciones_bases[names(opciones_bases) %in% names(bases)]

message("Total de bases cargadas: ", length(bases))

# ===================================================
# 4. Interfaz de usuario (UI)
# ===================================================

ui <- fluidPage(
  
  # CSS y JavaScript
  tags$head(
    tags$style(HTML("
      body { margin: 0; padding: 0; width: 100%; height: 100%; overflow: auto; }
      .container-fluid { width: 100% !important; max-width: 100% !important; padding: 0 !important; margin: 0 !important; }
      .pantalla-interna { position: relative; width: 100%; min-height: 100vh; background: #f5f5f7; overflow-y: auto; padding-top: 70px; padding-bottom: 40px; }
      .tarjeta-base { background-color: white; border: 1.5px solid #FF8C00; border-radius: 20px; padding: 28px 32px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); }
      .header-fijo { position: fixed; top: 0; left: 0; right: 0; background: white; z-index: 100; padding: 12px 30px; height: 55px; border-bottom: 1px solid #e0e0e0; }
      .btn-regresar { position: absolute; top: 12px; left: 30px; }
      .header-lad { text-align: left; margin-left: 120px; padding-top: 3px; font-size: 16px; font-weight: 600; color: #004c8c; }
      .titulo-principal { font-size: 26px; font-weight: 700; color: #004c8c; text-align: center; margin-bottom: 28px; }
      .subtitulo-seccion { font-size: 18px; font-weight: 600; color: #004c8c; margin-top: 20px; margin-bottom: 12px; }
      .btn-azul { background-color: #004c8c; color: white; border: none; border-radius: 25px; padding: 8px 24px; font-size: 14px; font-weight: 500; width: 100%; display: block; }
      .btn-gris { background-color: #f0f0f0; color: #333; border: 1px solid #ccc; border-radius: 25px; padding: 8px 24px; font-size: 14px; width: 100%; }
      .btn-verde { background-color: #28a745; color: white; border: none; border-radius: 25px; padding: 8px 24px; font-size: 14px; width: 100%; }
      .bases-cargadas-list { background-color: #f8f9fa; border-radius: 10px; padding: 0; margin-top: 10px; border: 1px solid #dee2e6; }
      .base-item { padding: 10px 12px; border-bottom: 1px solid #e9ecef; display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px; }
      .base-item:last-child { border-bottom: none; }
      .base-info { flex: 1; text-align: left; padding-left: 0; }
      .base-nombre { font-weight: bold; font-size: 14px; }
      .base-variables { font-size: 12px; color: #6c757d; margin-top: 2px; }
      .btn-eliminar { background-color: #dc3545; color: white; border: none; padding: 4px 12px; border-radius: 5px; cursor: pointer; font-size: 12px; flex-shrink: 0; }
    ")),
    tags$script(HTML("document.documentElement.requestFullscreen();"))
  ),
  
  shinyjs::useShinyjs(),
  
  # ===================================================
  # 4.1 Panel de bienvenida (primer frame)
  # ===================================================
  
  div(id = "bienvenidaPanel", style = "background-color: white; width: 100%; min-height: 100vh; overflow-y: auto;",
      div(style = "text-align: center; padding-top: 30px;", img(src = "Logo.png", height = "150px", alt = "LAD")),
      div(style = "max-width: 1000px; margin: 20px auto 50px auto; padding: 0 30px;",
          div(style = "text-align: center;",
              h2(style = "color: #004c8c; margin-bottom: 20px; text-align: left;", "¡Una cordial bienvenida!"),
              p(style = "text-align: justify; font-size: 16px; line-height: 1.6;", "El paquete computacional ", em("Laboratorio de Análisis Distributivo"), " (LAD) permite el cálculo de diversas medidas para el análisis distributivo en encuestas de hogares. Está diseñado principalmente para la formación de estudiantes de licenciatura y el desarrollo de actividades de investigación básica."),
              p(style = "text-align: justify; font-size: 16px; line-height: 1.6; margin-top: 15px;", "En esta versión 1.0, LAD estima únicamente el valor de las medias generalizadas (MG). En futuras versiones, se incorporarán otras medidas distributivas, tanto contemporáneas como clásicas, incluyendo el coeficiente de Gini."),
              p(style = "text-align: justify; font-size: 16px; line-height: 1.6; margin-top: 15px;", "El programa opera con bases precargadas de la ENIGH 2020-2024 mediante distintas configuraciones. Este paquete permite el cálculo de las MG con base en el artículo de Foster y Székely (2008) y Hermansen, Ruiz y Causa (2016)."),
              p(style = "text-align: justify; font-size: 16px; line-height: 1.6; margin-top: 15px;", "Se sugiere leer la documentación de forma completa antes de proseguir con los cálculos. Si desea utilizar el código R de forma directa, para mayor flexibilidad puede visitar el siguiente link: ", tags$a("https://github.com/LAD-investigacion", href = "https://github.com/LAD-investigacion", target = "_blank", style = "color: #004c8c; text-decoration: underline;")),
              p(style = "text-align: center; font-size: 12px; color: #666; margin-top: 20px;", "Registro DOI: (próximamente)"),
              div(style = "text-align: center; margin-top: 40px;", actionButton("btn_siguiente", "CONTINUAR", style = "background-color: #004c8c; color: white; border: none; padding: 10px 30px; border-radius: 25px; font-size: 16px;")),
              div(style = "display: flex; justify-content: center; gap: 20px; margin-top: 25px; margin-bottom: 35px;",
                  tags$a(href = "Manual_usuario_LAD.pdf", target = "_blank", tags$button("Manual de usuario", style = "background-color: white; color: #004c8c; border: 2px solid #004c8c; border-radius: 30px; padding: 10px 24px; font-size: 16px; font-weight: 600; cursor: pointer;")),
                  tags$a(href = "Nota_tecnica_LAD.pdf", target = "_blank", tags$button("Nota técnica", style = "background-color: white; color: #004c8c; border: 2px solid #004c8c; border-radius: 30px; padding: 10px 24px; font-size: 16px; font-weight: 600; cursor: pointer;"))
              )
          )
      )
  ),
  
  # ===================================================
  # 4.2 Panel de módulos (segundo frame)
  # ===================================================
  
  div(id = "modulosPanel", class = "pantalla-base", style = "display: none;",
      div(class = "header-fijo",
          div(class = "btn-regresar", actionButton("volver_inicio", label = tagList(icon("arrow-left"), "Regresar"), style = "background-color: transparent; color: #004c8c; border: 1px solid #004c8c; border-radius: 20px; padding: 5px 16px; font-size: 13px;")),
          div(class = "header-lad", img(src = "Logo_2.png", height = "30px", style = "margin-right: 10px;"))
      ),
      div(style = "max-width: 1000px; margin: 100px auto; padding: 0 30px;",
          div(class = "tarjeta-base",
              h2(style = "color: #004c8c; text-align: center; margin-bottom: 30px;", "Módulos disponibles"),
              div(style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap;",
                  div(style = "border: 2px solid #FF8C00; border-radius: 15px; padding: 20px; width: 200px; text-align: center; background-color: white;",
                      div(style = "font-size: 32px; font-weight: bold; color: #004c8c;", "MG"),
                      div(style = "font-size: 14px; font-weight: bold; margin: 10px 0;", "Medias generalizadas"),
                      div(style = "font-size: 12px; color: green;", "✓ Disponible"),
                      actionButton("btn_modulo_mg", "Ingresar", style = "background-color: #004c8c; color: white; border: none; padding: 6px 20px; border-radius: 20px; margin-top: 10px; font-size: 12px;")
                  ),
                  div(style = "border: 2px solid #ccc; border-radius: 15px; padding: 20px; width: 200px; text-align: center; background-color: #f5f5f5; opacity: 0.95;",
                      div(style = "font-size: 32px; font-weight: bold; color: #999;", "IID"),
                      div(style = "font-size: 14px; font-weight: bold; margin: 10px 0; color: #666;", "Curvas IID y FGT"),
                      div(style = "font-size: 12px; color: #999;", "⏳ Próximamente")
                  ),
                  div(style = "border: 2px solid #ccc; border-radius: 15px; padding: 20px; width: 200px; text-align: center; background-color: #f5f5f5; opacity: 0.95;",
                      div(style = "font-size: 32px; font-weight: bold; color: #999;", "SEN"),
                      div(style = "font-size: 14px; font-weight: bold; margin: 10px 0; color: #666;", "Índice de Sen"),
                      div(style = "font-size: 12px; color: #999;", "⏳ Próximamente")
                  ),
                  div(style = "border: 2px solid #ccc; border-radius: 15px; padding: 20px; width: 200px; text-align: center; background-color: #f5f5f5; opacity: 0.95;",
                      div(style = "font-size: 32px; font-weight: bold; color: #999;", "SST"),
                      div(style = "font-size: 14px; font-weight: bold; margin: 10px 0; color: #666;", "Sen-Shorrocks-Thon"),
                      div(style = "font-size: 12px; color: #999;", "⏳ Próximamente")
                  )
              )
          )
      )
  ),
  
  # ===================================================
  # 4.3 Módulo MG (tercer frame)
  # ===================================================
  
  div(id = "welcomePanel", class = "pantalla-interna", style = "display: none; background-color: #FFFFFF; min-height: 100vh;",
      div(class = "header-fijo",
          div(class = "btn-regresar", actionButton("volver_modulos_mg", label = tagList(icon("arrow-left"), "Regresar"), style = "background-color: transparent; color: #004c8c; border: 1px solid #004c8c; border-radius: 20px; padding: 5px 16px; font-size: 13px;"))
      ),
      div(style = "max-width: 1200px; margin: 80px auto 0 auto; padding: 20px 30px;",
          div(style = "position: absolute; top: 70px; left: 85px; z-index: 10;", img(src = "Logo.png", height = "90px", alt = "LAD")),
          div(style = "margin-top: -80px;", div(style = "font-size: 35px; font-weight: bold; text-align: center; color: #004c8c;", "Cálculo de medias generalizadas")),
          div(style = "position: absolute; top: 180px; right: 85px; z-index: 10;", actionButton("btn_conceptos", label = tagList(icon("book"), "Conceptos básicos"), style = "background-color: white; color: #004c8c; border: 1px solid #004c8c; border-radius: 25px; padding: 6px 18px; font-size: 15px; font-weight: 600;")),
          div(style = "display: flex; justify-content: center; gap: 70px; margin: 40px 0 0px 0; flex-wrap: wrap;",
              div(style = "border: 2px solid #FF8C00; border-radius: 20px; padding: 20px 30px; width: 260px; text-align: center; background-color: white;",
                  div(style = "font-family: 'Arial Black', sans-serif; font-size: 36px; font-weight: 900; color: #004c8c; margin-bottom: 10px;", "ENIGH"),
                  div(style = "width: 30px; height: 1px; background-color: #ccc; margin: 15px auto;"),
                  div(style = "font-size: 13px; color: #333; line-height: 1.4;", "Encuesta Nacional de Ingresos"),
                  div(style = "font-size: 13px; color: #333; margin-bottom: 15px;", "y Gastos de los Hogares"),
                  actionButton("btn_enigh", "Seleccionar", style = "background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 25px; padding: 6px 20px; color: #004c8c; font-size: 13px;")
              ),
              div(style = "border: 2px solid #FF8C00; border-radius: 20px; padding: 20px 30px; width: 260px; text-align: center; background-color: white;",
                  div(style = "font-size: 42px; margin-bottom: 10px;", "📁"),
                  div(style = "font-size: 18px; font-weight: bold; margin-bottom: 10px; color: #004c8c;", "Datos propios"),
                  div(style = "width: 30px; height: 1px; background-color: #ccc; margin: 15px auto;"),
                  div(style = "font-size: 13px; color: #555;", ".csv, .rds, .txt, .xlsx"),
                  div(style = "font-size: 13px; color: #555; margin-bottom: 15px;", "Cargue su propia base de datos"),
                  actionButton("btn_propios", "Seleccionar", style = "background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 25px; padding: 6px 20px; color: #004c8c; font-size: 13px;")
              )
          ),
          div(style = "text-align: center; margin: 30px 0 20px 0;", actionButton("btn_continuar", "CONTINUAR", style = "background-color: #004c8c; border: none; padding: 8px 40px; font-size: 20px; border-radius: 55px; color: white; font-weight: bold;")),
          div(style = "text-align: center; font-size: 11px; margin-top: -7px; color: #999;", "Elaboración: ACN"),
          div(style = "text-align: center; font-size: 11px; margin-top: 0px; color: #999;", "Versión 1.0 | Mayo 2026")
      )
  ),
  
  # ===================================================
  # 4.4 Panel conceptos básicos
  # ===================================================
  
  div(id = "conceptosPanel", class = "pantalla-interna", style = "display: none;",
      div(class = "header-fijo",
          div(class = "btn-regresar", actionButton("volver_conceptos", label = tagList(icon("arrow-left"), "Regresar"), style = "background-color: transparent; color: #004c8c; border: 1px solid #004c8c; border-radius: 20px; padding: 5px 16px; font-size: 13px;")),
          div(class = "header-lad", img(src = "Logo_2.png", height = "30px", style = "margin-right: 10px;"))
      ),
      div(style = "max-width: 1200px; margin: 0 auto; padding: 0 30px;",
          div(class = "tarjeta-base",
              div(style = "text-align: right; margin-bottom: -15px;", img(src = "Pastel.png", height = "80px", alt = "Pastel de zanahoria")),
              withMathJax(),
              h2(style = "color: #004c8c; font-weight: bold; text-align: center; margin-top: 0; margin-bottom: 20px;", "Las medias generalizadas (MG) y el asombroso caso del pastel de zanahoria"),
              p(style = "text-align: center; font-style: italic; color: #666; margin-top: -5px; margin-bottom: 30px;", "(pero eso te lo cuento al final)"),
              p("Las MG son una herramienta estadística utilizada para resumir un conjunto de datos. Su principal característica es que permiten otorgar distinta importancia a los valores bajos y altos dependiendo del valor del parámetro α (alfa, primera letra del alfabeto griego)."),
              p("Aunque su fórmula matemática puede parecer compleja al principio, su aplicación es relativamente sencilla cuando la sustitución de valores se realiza paso a paso y, por supuesto, ¡con un poco de paciencia!"),
              div(style = "text-align: center; font-size: 28px; margin: 30px 0; color: #004c8c; font-weight: bold;", "\\[ MG= \\left( \\frac{1}{n} \\sum_{i=1}^{n}x_i^{\\alpha} \\right)^{\\frac{1}{\\alpha}} \\]"),
              p("La letra griega Σ (sigma) representa el símbolo matemático de la suma de varios términos. Esta fórmula funciona para prácticamente todos los valores de α, excepto cuando α = 0. Este caso particular se explicará más adelante."),
              
              # Tabla resumen de parámetros α
              h3(class = "subtitulo-seccion", "Resumen de parámetros α"),
              p("La siguiente tabla resume los principales valores del parámetro α y el tipo de media que representan:"),
              div(style = "overflow-x: auto; margin: 30px auto; width: 92%;",
                  tags$table(style = "width: 100%; table-layout: fixed; border-collapse: collapse; background-color: white; border-radius: 10px; overflow: hidden; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                             tags$thead(
                               tags$tr(
                                 tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: center; background-color: #004c8c; color: white;", "α"),
                                 tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: center; background-color: #004c8c; color: white;", "Nombre"),
                                 tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: center; background-color: #004c8c; color: white;", "Fórmula"),
                                 tags$th(style = "border: 1px solid #ddd; padding: 10px; text-align: center; background-color: #004c8c; color: white;", "Sensibilidad")
                               )
                             ),
                             tags$tbody(
                               tags$tr(tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "\\(\\alpha = -1\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Media armónica"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\\(MG = \\left(\\frac{1}{n} \\sum x_i^{-1}\\right)^{-1}\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Muy sensible a valores bajos")),
                               tags$tr(tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "\\(\\alpha = 0\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Media geométrica"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\\(MG = \\left(\\prod x_i\\right)^{1/n}\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Sensible a valores bajos")),
                               tags$tr(tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "\\(\\alpha = 1\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Media aritmética"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\\(MG = \\frac{1}{n}\\sum x_i\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Sensibilidad neutra")),
                               tags$tr(tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "\\(\\alpha = 2\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Media euclidiana"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\\(MG = \\left(\\frac{1}{n}\\sum x_i^2\\right)^{1/2}\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Sensible a valores altos")),
                               tags$tr(tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "\\(\\alpha \\to -\\infty\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Mínima"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\\(MG = \\min(x_i)\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Muy sensible a valores bajos")),
                               tags$tr(tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "\\(\\alpha \\to +\\infty\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Máxima"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "\\(MG = \\max(x_i)\\)"), tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Muy sensible a valores altos")
                               )
                             )
                  )
              ),
              p(style = "margin-top: 15px; font-size: 14px;", "El parámetro α permite controlar la sensibilidad de la media generalizada: ", strong("α menores"), " otorgan mayor peso a los valores bajos, mientras que ", strong("α mayores"), " otorgan mayor peso a los valores altos de la distribución."),
              
              # Ejemplos
              h3(class = "subtitulo-seccion", "Cuando α = 1"),
              p("Por ejemplo, cuando α = 1, la media generalizada es equivalente a la media aritmética, es decir, al promedio tradicional del conjunto de datos observados."),
              p("Imaginemos ahora el siguiente conjunto de datos o vector del cual queremos calcular la media generalizada con α = 1:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c;", "{2, 4, 8, 10}"),
              p("En economía y estadística, cuando trabajamos con un conjunto ordenado de datos, solemos llamarlo vector. En este caso el vector tiene cuatro observaciones, por lo que:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c; font-weight: bold;", "\\[ n = 4 \\]"),
              p("Esto significa que en la fórmula de la media generalizada sustituiremos:"),
              tags$ul(tags$li("\\(\\alpha = 1\\)"), tags$li("\\(n = 4\\)")),
              p("Entonces, obtenemos:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG= \\left( \\frac{1}{4} \\sum_{i=1}^{4}x_i^1 \\right)^{\\frac{1}{1}} \\]"),
              p("Esta expresión puede simplificarse de la siguiente manera:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG= \\left( \\frac{1}{4} \\sum_{i=1}^{4}x_i^1 \\right)^1 = \\frac{1}{4} \\sum_{i=1}^{4}x_i^1 \\]"),
              p("Esto ocurre porque:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c; font-weight: bold;", "\\[ \\frac{1}{1}=1 \\]"),
              p("y cualquier expresión elevada a la potencia 1 es igual a sí misma. Por esta razón, el exponente puede eliminarse para simplificar la expresión."),
              p("Con esta última expresión, ahora sustituimos los valores del vector dentro del operador suma:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG= \\frac{1}{4} (2^1+4^1+8^1+10^1) \\]"),
              p("Como cualquier número elevado a la potencia 1 es igual al mismo número, obtenemos:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG= \\frac{1}{4} (2+4+8+10) \\]"),
              p("Luego realizamos la suma de los valores:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG= \\frac{1}{4}(24) \\]"),
              p("Por lo tanto, el resultado final es:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c; font-weight: bold;", "\\[ MG=6 \\]"),
              p("Esto significa que la media generalizada del conjunto de datos, cuando α = 1, es igual a 6. En este caso, el resultado coincide exactamente con el promedio o media aritmética."),
              
              h3(class = "subtitulo-seccion", "Cuando α = 0 (Media Geométrica)"),
              p("Ahora pasemos al caso particular en el que α = 0, también conocido como media geométrica. En esta situación, la fórmula es:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c; font-weight: bold;", "\\[ MG = \\left( \\prod_{i=1}^{n} x_i \\right)^{1/n} \\]"),
              p("La fórmula anterior no puede derivarse directamente de la expresión general cuando α = 0, debido a que el exponente \\(1/\\alpha\\) implicaría una división entre cero. Por ello, se define por separado como el límite cuando α tiende a 0, lo que da lugar a la media geométrica."),
              p("En este caso, Π (letra pi mayúscula del alfabeto griego) representa el símbolo matemático de la multiplicación de varios términos."),
              p("Si utilizamos nuevamente el vector del ejercicio anterior:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c;", "{2, 4, 8, 10}"),
              p("Entonces obtenemos que:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c; font-weight: bold;", "\\[ n = 4 \\]"),
              p("Y sustituimos esta información en la fórmula, entonces:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG = \\left( \\prod_{i=1}^{4} x_i \\right)^{1/4} \\]"),
              p("Y ahora sustituimos los valores del vector. Entonces obtenemos:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG = (2 \\cdot 4 \\cdot 8 \\cdot 10)^{1/4} \\]"),
              p("Al realizar la multiplicación de los valores, obtenemos:"),
              div(style = "text-align: center; font-size: 22px; margin: 25px 0; color: #004c8c;", "\\[ MG = (640)^{1/4} \\]"),
              p("Finalmente, calculamos la raíz cuarta de 640:"),
              div(style = "text-align: center; font-size: 28px; margin: 25px 0; color: #004c8c; font-weight: bold;", "\\[ MG \\approx 5.03 \\]"),
              p("Por lo tanto, cuando α = 0 (la media geométrica), la media generalizada del conjunto de datos es aproximadamente igual a 5.03."),
              
              h3(class = "subtitulo-seccion", "Comparación entre α = 1 y α = 0"),
              p("Ahora comparemos estos resultados con el obtenido anteriormente para α = 1, es decir, la media aritmética. Mientras que esta media fue igual a 6 unidades, la media geométrica resultó igual a 5.03."),
              p("Imaginemos que este conjunto de datos representa un vector de ingresos, es decir, cuánto ganan las personas. En ese caso, el promedio tradicional del ingreso sería igual a 6 unidades monetarias."),
              p("Sin embargo, cuando reducimos el valor del parámetro α a cero, la media generalizada se vuelve relativamente más sensible a los ingresos más bajos del conjunto de datos. Por esta razón, el valor de la media disminuye de 6 a aproximadamente 5.03 unidades."),
              
              h3(class = "subtitulo-seccion", "Importancia para el análisis distributivo"),
              p("¿Qué importancia tiene esto para el análisis distributivo? En países donde la desigualdad económica es directamente observable, el ajuste del parámetro α permite analizar cómo cambia la valoración del ingreso cuando se otorga mayor importancia relativa a las personas con menores recursos."),
              
              h3(class = "subtitulo-seccion", "El pastel de zanahoria: una analogía para entender la desigualdad"),
              p("Para entenderlo mejor, imaginemos un ejemplo sencillo."),
              p("Supón que tenemos un pastel de zanahoria dividido en ocho rebanadas. Tú te comes una y yo las siete restantes. ¡Gulp! En promedio, ambos comimos cuatro rebanadas; sin embargo, tú y yo sabemos la verdad. ¡Shhhhh, pero no se lo cuentes a nadie!"),
              p("Algo similar ocurre con el ingreso. Dos sociedades pueden tener el mismo ingreso promedio, pero distribuciones completamente distintas."),
              p("Por esta razón, conforme el parámetro α disminuye, la media generalizada se vuelve más sensible a los ingresos bajos de la distribución."),
              p("Esto permite estudiar no solamente cuánto ingreso existe en promedio, sino también cómo se encuentra distribuido entre toda la población."),
              
              div(style = "text-align: center; margin: 40px 0 20px 0;", h3(style = "color: #004c8c; font-weight: bold;", "🎯 ¡Nos vemos en la siguiente explicación!")),
              p(style = "text-align: center; font-size: 16px;", "Esperamos que esta introducción a las medias generalizadas te haya sido útil. Sigue explorando y aprendiendo sobre el análisis distributivo.")
          )
      )
  ),
  
  # ===================================================
  # 4.5 Panel de ENIGH
  # ===================================================
  
  div(id = "mainApp", class = "pantalla-interna", style = "display: none;",
      div(class = "header-fijo",
          div(class = "btn-regresar", actionButton("volver_bienvenida_enigh", label = tagList(icon("arrow-left"), "Regresar"), style = "background-color: transparent; color: #004c8c; border: 1px solid #004c8c; border-radius: 20px; padding: 5px 16px; font-size: 13px;")),
          div(class = "header-lad", img(src = "Logo_2.png", height = "30px", style = "margin-right: 10px;"))
      ),
      div(style = "max-width: 1400px; margin: 0 auto; padding: 0 30px;",
          div(class = "tarjeta-base",
              h2(class = "titulo-principal", "ENIGH"),
              div(style = "display: flex; gap: 40px; flex-wrap: wrap;",
                  div(style = "flex: 1.2; min-width: 260px;",
                      div(class = "subtitulo-seccion", "Distribuciones"),
                      checkboxGroupInput("bases_sel", label = NULL, choiceNames = as.character(opciones_bases), choiceValues = names(opciones_bases), selected = NULL),
                      p(style = "font-size: 11px; color: #888; margin-top: -5px;", "Todas las cifras a precios del mes de agosto"),
                      div(class = "subtitulo-seccion", "Variable de análisis"),
                      selectInput("var_ingreso", label = NULL, choices = setNames(c("ictpc", "ict", "ing_mon", "ing_lab", "ing_ren", "ing_tra", "nomon"), c("Ingreso corriente total per cápita", "Ingreso corriente total del hogar", "Ingreso corriente monetario", "Ingreso corriente laboral", "Ingreso por rentas", "Ingreso por transferencias", "Ingreso no monetario")), selected = "ictpc"),
                      div(class = "subtitulo-seccion", "Dominio de α"),
                      radioButtons("modo_alpha", NULL, choices = c("Discreto", "Continuo"), selected = "Discreto"),
                      conditionalPanel(condition = "input.modo_alpha == 'Discreto'", textInput("alphas", "Parámetros de α:", value = "0,1,2"), p(style = "font-size: 11px; color: #888; margin-top: -8px;", "Ingrese valores separados por comas. Ejemplo: -3, 0, 1.5")),
                      conditionalPanel(condition = "input.modo_alpha == 'Continuo'", numericInput("alpha_min", "α mínimo:", value = -1), numericInput("alpha_max", "α máximo:", value = 2), numericInput("alpha_step", "Incremento:", value = 0.1, min = 0.01)),
                      div(style = "width: 89%;", hr()),
                      div(style = "width: 100%;", actionButton("calcular", "Calcular", class = "btn-azul", style = "width: 89%;"), br(), br(), downloadButton("descargar_csv", "Descargar CSV", class = "btn-gris", style = "width: 89%;"), br(), br(), downloadButton("descargar_png", "Descargar gráfica PNG", class = "btn-gris", style = "width: 89%;"))
                  ),
                  div(style = "flex: 2.5;", h3(class = "subtitulo-seccion", "Estimaciones de medias generalizadas"), tableOutput("tabla"), br(), h3(class = "subtitulo-seccion", "Representación distributiva"), plotOutput("grafica", height = "400px"))
              )
          )
      )
  ),
  
  # ===================================================
  # 4.6 Panel de datos propios
  # ===================================================
  
  div(id = "datosPropiosPanel", class = "pantalla-interna", style = "display: none;",
      div(class = "header-fijo",
          div(class = "btn-regresar", actionButton("volver_datos_propios", label = tagList(icon("arrow-left"), "Regresar"), style = "background-color: transparent; color: #004c8c; border: 1px solid #004c8c; border-radius: 20px; padding: 5px 16px; font-size: 13px;")),
          div(class = "header-lad", img(src = "Logo_2.png", height = "30px", style = "margin-right: 10px;"))
      ),
      div(style = "max-width: 1400px; margin: 0 auto; padding: 0 30px;",
          div(class = "tarjeta-base",
              h2(class = "titulo-principal", "Datos propios"),
              div(style = "display: flex; gap: 40px; flex-wrap: wrap;",
                  div(style = "flex: 1.2; min-width: 260px;",
                      div(class = "subtitulo-seccion", "Agregar nueva base"),
                      fileInput("archivo_nuevo", "Seleccionar archivo", accept = c(".csv", ".rds", ".txt", ".xlsx"), buttonLabel = "Examinar", placeholder = "Ningún archivo seleccionado"),
                      selectInput("var_ingreso_nueva", "Variable de ingreso:", choices = c("(Cargue un archivo primero)" = ""), selected = ""),
                      selectInput("var_factor_nueva", "Factor de expansión:", choices = c("(Cargue un archivo primero)" = "", "Sin factor (ponderación uniforme)" = "none"), selected = ""),
                      actionButton("cargar_base_btn", "Cargar esta base", class = "btn-verde", style = "width: 88%;"),
                      hr(),
                      div(style = "width: 88%;", actionButton("agregar_otra_base", "+ Agregar otra base", style = "background-color: #007bff; color: white; border: none; padding: 8px 20px; border-radius: 20px; font-size: 13px; width: 100%;")),
                      div(style = "width: 88%;", hr()),
                      div(class = "subtitulo-seccion", "Bases cargadas"),
                      div(style = "width: 88%; max-height: 200px; overflow-y: auto;", uiOutput("lista_bases_cargadas")),
                      div(style = "width: 88%;", hr()),
                      div(class = "subtitulo-seccion", "Seleccione distribuciones"),
                      uiOutput("seleccion_bases_analisis"),
                      div(style = "margin-top: -10px; margin-bottom: -10px; margin-left: -10px; padding: 10px; font-size: 13px; color: #555; width: 88%; line-height: 1.5; text-align: justify;", tags$b("Importante: "), "las distribuciones comparadas deben expresarse en precios constantes del mismo año base."),
                      div(style = "width: 88%;", hr()),
                      div(class = "subtitulo-seccion", "Dominio de α"),
                      radioButtons("modo_alpha_propio", NULL, choices = c("Discreto", "Continuo"), selected = "Discreto"),
                      conditionalPanel(condition = "input.modo_alpha_propio == 'Discreto'", textInput("alphas_propio", "Parámetros de α:", value = "0,1,2"), p(style = "font-size: 11px; color: #888; margin-top: -8px;", "Ingrese valores separados por comas. Ejemplo: -3, 0, 1.5")),
                      conditionalPanel(condition = "input.modo_alpha_propio == 'Continuo'", numericInput("alpha_min_propio", "α mínimo:", value = -1), numericInput("alpha_max_propio", "α máximo:", value = 2), numericInput("alpha_step_propio", "Incremento:", value = 0.1, min = 0.01)),
                      div(style = "width: 89%;", hr()),
                      div(style = "width: 100%;", actionButton("calcular_propio", "Calcular", class = "btn-azul", style = "width: 89%;"), br(), br(), downloadButton("descargar_csv_propio", "Descargar CSV", class = "btn-gris", style = "width: 89%;"), br(), br(), downloadButton("descargar_png_propio", "Descargar gráfica PNG", class = "btn-gris", style = "width: 89%;"))
                  ),
                  div(style = "flex: 2.5;", h3(class = "subtitulo-seccion", "Estimaciones de medias generalizadas"), tableOutput("tabla_propia"), br(), h3(class = "subtitulo-seccion", "Representación distributiva"), plotOutput("grafica_propia", height = "400px"))
              )
          )
      )
  )
) # cierra fluidPage

# ===================================================
# 5. Servidor
# ===================================================

server <- function(input, output, session) {
  
  # Control de pantallas
  opcion_seleccionada <- reactiveVal(NULL)
  
  # Navegación entre frames
  observeEvent(input$volver_inicio, { shinyjs::hide("modulosPanel"); shinyjs::show("bienvenidaPanel") })
  observeEvent(input$btn_siguiente, { shinyjs::hide("bienvenidaPanel"); shinyjs::show("modulosPanel") })
  observeEvent(input$btn_modulo_mg, { shinyjs::hide("modulosPanel"); shinyjs::show("welcomePanel") })
  observeEvent(input$volver_modulos_mg, { shinyjs::hide("welcomePanel"); shinyjs::show("modulosPanel") })
  
  # Navegación dentro de MG
  observeEvent(input$btn_enigh, { opcion_seleccionada("enigh"); showNotification("✓ Has seleccionado ENIGH. Presione 'CONTINUAR'", duration = 3) })
  observeEvent(input$btn_propios, { opcion_seleccionada("datos_propios"); showNotification("✓ Has seleccionado Datos propios. Presione 'CONTINUAR'", duration = 3) })
  
  observeEvent(input$btn_continuar, {
    if (is.null(opcion_seleccionada())) {
      showNotification("⚠️ Primero seleccione ENIGH o Datos propios", duration = 3)
    } else if (opcion_seleccionada() == "enigh") {
      shinyjs::hide("welcomePanel"); shinyjs::show("mainApp")
    } else if (opcion_seleccionada() == "datos_propios") {
      shinyjs::hide("welcomePanel"); shinyjs::show("datosPropiosPanel")
    }
  })
  
  observeEvent(input$volver_bienvenida_enigh, { shinyjs::hide("mainApp"); opcion_seleccionada(NULL); shinyjs::show("welcomePanel") })
  observeEvent(input$volver_datos_propios, { shinyjs::hide("datosPropiosPanel"); opcion_seleccionada(NULL); shinyjs::show("welcomePanel") })
  
  # Conceptos básicos
  observeEvent(input$btn_conceptos, { shinyjs::hide("welcomePanel"); shinyjs::show("conceptosPanel") })
  observeEvent(input$volver_conceptos, { shinyjs::hide("conceptosPanel"); shinyjs::show("welcomePanel") })
  
  # ===================================================
  # 5.1 Datos propios
  # ===================================================
  
  bases_propias <- reactiveVal(list())
  contador_bases <- reactiveVal(0)
  
  # Lectura de archivos
  observeEvent(input$archivo_nuevo, {
    req(input$archivo_nuevo)
    archivo <- input$archivo_nuevo
    ext <- tolower(tools::file_ext(archivo$name))
    
    tryCatch({
      if (ext == "rds") {
        datos <- readRDS(archivo$datapath)
      } else if (ext == "csv" || ext == "txt") {
        raw_bytes <- readBin(archivo$datapath, "raw", n = file.info(archivo$datapath)$size)
        if (length(raw_bytes) >= 2) {
          if (raw_bytes[1] == 0xff && raw_bytes[2] == 0xfe) {
            raw_bytes <- raw_bytes[3:length(raw_bytes)]
            texto <- iconv(list(raw = raw_bytes), from = "UTF-16LE", to = "UTF-8")
          } else if (raw_bytes[1] == 0xfe && raw_bytes[2] == 0xff) {
            raw_bytes <- raw_bytes[3:length(raw_bytes)]
            texto <- iconv(list(raw = raw_bytes), from = "UTF-16BE", to = "UTF-8")
          } else {
            texto <- rawToChar(raw_bytes)
          }
        } else {
          texto <- rawToChar(raw_bytes)
        }
        temp_file <- tempfile(fileext = ".csv")
        writeLines(texto, temp_file, useBytes = TRUE)
        datos <- data.table::fread(temp_file, sep = ",", dec = ".")
        unlink(temp_file)
      } else if (ext == "xlsx") {
        if (!require(readxl)) install.packages("readxl")
        library(readxl)
        datos <- data.table::as.data.table(readxl::read_excel(archivo$datapath))
      } else {
        showNotification("Formato no soportado. Use CSV, RDS, TXT o XLSX.", duration = 3)
        return(NULL)
      }
      
      if (nrow(datos) == 0) {
        showNotification("El archivo está vacío", duration = 3)
        return(NULL)
      }
      
      columnas <- names(datos)
      updateSelectInput(session, "var_ingreso_nueva", choices = columnas, selected = columnas[1])
      opciones_factor <- c("Sin factor (ponderación uniforme)" = "none", columnas)
      updateSelectInput(session, "var_factor_nueva", choices = opciones_factor, selected = if ("factor" %in% columnas) "factor" else "none")
      attr(datos, "nombre_archivo") <- archivo$name
      assign("temp_datos", datos, envir = .GlobalEnv)
      showNotification(paste("✓ Archivo cargado:", nrow(datos), "filas,", ncol(datos), "columnas"), duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message), duration = 5, type = "error")
    })
  })
  
  # Cargar base a la lista
  observeEvent(input$cargar_base_btn, {
    req(exists("temp_datos"))
    nuevo_id <- contador_bases() + 1
    nueva_base <- list(id = nuevo_id, nombre = attr(temp_datos, "nombre_archivo"), datos = get("temp_datos", envir = .GlobalEnv), ingreso_col = input$var_ingreso_nueva, factor_col = input$var_factor_nueva)
    bases_actuales <- bases_propias()
    bases_actuales[[as.character(nuevo_id)]] <- nueva_base
    bases_propias(bases_actuales)
    contador_bases(nuevo_id)
    resetear_carga_nueva()
    showNotification(paste("✓ Base cargada:", nueva_base$nombre), duration = 3)
  })
  
  resetear_carga_nueva <- function() {
    shinyjs::reset("archivo_nuevo")
    updateSelectInput(session, "var_ingreso_nueva", choices = c("(Cargue un archivo primero)" = ""))
    updateSelectInput(session, "var_factor_nueva", choices = c("(Cargue un archivo primero)" = ""))
    if (exists("temp_datos", envir = .GlobalEnv)) rm("temp_datos", envir = .GlobalEnv)
  }
  
  eliminar_base <- function(id) {
    bases_actuales <- bases_propias()
    bases_actuales[[as.character(id)]] <- NULL
    bases_propias(bases_actuales)
    showNotification("Base eliminada", duration = 2)
  }
  
  output$lista_bases_cargadas <- renderUI({
    bases_list <- bases_propias()
    if (length(bases_list) == 0) {
      return(div(class = "bases-cargadas-list", style = "text-align: center; color: #6c757d; padding: 15px;", "No hay bases cargadas aún. Agregue una base arriba."))
    }
    tagList(div(class = "bases-cargadas-list", lapply(bases_list, function(base) {
      div(class = "base-item",
          div(class = "base-info", div(class = "base-nombre", base$nombre), div(class = "base-variables", paste0("Ingreso: ", base$ingreso_col, "  |  Factor: ", base$factor_col))),
          actionButton(paste0("eliminar_btn_", base$id), "Eliminar", class = "btn-eliminar", onclick = sprintf("Shiny.setInputValue('eliminar_base_id', %d, {priority: 'event'})", base$id))
      )
    })))
  })
  
  observeEvent(input$eliminar_base_id, { eliminar_base(input$eliminar_base_id) })
  observeEvent(input$agregar_otra_base, { resetear_carga_nueva(); showNotification("Listo para cargar otra base", duration = 2) })
  
  output$seleccion_bases_analisis <- renderUI({
    bases_list <- bases_propias()
    if (length(bases_list) == 0) return(p("No hay bases cargadas", style = "color: #666;"))
    checkboxes <- lapply(bases_list, function(base) { div(style = "margin: 5px 0;", checkboxInput(paste0("sel_base_", base$id), base$nombre, value = FALSE)) })
    do.call(tagList, checkboxes)
  })
  
  # Cálculo de resultados para datos propios
  resultados_propios <- eventReactive(input$calcular_propio, {
    bases_list <- bases_propias()
    if (length(bases_list) == 0) { showNotification("No hay bases cargadas", duration = 3); return(NULL) }
    ids_seleccionados <- NULL
    for (base in bases_list) {
      if (!is.null(input[[paste0("sel_base_", base$id)]]) && input[[paste0("sel_base_", base$id)]] == TRUE) {
        ids_seleccionados <- c(ids_seleccionados, base$id)
      }
    }
    if (length(ids_seleccionados) == 0) { showNotification("Seleccione al menos una base para analizar", duration = 3); return(NULL) }
    
    if (input$modo_alpha_propio == "Discreto") {
      alphas <- as.numeric(unlist(strsplit(gsub(" ", "", input$alphas_propio), ",")))
      alphas <- alphas[!is.na(alphas)]
      if (length(alphas) == 0) return(NULL)
    } else {
      alphas <- seq(from = input$alpha_min_propio, to = input$alpha_max_propio, by = input$alpha_step_propio)
    }
    
    resultados_list <- list()
    for (id in ids_seleccionados) {
      base <- bases_list[[as.character(id)]]
      dt <- base$datos
      ingresos <- as.numeric(dt[[base$ingreso_col]])
      
      if (base$factor_col == "none") {
        ponderadores <- rep(1, length(ingresos))
      } else {
        ponderadores <- as.numeric(dt[[base$factor_col]])
      }
      
      validos <- !is.na(ingresos) & !is.na(ponderadores) & is.finite(ingresos) & is.finite(ponderadores) & ingresos > 0 & ponderadores > 0
      ingresos <- ingresos[validos]
      ponderadores <- ponderadores[validos]
      if (length(ingresos) == 0) next
      ponderadores <- ponderadores / sum(ponderadores)
      
      resultados <- list()
      for (alpha in alphas) {
        if (abs(alpha) < 1e-10) {
          mg <- exp(sum(ponderadores * log(ingresos)))
        } else if (abs(alpha - 1) < 1e-10) {
          mg <- sum(ponderadores * ingresos)
        } else {
          mg <- (sum(ponderadores * (ingresos^alpha)))^(1/alpha)
        }
        resultados[[paste0("α = ", sprintf("%.4f", alpha))]] <- round(mg, 2)
      }
      resultados_list[[length(resultados_list) + 1]] <- data.table(periodo = base$nombre, base = base$nombre, as.data.table(resultados))
    }
    if (length(resultados_list) == 0) return(NULL)
    data.table::rbindlist(resultados_list)
  })
  
  output$tabla_propia <- renderTable({ req(resultados_propios()); resultados_propios() }, align = "c")
  
  output$grafica_propia <- renderPlot({
    req(resultados_propios())
    res_long <- data.table::melt(resultados_propios(), id.vars = c("periodo", "base"))
    res_long[, alpha := as.numeric(sub("α = ", "", variable))]
    p <- ggplot(res_long, aes(x = alpha, y = value, color = periodo, group = periodo)) +
      geom_line(linewidth = 1.2) + theme_minimal(base_size = 12) +
      labs(x = expression(alpha), y = "Media generalizada", color = "Base")
    if (input$modo_alpha_propio == "Discreto") p <- p + geom_point(size = 3)
    p
  })
  
  output$descargar_csv_propio <- downloadHandler(filename = function() paste0("resultados_propios_", Sys.Date(), ".csv"), content = function(file) data.table::fwrite(resultados_propios(), file))
  output$descargar_png_propio <- downloadHandler(filename = function() paste0("grafica_propia_", Sys.Date(), ".png"), content = function(file) {
    req(resultados_propios())
    res_long <- data.table::melt(resultados_propios(), id.vars = c("periodo", "base"))
    res_long[, alpha := as.numeric(sub("α = ", "", variable))]
    p <- ggplot(res_long, aes(x = alpha, y = value, color = periodo, group = periodo)) +
      geom_line(linewidth = 1.2) + theme_minimal(base_size = 12) +
      labs(x = expression(alpha), y = "Media generalizada", color = "Base")
    if (input$modo_alpha_propio == "Discreto") p <- p + geom_point(size = 3)
    ggsave(filename = file, plot = p, width = 10, height = 6, dpi = 300)
  })
  
  # ===================================================
  # 5.2 ENIGH
  # ===================================================
  
  resultados <- eventReactive(input$calcular, {
    bases_sel <- input$bases_sel
    if (length(bases_sel) == 0) { showNotification("Seleccione al menos una base", duration = 3); return(NULL) }
    bases_sel <- bases_sel[bases_sel %in% names(bases)]
    if (length(bases_sel) == 0) { showNotification("Las bases seleccionadas no están disponibles", duration = 3); return(NULL) }
    
    col_ingreso <- input$var_ingreso
    if (input$modo_alpha == "Discreto") {
      alphas <- as.numeric(unlist(strsplit(gsub(" ", "", input$alphas), ",")))
      alphas <- alphas[!is.na(alphas)]
      if (length(alphas) == 0) return(NULL)
    } else {
      alphas <- round(seq(from = input$alpha_min, to = input$alpha_max, by = input$alpha_step), 4)
    }
    data.table::rbindlist(lapply(bases_sel, function(id_base) {
      calcular_mg_flexible(dt = bases[[id_base]], año = id_base, alphas = alphas, col_ingreso = col_ingreso)
    }))
  })
  
  output$tabla <- renderTable({ req(resultados()); resultados() }, align = "c")
  
  output$grafica <- renderPlot({
    req(resultados())
    res_long <- data.table::melt(resultados(), id.vars = c("periodo", "base"))
    res_long[, alpha := as.numeric(sub("α = ", "", variable))]
    res_long[, etiqueta := paste0(periodo, " (base ", base, ")")]
    p <- ggplot(res_long, aes(x = alpha, y = value, color = etiqueta, group = etiqueta)) +
      geom_line(linewidth = 1.2) + theme_minimal(base_size = 14) +
      labs(x = expression(alpha), y = "Media generalizada", color = "Distribución")
    if (input$modo_alpha == "Discreto") p <- p + geom_point(size = 3)
    p
  })
  
  output$descargar_csv <- downloadHandler(filename = function() paste0("resultados_mg_", Sys.Date(), ".csv"), content = function(file) data.table::fwrite(resultados(), file))
  output$descargar_png <- downloadHandler(filename = function() paste0("grafica_mg_", Sys.Date(), ".png"), content = function(file) {
    req(resultados())
    res_long <- data.table::melt(resultados(), id.vars = c("periodo", "base"))
    res_long[, alpha := as.numeric(sub("α = ", "", variable))]
    res_long[, etiqueta := paste0(periodo, " (base ", base, ")")]
    p <- ggplot(res_long, aes(x = alpha, y = value, color = etiqueta, group = etiqueta)) +
      geom_line(linewidth = 1.2) + theme_minimal(base_size = 14) +
      labs(x = expression(alpha), y = "Media generalizada", color = "Distribución")
    if (input$modo_alpha == "Discreto") p <- p + geom_point(size = 3)
    ggsave(filename = file, plot = p, width = 10, height = 6, dpi = 300)
  })
}

# ===================================================
# 6. Ejecución
# ===================================================

shinyApp(ui, server, options = list(launch.browser = TRUE))



