

# load packages
suppressPackageStartupMessages(library(shiny))

description.mainpanel <- mainPanel(
  fluidPage(
    
    tags$p("Esta aplicación Shiny tiene por objeto la visualización de los datos públicos obtenidos en la ",
           tags$a(href="http://www.gobiernodecanarias.org/istac/temas_estadisticos/sectorservicios/hosteleriayturismo/demanda/C00028A.html",
                  "Encuesta sobre Gasto Turístico"),
           " del",
           tags$a(href = "http://www.gobiernodecanarias.org/istac/",
                  "Instituto Canario de Estadística (ISTAC)"),
           "."),
    
    tags$p("Los datos publicados se han obtenido usando la API base del",
           tags$a(href = "http://www.gobiernodecanarias.org/istac/",
                  "ISTAC"),
           ", a través de la librería ",
           tags$a(href = "https://github.com/rOpenSpain/istacr", "istacr"),
           "."),
    
    tags$h4("Aplicación Shiny en proceso de desarrollo... se muestran resultados preliminares.")
    
  ), align = "left")
