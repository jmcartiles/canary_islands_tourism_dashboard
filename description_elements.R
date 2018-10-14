

description.mainpanel <- mainPanel(
  fluidPage(
    
    tags$p("Esta aplicación web tiene por objeto la visualización de los datos públicos obtenidos en la ",
           tags$a(href="http://www.gobiernodecanarias.org/istac/temas_estadisticos/sectorservicios/hosteleriayturismo/demanda/C00028A.html",
                  "Encuesta sobre Gasto Turístico"),
           " del",
           tags$a(href = "http://www.gobiernodecanarias.org/istac/",
                  "Instituto Canario de Estadística (ISTAC)"),
           "."),
    tags$p("Actualmente, recoge datos de gasto y llegada de turistas según países de residencia, perfil y características del viaje."),
    tags$p("Los datos se muestran mediante gráficos dinámicos y tablas que pueden descargarse en formato CSV."),
    
    tags$p("Los datos publicados se han obtenido usando la API base del",
           tags$a(href = "http://www.gobiernodecanarias.org/istac/",
                  "ISTAC"),
           ", a través de la librería ",
           tags$a(href = "https://github.com/rOpenSpain/istacr", "istacr"),
           "."),
    
    tags$h2(""),
    # tags$h6("Aplicación Shiny en proceso de desarrollo... se muestran resultados preliminares."),
    
    hr(),
    tags$h6("Aplicación",
    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
    "en proceso de desarrollo... se muestran resultados preliminares.")
    
    ), align = "left")






