




# load packages
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(istacr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(dygraphs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))




# datasets
# b1.gasto <- suppressWarnings(istac("sec.hos.enc.ser.2530", POSIXct = TRUE)) %>%
#   mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
#          periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
#          periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
# save(b1.gasto, file = "data/egt_gasto_2530.RData")
# b2.perfil <- suppressWarnings(istac("sec.hos.enc.ser.2597", POSIXct = TRUE)) %>%
#   mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
#          periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
#          periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
# save(b2.perfil, file = "data/egt_perfil_2597.RData")
# b3.motivos <- suppressWarnings(istac("sec.hos.enc.ser.2646", POSIXct = TRUE)) %>%
#   mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
#          periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
#          periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
# save(b3.motivos, file = "data/egt_motivo_2646.RData")
load(file = "data/egt_gasto_2530.RData")
load(file = "data/egt_perfil_2597.RData")
load(file = "data/egt_motivo_2646.RData")


# elements
source("description_elements.R", encoding = "utf-8")
source("authors_elements.R", encoding = "utf-8")
source("ui_expenditure_elements.R", encoding = "utf-8")
source("ui_profile_elements.R", encoding = "utf-8")
source("ui_travelcharacteristics_elements.R", encoding = "utf-8")


# user interface
ui <- fluidPage(
  theme = shinytheme(theme = "flatly"),
  navbarPage(tagList(a("ISTAC",
                       href="http://www.gobiernodecanarias.org/istac/"),
                     " || ",
                     a("Turismo",
                       href="http://www.gobiernodecanarias.org/istac/temas_estadisticos/sectorservicios/hosteleriayturismo/")),
             
             tabPanel("Descripción",
                      description.mainpanel),
             
             navbarMenu("Gasto turístico",
                        tabPanel("01. Gasto turístico total por islas según países de residencia",
                                 sidebarLayout(
                                   gasto01.sidebarpanel,
                                   gasto01.mainpanel
                                   ))),
             
             navbarMenu("Perfil del turista",
             tabPanel("01. Turistas por islas según grupos de edad, sexos y países de residencia",
                      sidebarLayout(
                        perfil01.sidebarpanel,
                        perfil01.mainpanel
                       ))),
             
             
             navbarMenu("Características del viaje",
             tabPanel("01. Turistas por islas según países de residencia y motivos de la estancia",
                      sidebarLayout(
                        caractviaje01.sidebarpanel,
                        caractviaje01.mainpanel
                        ))),
             
             tabPanel("Autores",
                      authors.mainpanel
                      )
             
             )
)





# server application
server <- function(input, output) {
  
  df.input.1 <- reactive({
    data1 <- b1.gasto %>%
      filter(`Indicadores de gasto` == input$indgasto1,
             `Países de residencia` %in% input$residencia1,
             Indicadores == input$indicador1,
             periodicidad == input$period1,
             Islas %in% input$isla1)
    return(data1)
  })
  
  df.input.2 <- reactive({
    data2 <- b2.perfil %>%
      filter(Edades == input$edad2,
             `Países de residencia` %in% input$residencia2,
             Sexos == input$sexo2,
             periodicidad == input$period2,
             Islas == input$isla2)
    return(data2)
  })
   
   df.input.3 <- reactive({
     data3 <- b3.motivos %>%
       filter(`Países de residencia` %in% input$residencia3,
              `Motivos de la estancia` == input$motivo3,
              periodicidad == input$period3,
              Islas == input$isla3)
     return(data3)
   })
   
   output$df1 <- DT::renderDataTable({
     df.input.1()
   })
   
   output$df2 <- DT::renderDataTable({
     df.input.2()
   })
   
   output$df3 <- DT::renderDataTable({
     df.input.3()
   })
   
   
   
   output$df1graph <- renderDygraph({
     data1 <- df.input.1()[,c("Países de residencia", "fecha", "valor")] %>%
       reshape(., idvar = "fecha", timevar = "Países de residencia", direction = "wide")
     colnames(data1) <- gsub("valor.", "", colnames(data1))
     
     xts(data1, as.Date(data1$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
       dygraph() %>%
       dyAxis(
         name = "y",
         valueFormatter = 'function(d){return d}',
         axisLabelFormatter = 'function(d){return Math.round(d/1e6) + " mill."}'
       ) %>%
       dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
       dyRangeSelector()
     
   })
   
   output$df2graph <- renderDygraph({
     data2 <- df.input.2()[,c("Países de residencia", "fecha", "valor")] %>%
       reshape(., idvar = "fecha", timevar = "Países de residencia", direction = "wide")
     colnames(data2) <- gsub("valor.", "", colnames(data2))
     
     xts(data2, as.Date(data2$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
       dygraph() %>%
       dyAxis(
         name = "y",
         valueFormatter = 'function(d){return d}',
         axisLabelFormatter = 'function(d){return Math.round(d/1e6) + " mill."}'
       ) %>%
       dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
       dyRangeSelector()
     
   })
   
   output$df3graph <- renderDygraph({
     data3 <- df.input.3()[,c("Países de residencia", "fecha", "valor")] %>%
       reshape(., idvar = "fecha", timevar = "Países de residencia", direction = "wide")
     colnames(data3) <- gsub("valor.", "", colnames(data3))
     
     xts(data3, as.Date(data3$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
       dygraph() %>%
       dyAxis(
         name = "y",
         valueFormatter = 'function(d){return d}',
         axisLabelFormatter = 'function(d){return Math.round(d/1e6) + " mill."}'
       ) %>%
       dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
       dyRangeSelector()
   })
   
   output$download1 <- downloadHandler(
     filename = function(){"thename.csv"}, 
     content = function(fname){
       write.csv(df.input.1(), fname)
     }
   )
   output$download2 <- downloadHandler(
     filename = function(){"thename.csv"}, 
     content = function(fname){
       write.csv(df.input.2(), fname)
     }
   )
   output$download3 <- downloadHandler(
     filename = function(){"thename.csv"}, 
     content = function(fname){
       write.csv(df.input.3(), fname)
     }
   )
   
   
   
}

# run application 
shinyApp(ui = ui, server = server)


