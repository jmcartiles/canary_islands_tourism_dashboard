




# load packages
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(istacr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(dygraphs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(xts))


# datasets
# b1.gasto <- suppressWarnings(istac("sec.hos.enc.ser.2530", POSIXct = TRUE)) %>%
#   mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
#          periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
#          periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
# save(b1.gasto, file = "egt_gasto_07042018.RData")
# b2.perfil <- suppressWarnings(istac("sec.hos.enc.ser.2597", POSIXct = TRUE)) %>%
#   mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
#          periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
#          periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
# save(b2.perfil, file = "egt_perfil_07042018.RData")
# b3.motivos <- suppressWarnings(istac("sec.hos.enc.ser.2646", POSIXct = TRUE)) %>%
#   mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
#          periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
#          periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
# save(b3.motivos, file = "egt_motivo_07042018.RData"
load(file = "data/egt_gasto_07042018.RData")
load(file = "data/egt_perfil_07042018.RData")
load(file = "data/egt_motivo_07042018.RData")


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
             tabPanel("01. Turistas por islas según grupos de edad, sexos y pa?ses de residencia",
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
    data1 <- b1.gasto
    data1 <- filter(data1, `Indicadores de gasto` == input$indgasto1)
    data1 <- filter(data1, `Países de residencia` %in% input$residencia1)
    data1 <- filter(data1, Indicadores == input$indicador1)
    data1 <- filter(data1, periodicidad == input$period1)
    data1 <- filter(data1, Islas == input$isla1)
    return(data1)
  })
  
  df.input.2 <- reactive({
    data2 <- b2.perfil
    data2 <- filter(data2, Edades == input$edad2)
    data2 <- filter(data2, `Países de residencia` == input$residencia2)
    data2 <- filter(data2, Sexos == input$sexo2)
    data2 <- filter(data2, periodicidad == input$period2)
    data2 <- filter(data2, Islas == input$isla2)
    return(data2)
  })
   
   df.input.3 <- reactive({
     data3 <- b3.motivos
     data3 <- filter(data3, `Países de residencia` == input$residencia3)
     data3 <- filter(data3, `Motivos de la estancia` == input$motivo3)
     data3 <- filter(data3, periodicidad == input$period3)
     data3 <- filter(data3, Islas == input$isla3)
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
     data1 <- b1.gasto
     data1 <- filter(data1, `Indicadores de gasto` == input$indgasto1)
     data1 <- filter(data1, `Países de residencia` == input$residencia1)
     data1 <- filter(data1, Indicadores == input$indicador1)
     data1 <- filter(data1, periodicidad == input$period1)
     data1 <- filter(data1, Islas == input$isla1)
     xts(data1$valor, as.Date(data1$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
       dygraph() %>%
       dyRangeSelector()
   })
   
   output$df2graph <- renderDygraph({
     data2 <- b2.perfil
     data2 <- filter(data2, Edades == input$edad2)
     data2 <- filter(data2, `Países de residencia` == input$residencia2)
     data2 <- filter(data2, Sexos == input$sexo2)
     data2 <- filter(data2, periodicidad == input$period2)
     data2 <- filter(data2, Islas == input$isla2)
     xts(data2$valor, as.Date(data2$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
       dygraph() %>%
       dyRangeSelector()
   })
   
   output$df3graph <- renderDygraph({
     data3 <- b3.motivos
     data3 <- filter(data3, `Países de residencia` == input$residencia3)
     data3 <- filter(data3, `Motivos de la estancia` == input$motivo3)
     data3 <- filter(data3, periodicidad == input$period3)
     data3 <- filter(data3, Islas == input$isla3)
     xts(data3$valor, as.Date(data3$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
       dygraph() %>%
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

