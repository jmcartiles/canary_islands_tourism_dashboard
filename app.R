









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
b1.gasto <- suppressWarnings(istac("sec.hos.enc.ser.2530", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"))
b2.perfil <- suppressWarnings(istac("sec.hos.enc.ser.2597", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"))
b3.motivos <- suppressWarnings(istac("sec.hos.enc.ser.2646", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"))


# d1.gasto <- istac("sec.hos.enc.ser.2530")
# d2.perfil <- istac("sec.hos.enc.ser.2597")
# d3.motivos <- istac("sec.hos.enc.ser.2646")




# user interface
ui <- fluidPage(
  theme = shinytheme(theme = "flatly"),
  # navbarPage("ISTAC || Tourism Statistics",
  navbarPage(tagList(a("ISTAC",
                       href="http://www.gobiernodecanarias.org/istac/"),
                     " || ",
                     a("Turismo",
                       href="http://www.gobiernodecanarias.org/istac/temas_estadisticos/sectorservicios/hosteleriayturismo/")),
             
             tabPanel("Descripción"),
             
             navbarMenu("Gasto turístico",
                        tabPanel("01. Gasto turístico total por islas según países de residencia",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("indgasto1", "Indicadores de gasto",
                                                 choices = c(b1.gasto$`Indicadores de gasto` %>%
                                                               unique() %>% sort() %>% as.vector()),
                                                 selected = "Gasto total"),
                                     selectInput("indicador1", "Indicadores",
                                                 choices = c(b1.gasto$Indicadores %>%
                                                               unique() %>% sort() %>% as.vector()),
                                                 selected = "Valor absoluto"),
                                     selectInput("residencia1", "Países de residencia",
                                                 choices = c(b1.gasto$`Países de residencia` %>%
                                                               unique() %>% sort() %>% as.vector()),
                                                 selected = "TOTAL PAÍSES"),
                                     selectInput("isla1", "Islas",
                                                 choices = c(b1.gasto$Islas %>%
                                                               unique() %>% sort() %>% as.vector()),
                                                 selected = "CANARIAS"),
                                     selectInput("period1", "Periodicidad",
                                                 choices = c(b1.gasto$periodicidad %>%
                                                               unique() %>% sort() %>% as.vector()),
                                                 selected = "anual"),
                                     downloadButton('download1',"Descargar datos (csv)")
                                   ),
                                   mainPanel(
                                     # tabsetPanel(
                                     # tabPanel("Gráficos", plotOutput("plot")),
                                     # tabPanel("Perfil del turista por islas", DT::dataTableOutput("df2"))
                                     h4("01. Gasto turístico total por islas según países de residencia", align = "left"),
                                     h1("", align = "left"),
                                     dygraphOutput("df1graph"),
                                     h1("", align = "left"),
                                     DT::dataTableOutput("df1")
                                     
                                     # )
                                   )))),
             
             navbarMenu("Perfil del turista",
             tabPanel("01. Turistas por islas según grupos de edad, sexos y países de residencia",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("residencia2", "Países de residencia",
                                      choices = c(b2.perfil$`Países de residencia` %>%
                                                    unique() %>% sort() %>% as.vector()),
                                      selected = "TOTAL PAÍSES"),
                          selectInput("isla2", "Islas",
                                      choices = c(b2.perfil$Islas %>%
                                                    unique() %>% sort() %>% as.vector()),
                                      selected = "CANARIAS"),
                          selectInput("edad2", "Grupo de Edad",
                                     choices = c(b2.perfil$Edades %>%
                                                   unique() %>% sort() %>% as.vector()),
                                     selected = "TOTAL GRUPOS DE EDAD"),
                          selectInput("sexo2", "Sexos",
                                     choices = c(b2.perfil$Sexos %>%
                                                   unique() %>% sort() %>% as.vector()),
                                     selected = "AMBOS SEXOS"),
                          selectInput("period2", "Periodicidad",
                                     choices = c(b2.perfil$periodicidad %>%
                                                   unique() %>% sort() %>% as.vector()),
                                     selected = "anual"),
                         downloadButton('download2',"Descargar datos (csv)")
                         ),
                         mainPanel(
                           # tabsetPanel(
                             # tabPanel("Gráficos", plotOutput("plot")),
                             # tabPanel("Perfil del turista por islas", DT::dataTableOutput("df2"))
                           h4("01. Turistas por islas según grupos de edad, sexos y países de residencia", align = "left"),
                           h1("", align = "left"),
                           dygraphOutput("df2graph"),
                           h1("", align = "left"),
                           DT::dataTableOutput("df2")
                             # )
                       )))),
             
             navbarMenu("Características del viaje",
             tabPanel("01. Turistas por islas según países de residencia y motivos de la estancia",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("residencia3", "Países de residencia",
                                      choices = c(b3.motivos$`Países de residencia` %>%
                                                    unique() %>% sort() %>% as.vector()),
                                      selected = "TOTAL PAÍSES"),
                          selectInput("isla3", "Islas",
                                      choices = c(b3.motivos$Islas %>%
                                                    unique() %>% sort() %>% as.vector()),
                                      selected = "CANARIAS"),
                          selectInput("motivo3", "Motivos de la estancia",
                                      choices = c(b3.motivos$`Motivos de la estancia`
                                                  %>% unique() %>% sort() %>% as.vector()),
                                      selected = "TOTAL"),
                          selectInput("period3", "Periodicidad",
                                      choices = c(b3.motivos$periodicidad %>%
                                                    unique() %>% sort() %>% as.vector()),
                                      selected = "anual"),
                          downloadButton('download3',"Descargar datos (csv)")
                        ),
                        mainPanel(
                        #   tabsetPanel(
                        #     tabPanel("Características de los viajes por islas", DT::dataTableOutput("df3"))
                          h4("01. Turistas por islas según países de residencia y motivos de la estancia", align = "left"),
                          h1("", align = "left"),
                          dygraphOutput("df3graph"),
                          h1("", align = "left"),
                          DT::dataTableOutput("df3")
                        )))),
             
             tabPanel("Autores")
             
             )
)








# server application
server <- function(input, output) {
  
  df.input.1 <- reactive({
    data1 <- b1.gasto
    data1 <- filter(data1, `Indicadores de gasto` == input$indgasto1)
    data1 <- filter(data1, `Países de residencia` == input$residencia1)
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
   
   # output$plot <- renderPlot({
   #   ggplot(df.input(), aes(fecha, valor)) +
   #     geom_line() +
   #     geom_point() +
   #     xlab("") +
   #     ylab("") +
   #     theme(
   #       panel.background = element_blank(),
   #       axis.ticks = element_blank()
   #     )
   # })
   
   
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

