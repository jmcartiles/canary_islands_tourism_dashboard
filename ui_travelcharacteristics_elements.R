


# datasets
load(file = "data/egt_motivo_2646.RData")


# 01. 01. Turistas por islas segun paises de residencia y motivos de la estancia ----

## A. sidebarpanel
caractviaje01.sidebarpanel <- sidebarPanel(
    selectInput("residencia3", "Países de residencia",
                choices = c(b3.motivos$paisesresidencia %>%
                              unique() %>% sort() %>% as.vector()),
                selected = "TOTAL PAÍSES", multiple = TRUE),
    selectInput("isla3", "Islas",
                choices = c(b3.motivos$islas %>%
                              unique() %>% sort() %>% as.vector()),
                selected = "CANARIAS"),
    selectInput("motivo3", "Motivos de la estancia",
                choices = c(b3.motivos$motivos
                            %>% unique() %>% sort() %>% as.vector()),
                selected = "TOTAL"),
    selectInput("period3", "Periodicidad",
                choices = c(b3.motivos$periodicidad %>%
                              unique() %>% sort() %>% as.vector()),
                selected = "anual"),
    downloadButton('download3',"Descargar datos (csv)")
  )


## B. mainpanel
caractviaje01.mainpanel <- mainPanel(
  h4("01. Turistas por islas según países de residencia y motivos de la estancia", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("df3graph"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df3")
)
