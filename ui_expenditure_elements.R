


# datasets
load(file = "data/egt_gasto_07042018.RData")

# 01. Gasto turistico total por islas segun paises de residencia ----

## A. sidebarpanel
gasto01.sidebarpanel <- sidebarPanel(
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
              selected = "TOTAL PAÍSES" #, multiple = TRUE
  ),
  selectInput("isla1", "Islas",
              choices = c(b1.gasto$Islas %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "CANARIAS"),
  selectInput("period1", "Periodicidad",
              choices = c(b1.gasto$periodicidad %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "anual"),
  downloadButton('download1',"Descargar datos (csv)")
)


## B. mainpanel
gasto01.mainpanel <- mainPanel(
  h4("01. Gasto turístico total por islas según paÍses de residencia", align = "left"),
  h1("", align = "left"),
  dygraphOutput("df1graph"),
  h1("", align = "left"),
  DT::dataTableOutput("df1")
)
