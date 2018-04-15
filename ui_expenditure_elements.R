


# datasets
load(file = "data/egt_gasto_2528.RData")
load(file = "data/egt_gasto_2529.RData")
load(file = "data/egt_gasto_2530.RData")

# 01. Gasto turistico total en Canarias según países de residencia ----

## A. sidebarpanel
gasto.sidebarpanel.2528 <- div(id = "Sidebar",
                               sidebarPanel(
                                 selectInput("indgasto2528", "Indicadores de gasto",
                                             choices = c(df.gasto.2528$`Indicadores de gasto` %>%
                                                           unique() %>% sort() %>% as.vector()),
                                             selected = "Gasto total"),
                                 selectInput("indicador2528", "Indicadores",
                                             choices = c(df.gasto.2528$Indicadores %>%
                                                           unique() %>% sort() %>% as.vector()),
                                             selected = "Valor absoluto"),
                                 selectInput("residencia2528", "Países de residencia",
                                             choices = c(df.gasto.2528$`Países de residencia` %>%
                                                           unique() %>% sort() %>% as.vector()),
                                             selected = "TOTAL PAÍSES" , multiple = TRUE
                                             ),
                                 selectInput("period2528", "Periodicidad",
                                             choices = c(df.gasto.2528$periodicidad %>%
                                                           unique() %>% sort() %>% as.vector()),
                                             selected = "anual"),
                                 downloadButton('download2528',"Descargar datos (csv)")
                                 )
                               )


## B. mainpanel
gasto.mainpanel.2528 <- mainPanel(
  h4("01. Gasto turístico total en Canarias según países de residencia", align = "left"),
  
  p(
  actionButton("showSidebar", "Mostrar filtros"),
  actionButton("hideSidebar", "Ocultar filtros"),
  align = "right"),
  
  h1("", align = "left"),
  dygraphOutput("dygraph2528"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2528")
)

# 02. Gasto turistico total por NUTS1 de residencia ----

## A. sidebarpanel
gasto.sidebarpanel.2529 <- sidebarPanel(
  selectInput("indgasto2529", "Indicadores de gasto",
              choices = c(df.gasto.2529$`Indicadores de gasto` %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "Gasto total"),
  selectInput("indicador2529", "Indicadores",
              choices = c(df.gasto.2529$Indicadores %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "Valor absoluto"),
  selectInput("nuts12529", "NUTS1",
              choices = c(df.gasto.2529$NUTS1 %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "ALEMANIA" , multiple = TRUE
              ),
  selectInput("period2529", "Periodicidad",
              choices = c(df.gasto.2529$periodicidad %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "anual"),
  downloadButton('download2529',"Descargar datos (csv)")
  )


## B. mainpanel
gasto.mainpanel.2529 <- mainPanel(
  h4("02. Gasto turístico total en Canarias según NUTS1 de residencia", align = "left"),

  h1("", align = "left"),
  dygraphOutput("dygraph2529"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2529")
)

# 03. Gasto turistico total por islas segun paises de residencia ----

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
              selected = "TOTAL PAÍSES" , multiple = TRUE
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
  h4("03. Gasto turístico total por islas según países de residencia", align = "left"),

  h1("", align = "left"),
  dygraphOutput("df1graph"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df1")
)
