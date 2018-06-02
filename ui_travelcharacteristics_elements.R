


# datasets
load(file = "data/egt_motivo_2646.RData")
load(file = "data/egt_motivo_2609.RData")
load(file = "data/egt_motivo_2610.RData")


# 01. Turistas por islas segun paises de residencia y motivos de la estancia ----

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


# 02.Turistas segun aspectos en la eleccion de Canarias como destino turistico por tipos de alojamiento ----

## A. sidebarpanel
caractviaje.sidebarpanel.2609 <- sidebarPanel(
  # selectInput("taloj2609", "Tipos de alojamiento",
  #             choices = c(df.motivo.2609$`Tipos de alojamiento` %>%
  #                           unique() %>% sort() %>% as.vector()),
  #             selected = "TOTAL ALOJAMIENTOS", multiple = TRUE),
  selectInput("aspeleccion2609", "Aspectos en la elección de Canarias",
              choices = c(df.motivo.2609$`Aspectos en la elección de Canarias` %>%
                            unique() %>% sort() %>% as.vector()),
              selected = c("Clima o sol"), multiple = TRUE),
  selectInput("period2609", "Periodo",
              choices = c(df.motivo.2609$Periodos %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "2017"),
  downloadButton('download2609',"Descargar datos (csv)")
)


## B. mainpanel
caractviaje.mainpanel.2609 <- mainPanel(
  h4("02. Turistas según aspectos en la elección de Canarias como destino turístico por tipos de alojamiento", align = "left"),
  
  h1("", align = "left"),
  billboarderOutput("billboarderbar2609"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2609")
)


# 03.Turistas segun formas de conocer Canarias como destino turistico por paises de residencia ----

## A. sidebarpanel
caractviaje.sidebarpanel.2610 <- sidebarPanel(
  # selectInput("taloj2609", "Tipos de alojamiento",
  #             choices = c(df.motivo.2609$`Tipos de alojamiento` %>%
  #                           unique() %>% sort() %>% as.vector()),
  #             selected = "TOTAL ALOJAMIENTOS", multiple = TRUE),
  selectInput("residencia2610", "Países de residencia",
              choices = c(df.motivo.2610$`Países de residencia` %>%
                            unique() %>% sort() %>% as.vector()),
              selected = c("TOTAL PAÍSES"), multiple = TRUE),
  selectInput("period2610", "Periodo",
              choices = c(df.motivo.2610$Periodos %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "2017"),
  downloadButton('download2610',"Descargar datos (csv)")
)


## B. mainpanel
caractviaje.mainpanel.2610 <- mainPanel(
  h4("03. Turistas según formas de conocer Canarias como destino turístico por países de residencia", align = "left"),
  
  h1("", align = "left"),
  billboarderOutput("billboarderbar2610"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2610")
)


