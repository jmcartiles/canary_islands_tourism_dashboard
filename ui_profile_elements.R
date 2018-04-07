

# datasets
load(file = "data/egt_perfil_07042018.RData")

# 01. Turistas por islas segUn grupos de edad, sexos y paIses de residencia ----

## A. sidebarpanel
perfil01.sidebarpanel <- sidebarPanel(
  selectInput("residencia2", "Pa?ses de residencia",
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
)


## B. mainpanel
perfil01.mainpanel <- mainPanel(
  h4("01. Turistas por islas según grupos de edad, sexos y países de residencia", align = "left"),
  h1("", align = "left"),
  dygraphOutput("df2graph"),
  h1("", align = "left"),
  DT::dataTableOutput("df2")
)
