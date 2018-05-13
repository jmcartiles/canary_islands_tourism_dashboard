


# datasets
load(file = "data/egt_perfil_2597.RData")
load(file = "data/egt_perfil_2587.RData")
load(file = "data/egt_perfil_2588.RData")

# 01. Turistas por islas segUn grupos de edad, sexos y paIses de residencia ----

## A. sidebarpanel
perfil01.sidebarpanel <- sidebarPanel(
  selectInput("residencia2", "Países de residencia",
              choices = c(b2.perfil$paisesresidencia %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "TOTAL PAÍSES", multiple = TRUE),
  selectInput("isla2", "Islas",
              choices = c(b2.perfil$islas %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "CANARIAS"),
  selectInput("edad2", "Grupo de Edad",
              choices = c(b2.perfil$edades %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "TOTAL GRUPOS DE EDAD"),
  selectInput("sexo2", "Sexos",
              choices = c(b2.perfil$sexos %>%
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
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2")
)


# 01. Turistas segun sexos por NUTS1 de residencia ----

## A. sidebarpanel
perfil.sidebarpanel.2587 <- sidebarPanel(
  selectInput("sexo2587", "Sexos",
              choices = c(df.perfil.2587$Sexos %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "AMBOS SEXOS"),
  selectInput("period2587", "Periodicidad",
              choices = c(df.perfil.2587$periodicidad %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "anual"),
  selectInput("nuts12587", "NUTS1",
              choices = c(df.perfil.2587$NUTS1 %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "ALEMANIA", multiple = TRUE),
  downloadButton('download2587',"Descargar datos (csv)")
)


## B. mainpanel
perfil.mainpanel.2587 <- mainPanel(
  h4("02. Turistas según sexos por NUTS1 de residencia", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("dygraph2587"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2587")
)

# 01. Turistas segun grupo de edad por NUTS1 de residencia ----

## A. sidebarpanel
perfil.sidebarpanel.2588 <- sidebarPanel(
  selectInput("edad2588", "Edades",
              choices = c(df.perfil.2588$Edades %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "TOTAL GRUPOS DE EDADES"),
  selectInput("period2588", "Periodicidad",
              choices = c(df.perfil.2588$periodicidad %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "anual"),
  selectInput("nuts12588", "NUTS1",
              choices = c(df.perfil.2588$NUTS1 %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "ALEMANIA", multiple = TRUE),
  downloadButton('download2588',"Descargar datos (csv)")
)


## B. mainpanel
perfil.mainpanel.2588 <- mainPanel(
  h4("03. Turistas según grupo de edad por NUTS1 de residencia", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("dygraph2588"),
  h4("", align = "left"),
  
  p(paste0("Fecha de actualización: ", Sys.Date())),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2588")
)
