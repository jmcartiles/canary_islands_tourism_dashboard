


# datasets
load(file = "data/egt_perfil_2597.RData")
load(file = "data/egt_perfil_2587.RData")
load(file = "data/egt_perfil_2588.RData")
load(file = "data/egt_perfil_2589.RData")


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
  downloadButton('download2',"Descargar datos (csv)"),
  
  width = 3
  
)


## B. mainpanel
perfil01.mainpanel <- mainPanel(
  h4("01. Turistas por islas según grupos de edad, sexos y países de residencia", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("df2graph"),
  h4("", align = "left"),
  
  p("Fecha de actualización: 14/10/2018"),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2"),
  
  width = 9
  
)


# 02. Turistas segun sexos por NUTS1 de residencia ----

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
  downloadButton('download2587',"Descargar datos (csv)"),
  
  width = 3
  
)


## B. mainpanel
perfil.mainpanel.2587 <- mainPanel(
  h4("02. Turistas según sexos por NUTS1 de residencia", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("dygraph2587"),
  h4("", align = "left"),
  
  p("Fecha de actualización: 14/10/2018"),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2587"),
  
  width = 9
  
)

# 03. Turistas segun grupo de edad por NUTS1 de residencia ----

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
  downloadButton('download2588',"Descargar datos (csv)"),
  
  width = 3
  
)


## B. mainpanel
perfil.mainpanel.2588 <- mainPanel(
  h4("03. Turistas según grupos de edad por NUTS1 de residencia", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("dygraph2588"),
  h4("", align = "left"),
  
  p("Fecha de actualización: 14/10/2018"),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2588"),
  
  width = 9
  
)

# 04. Turistas segun grupos de edad y sexos por tipos de alojamiento ----

## A. sidebarpanel
perfil.sidebarpanel.2589 <- sidebarPanel(
  selectInput("edad2589", "Edades",
              choices = c(df.perfil.2589$Edades %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "TOTAL GRUPOS DE EDADES"),
  selectInput("sexo2589", "Sexos",
              choices = c(df.perfil.2589$Sexos %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "TOTAL GRUPOS DE EDADES"),
  selectInput("period2589", "Periodicidad",
              choices = c(df.perfil.2589$periodicidad %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "anual"),
  selectInput("alojamiento2589", "Tipos de alojamiento",
              choices = c(df.perfil.2589$`Tipos de alojamiento` %>%
                            unique() %>% sort() %>% as.vector()),
              selected = "TOTAL ALOJAMIENTOS", multiple = TRUE),
  downloadButton('download2589',"Descargar datos (csv)"),
  
  width = 3
  
)


## B. mainpanel
perfil.mainpanel.2589 <- mainPanel(
  h4("04. Turistas según grupos de edad y sexos por tipos de alojamiento", align = "left"),
  
  h1("", align = "left"),
  dygraphOutput("dygraph2589"),
  h4("", align = "left"),
  
  p("Fecha de actualización: 14/10/2018"),
  h1("", align = "left"),
  
  DT::dataTableOutput("df2589"),
  
  width = 9
  
)

