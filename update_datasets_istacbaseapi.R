



# load datasets from istac base API using istacr

# load necessary packages
suppressPackageStartupMessages(library(istacr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))

# functions

get_data_labelled <- function(istacr.id) {
  
  suppressWarnings(istac(istacr.id, POSIXct = TRUE)) %>%
    mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
           periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
           periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
  
}


update_dataset <- function(istacr.id, section) {
  
  assign(paste0("df.", section, ".", substring(istacr.id, nchar(istacr.id)-3, nchar(istacr.id))),
         get_data_labelled(istacr.id = istacr.id))
  save(list = (paste0("df.", section, ".", substring(istacr.id, nchar(istacr.id)-3, nchar(istacr.id)))),
       file = paste0("data/egt_", section, "_", substring(istacr.id, nchar(istacr.id)-3, nchar(istacr.id)), ".RData"))
  
}

# datasets

# egt

# expenditure

df.gasto.2528 <- get_data_labelled(istacr.id = "sec.hos.enc.ser.2528") %>%
  rename(indicadoresgasto = "Indicadores de gasto",
         paisesresidencia = "Países de residencia",
         indicadores = "Indicadores",
         periodos = "Periodos")
save(df.gasto.2528, file = "data/egt_gasto_2528.RData")

df.gasto.2529 <- get_data_labelled(istacr.id = "sec.hos.enc.ser.2529") %>%
    rename(indicadoresgasto = "Indicadores de gasto",
           indicadores = "Indicadores",
           periodos = "Periodos")
save(df.gasto.2529, file = "data/egt_gasto_2529.RData")

b1.gasto <- get_data_labelled(istacr.id = "sec.hos.enc.ser.2530") %>%
    rename(indicadoresgasto = "Indicadores de gasto",
           paisesresidencia = "Países de residencia",
           islas = "Islas",
           indicadores = "Indicadores",
           periodos = "Periodos")
save(b1.gasto, file = "data/egt_gasto_2530.RData")

# profile
b2.perfil <- get_data_labelled(istacr.id = "sec.hos.enc.ser.2597") %>%
      rename(edades = "Edades",
             sexos = "Sexos",
             paisesresidencia = "Países de residencia",
             islas = "Islas",
             periodos = "Periodos")
save(b2.perfil, file = "data/egt_perfil_2597.RData")


pmap(list(istacr.id = list("sec.hos.enc.ser.2587","sec.hos.enc.ser.2588","sec.hos.enc.ser.2589"),
          section = rep("perfil", 3)),
     update_dataset)


# travel characteristics
b3.motivos <- get_data_labelled(istacr.id = "sec.hos.enc.ser.2646") %>%
  rename(motivos = "Motivos de la estancia",
         paisesresidencia = "Países de residencia",
         islas = "Islas",
         periodos = "Periodos")
save(b3.motivos, file = "data/egt_motivo_2646.RData")

pmap(list(istacr.id = list("sec.hos.enc.ser.2609", "sec.hos.enc.ser.2610"),
          section = rep("motivo", 2)),
     update_dataset)


