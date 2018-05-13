



# load datasets from istac base API using istacr

# load necessary packages
suppressPackageStartupMessages(library(istacr))
suppressPackageStartupMessages(library(dplyr))

# datasets

# egt

# expenditure
df.gasto.2528 <- suppressWarnings(istac("sec.hos.enc.ser.2528", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual")) %>%
  rename(indicadoresgasto = "Indicadores de gasto",
         paisesresidencia = "Países de residencia",
         indicadores = "Indicadores",
         periodos = "Periodos")
save(df.gasto.2528, file = "data/egt_gasto_2528.RData")
df.gasto.2529 <- suppressWarnings(istac("sec.hos.enc.ser.2529", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual")) %>%
    rename(indicadoresgasto = "Indicadores de gasto",
           indicadores = "Indicadores",
           periodos = "Periodos")
save(df.gasto.2529, file = "data/egt_gasto_2529.RData")
b1.gasto <- suppressWarnings(istac("sec.hos.enc.ser.2530", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual")) %>%
    rename(indicadoresgasto = "Indicadores de gasto",
           paisesresidencia = "Países de residencia",
           islas = "Islas",
           indicadores = "Indicadores",
           periodos = "Periodos")
save(b1.gasto, file = "data/egt_gasto_2530.RData")

# profile
b2.perfil <- suppressWarnings(istac("sec.hos.enc.ser.2597", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual")) %>%
      rename(edades = "Edades",
             sexos = "Sexos",
             paisesresidencia = "Países de residencia",
             islas = "Islas",
             periodos = "Periodos")
save(b2.perfil, file = "data/egt_perfil_2597.RData")
df.perfil.2587 <- suppressWarnings(istac("sec.hos.enc.ser.2587", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
save(df.perfil.2587, file = "data/egt_perfil_2587.RData")
df.perfil.2588 <- suppressWarnings(istac("sec.hos.enc.ser.2588", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual"))
save(df.perfil.2588, file = "data/egt_perfil_2588.RData")

# travel characteristics
b3.motivos <- suppressWarnings(istac("sec.hos.enc.ser.2646", POSIXct = TRUE)) %>%
  mutate(fecha = format(fecha, "%Y-%m-%d %H:%M:%OS"),
         periodicidad = replace(periodicidad, periodicidad=="cuatrimestral", "Trimestral"),
         periodicidad = replace(periodicidad, periodicidad=="anual", "Anual")) %>%
  rename(motivos = "Motivos de la estancia",
         paisesresidencia = "Países de residencia",
         islas = "Islas",
         periodos = "Periodos")
save(b3.motivos, file = "data/egt_motivo_2646.RData")




