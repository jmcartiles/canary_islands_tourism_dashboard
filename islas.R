library(rgdal) #readOGR
library(maptools)
library(RColorBrewer)
library(classInt)
library(dplyr)
library(tmap)

poligonos <- readOGR("data/municipios.shp")
islas.df <-  data.frame(id = 1:7,
                        isla = unique(poligonos@data$isla))
  


islas.coords <- coordinates(poligonos)
islas.id <- poligonos@data$isla
row.names(islas.df) <- unique(islas.id)


islas <- unionSpatialPolygons(poligonos, islas.id)
islas.shp <- SpatialPolygonsDataFrame(islas, islas.df)

plot(islas)
qtm(islas.shp, "id")
