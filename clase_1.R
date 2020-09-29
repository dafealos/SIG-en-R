library(sf)
library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(raster) 
library(rasterVis) # levelplot(dtaa) genera una escala
library(rworldxtra)
data("countriesHigh")

#volver a simpleficher o tabla

Mundo <- st_as_sf(countriesHigh)
head(Mundo)

colombia <- Mundo %>%  select(ne_10m_adm) %>% filter(ne_10m_adm=="COL")

plot(colombia)

ggplot() + geom_sf(data = Mundo, aes(fill = continent))

Africa <- Mundo %>% filter(continent == "Africa")

ggplot() + geom_sf(data = Africa, aes(fill = POP_EST))

#poner poblacion en millones

Africa <- Africa %>% mutate(Poblacion_mill = POP_EST/1e+06)
ggplot() + geom_sf(data = Africa, aes(fill = Poblacion_mill)) + 
        theme_bw() + scale_fill_viridis_c()

#Guardar

write_sf(colombia, "colombia.shp")


#leer

read_sf("colombia.shp")

# obtener datos vectoriales

getData(name  = "ISO3")

#level 0, 1, 2,  = pais, departamentos, municipios 

colom <- getData("GADM", country = "COL", level = 1)

#transformar a sf

colom <- colom %>% st_as_sf()

ggplot() + geom_sf(data = colom, aes(fill = NAME_1)) + theme(legend.position = "none")

#Añadir puntos

DF <- data.frame(lon = c(-74, -74), lat = c(10, 5), Casa = c("Grande", "Chica")) %>% 
        st_as_sf(coords = c(1,2), crs = "+proj=longlat +datum=WGS84") # ver proyeccion escribir colom

ggplot() + geom_sf(data = colom) + geom_sf(data = DF, aes(color = Casa))                   

#RASTER

Prec <- getData("worldclim", res = 10, var = "prec")

Invierno <- Prec[[c(6, 7, 8)]]
plot(Invierno, colNA = "black") # colNA dibuja negro el fondo

rasterVis::levelplot(Invierno)

#precipitacion total para junio, julio y agosto
Total_inv <- Prec[[6]] + Prec[[7]] + Prec[[8]]
plot(Total_inv, colNA = "black")

#precipitacion anual

PP_Total <- sum(Prec)
plot(PP_Total, colNA = "black")


### PAra cortar

Raster_colombia <- PP_Total %>% crop(colom) %>% mask(colom)
plot(Raster_colombia, colNA = "black")

#extraer precipitation total en los puntos y agregarlos a una nueva variable del DF

DF$Prec <- extract(Raster_colombia, DF)

# precipitacion mensual para cada casa

Prec_Casas <- extract(Prec, DF)

prec_casa <- Prec_Casas %>% as.data.frame() %>% bind_cols(DF)

#Guardar raster

writeRaster(Raster_colombia, "PP_col.grd", overwrite = T)
rastter <- raster("PP_col.grd") # cargar o stack


#Proyecciones

proj4string(Raster_colombia)

#generar mapas proyecciones de igual area
#https://projectionwizard.org

colom_igual <- projectRaster(Raster_colombia, crs = "+proj=laea +lon_0=23.2 +lat_0=-0.49 +datum=WGS84 +units=m +no_defs")
plot(colom_igual, colNA = "black")        



#Graficas de shpafiles y raster

colombia_raster_DF <- Raster_colombia %>% as("SpatialPixelsDataFrame") %>% 
        as.data.frame() %>% rename(preci = layer)


ggplot() + geom_tile(data = colombia_raster_DF, aes(x = x, y = y, fill = preci)) + 
        geom_sf(data = colom, alpha = 0) + scale_fill_viridis_c() + xlab("") + 
        ylab("") + theme_bw() 

