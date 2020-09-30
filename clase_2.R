library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)

data("countriesHigh")
Datos <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Bombus.csv")
## Miremos los datos
Datos <- Datos %>% st_as_sf(coords = c(5, 6), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
Mapa <- countriesHigh %>% st_as_sf() %>% st_crop(Datos) #

#st_box() coordenadas minimas y maximas

#Graficas

ggplot() + geom_sf(data = Mapa) + theme_bw()

#agregamos los puntos

ggplot() + geom_sf(data = Mapa) + geom_sf(data = Datos) + theme_bw()

#clasificamos por especie

ggplot() + geom_sf(data = Mapa) + geom_sf(data = Datos, aes(color = species)) + 
        theme_bw()

#agregamos tamaño por abundancia

ggplot() + geom_sf(data = Mapa) + 
        geom_sf(data = Datos, aes(color = species, size = Measurement)) + 
        theme_bw() + theme(legend.position = "bottom")


#tabla para ver mejor especie a modelar

Bombus <- Datos %>% 
        group_by(species) %>% 
        summarise(n=n(), maximo = max(Measurement),
                  minimo =min(Measurement), promedio = mean(Measurement))

#Modelo para Bombus impatiens

B_impatiens <- Datos %>% dplyr::filter(species == "Bombus impatiens")
ggplot() + geom_sf(data = Mapa) + geom_sf(data = B_impatiens, 
                                          aes(size = Measurement)) + theme_bw()


#obtener datos climaticos

Bioclim <- getData("worldclim", var = "bio", res = 5) %>% crop(B_impatiens)

# extraer las capas que se van a utilizar

# https://www.worldclim.org/data/bioclim.html

Bioclim <- Bioclim[[c(1, 7, 12, 15)]]
plot(Bioclim)
#extraemos los datos de los puntos

Clima <- extract(Bioclim, B_impatiens) %>% as.data.frame()

# las unimos

B_impatiens <- B_impatiens %>% bind_cols(Clima)
