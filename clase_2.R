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

Clima <- raster::extract(Bioclim, B_impatiens) %>% as.data.frame()

# las unimos

B_impatiens <- B_impatiens %>% bind_cols(Clima)

## Generar el modelo

Fit1 <- glm(Measurement ~ bio1 + I(bio1^2) + bio7 + I(bio7^2) + 
                    bio12 + I(bio12^2) + bio15 + I(bio15^2), family = poisson, 
            data = B_impatiens)

#### Predictions

Prediccion <- predict(Bioclim, Fit1, type = "response")
plot(Prediccion, colNA = "black")

## Mapas con raster y sf

USA <- getData(name = "GADM", country = "USA", level = 1) %>% st_as_sf() %>% st_crop(B_impatiens)

Prediccion_DF <- Prediccion %>% as("SpatialPixelsDataFrame") %>% 
        as.data.frame()  %>% rename(Abundancia = layer)

ggplot() + geom_tile(data = Prediccion_DF, aes(x = x, y = y,fill = Abundancia)) + #geom_tile para raster
        geom_sf(data = Mapa, alpha = 0, color = "white") + theme_bw() + scale_fill_viridis_c() + xlab("") + ylab("")

ggplot() + 
        geom_tile(data = Prediccion_DF, aes(x = x, y = y,fill = Abundancia)) + 
        geom_sf(data = USA, alpha = 0, color = "white") + 
        geom_sf(data = B_impatiens, aes(size = Measurement)) + 
        theme_bw() + scale_fill_viridis_c(option = "plasma") + 
        theme(legend.position = "bottom") + xlab("") + ylab("")


### Para predecir al futuro

Futuro <- getData("CMIP5", var = "bio", res = 5, rcp = 85,model = "HD", year = 70) %>% 
        crop(B_impatiens)
Futuro <- Futuro[[c(1, 7, 12, 15)]]

names(Futuro) <- c("bio1", "bio7", "bio12", "bio15")
Futuro_pred <- predict(Futuro, Fit1, type = "response")
plot(Futuro_pred, colNA = "black")
