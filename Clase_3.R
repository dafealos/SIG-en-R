library(leaflet)
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)
library(mapedit)
library(mapview)

Nothofagus <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Nothofagus.csv")

## Generando un primer mapa

leaflet() %>% addTiles() %>% addCircles(data = Nothofagus, lat = ~latitude, 
                                        lng = ~longitude)

#tambiense puede poner

addAwesomeMarkers()
addCircleMarkers()
addMarkers()

#cambiar colores

#numero de especies
Number_spp <- Nothofagus$scrubbed_species_binomial %>% unique() %>% 
        length()

## Nombre de las especies
Spp_Names <- Nothofagus$scrubbed_species_binomial %>% unique()

## Colores de las especies seran
Colores <- c('#e41a1c','#377eb8','#4daf4a','#984ea3',
             '#ff7f00','#ffff33','#a65628','#f781bf','#999999')

## Generamos la paleta
pal <- colorFactor(Colores, domain = Spp_Names)

## Mapa con colores
m <- leaflet() %>% 
        addTiles() %>% 
        addCircles(data = Nothofagus, lat = ~latitude, 
                                             lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                                             fillOpacity = 0.5, popup = ~date_collected, label = ~datasource, 
                                             group = "Especies")

#generar leyenda cuando se sobrepone

leaflet() %>% addTiles() %>% addCircles(data = Nothofagus, lat = ~latitude, 
                                        lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                                        fillOpacity = 0.5, label = ~scrubbed_species_binomial)

# Popups es dar clic la informacion que aparece

m <- leaflet() %>% 
        addTiles() %>% 
        addCircles(data = Nothofagus, 
        lat = ~latitude, lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
        fillOpacity = 0.5, label = ~scrubbed_species_binomial, popup = ~datasource, group = "Especies")
 
# agregar una leyenda


m <- m %>% addLegend(data = Nothofagus, "bottomright", pal = pal, 
                     values = ~scrubbed_species_binomial, title = "Especies", 
                     opacity = 0.8, group = "Leyenda")                                                                                       
                                             
# control de objetos   o seleccionar capas                          

m %>% addLayersControl(overlayGroups = c("Especies", "Leyenda"), 
                            options = layersControlOptions(collapsed = T))                                             
                                             

#Generar una cpapa por especie para poder controrlas en el mapa

N_alpina <- Nothofagus %>% dplyr::filter(scrubbed_species_binomial == 
                                                 "Nothofagus alpina")

alpin <- leaflet() %>% addTiles() %>% 
        addCircles(data = N_alpina, 
        lat = ~latitude, lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
        fillOpacity = 0.5, label = ~scrubbed_species_binomial, popup = ~datasource,
        group = "alpina")
                                             
# Creamos un loop

Spp_Pres <- list()
for (i in 1:length(Spp_Names)) {
        Spp_Pres[[i]] <- Nothofagus %>% dplyr::filter(scrubbed_species_binomial == 
                                                              Spp_Names[i])
}
names(Spp_Pres) <- Spp_Names

#generar uyn mapa pero no debe ser asi

Spp_Map <- leaflet() %>% 
        addTiles() %>% 
        addCircles(data = Spp_Pres[[1]], 
                lat = ~latitude, lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                fillOpacity = 0.5, label = ~scrubbed_species_binomial, popup = ~datasource, 
                group = Spp_Names[1]) %>% addCircles(data = Spp_Pres[[2]], 
                lat = ~latitude, lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                fillOpacity = 0.5, label = ~scrubbed_species_binomial, popup = ~datasource, 
                group = Spp_Names[2]) %>% addCircles(data = Spp_Pres[[3]], 
                lat = ~latitude, lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                fillOpacity = 0.5, label = ~scrubbed_species_binomial, popup = ~datasource, 
                group = Spp_Names[3])
.......

# Por medio de un loop

Spp_Map <- leaflet() %>% addTiles()
for (i in 1:length(Spp_Pres)) {
        Spp_Map <- Spp_Map %>% 
                addCircles(data = Spp_Pres[[i]], lat = ~latitude, 
                lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                fillOpacity = 0.5, label = ~scrubbed_species_binomial, 
                popup = ~datasource, group = Spp_Names[i])
}

# agregamos la leyenda

Spp_Map <- Spp_Map %>% addLegend(data = Nothofagus, "bottomright", 
                                 pal = pal, values = ~scrubbed_species_binomial, title = "Especies", 
                                 opacity = 1, group = "Leyenda")

# control de capas 

Grupos <- c("Leyenda", Spp_Names)
Spp_Map <- Spp_Map %>% addLayersControl(overlayGroups = Grupos, 
                                        options = layersControlOptions(collapsed = TRUE)) %>% 
                                        hideGroup("Leyenda")


## Generar un heatmap

Heat_Map <- leaflet() %>% addTiles()
for (i in 1:length(Spp_Pres)) {
        Heat_Map <- Heat_Map %>% addHeatmap(data = Spp_Pres[[i]], 
                                            lat = ~latitude, lng = ~longitude, group = Spp_Names[i], blur = 50, radius = 20)
}
Heat_Map <- Heat_Map %>% addLayersControl(baseGroups = Spp_Names, 
                                          options = layersControlOptions(collapsed = FALSE))

### Trabajando con poligonos

#download.file("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Chile.zip", 
              #destfile = "Chile.zip")

#unzip("Chile.zip")

colombia <- getData("GADM", country = "COL", level = 1)

colombia <- colombia %>% st_as_sf()

colombia <- colombia %>% select(NAME_1, geometry)

colombia_spat <- colombia %>%  as_Spatial()

leaflet(colombia) %>% addTiles() %>% addPolygons()

departamentos <- leaflet() %>% addTiles()  %>% 
        addPolygons(data = colombia_spat, 
                    fillColor = topo.colors(32, alpha = NULL), 
                    weight = 0.5, 
                    label = ~NAME_1, 
                    group = "Departamentos")%>% 
        addLayersControl(overlayGroups = "Departamentos", options = layersControlOptions(collapsed = TRUE))

# agregamos medias de area

departamentos <- departamentos %>% 
        addMeasurePathToolbar(options = measurePathOptions(imperial = F, 
                                 minPixelDistance = 100, showDistances = FALSE))



#### Permite dibujar areas

departamentos <- leaflet() %>% 
        addTiles() %>% 
        addPolygons(data = colombia_spat, 
                    fillColor = topo.colors(16, alpha = NULL), weight = 0.5, 
                    label = ~NAME_1, group = "Departamentos") %>% 
        addDrawToolbar(targetGroup = "nueva area", 
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>% 
        addLayersControl(overlayGroups = c("Departamentos", "nueva area"), 
                         options = layersControlOptions(collapsed = TRUE)) %>% 
        addMeasurePathToolbar(options = measurePathOptions(imperial = F, 
                                                           minPixelDistance = 100, showDistances = T)) %>%
        addStyleEditor()


### Mapedit


colombia_SF <- colombia_spat %>% st_as_sf()


nuevos_dep <- mapview(colombia_SF) %>%
        editMap("colombia_SF")
