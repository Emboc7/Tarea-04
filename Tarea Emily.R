### Abrir librerías
library(dplyr)
library(DT)
library(leaflet)
library(plotly)
library(raster)
library(sf)
library(spData)
library(tidyr)


### Datos
orchidaceae <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

asp <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    quiet = TRUE
  )

st_crs(asp) = 4326
st_crs(orchidaceae) = 4326

orchidaceae$species[orchidaceae$species == ""] <- "orchidaceae"

### Desarrollo
#### Eliminar los registros con un valor mayor que 1000 y eliminar los registros con valor vacio o NA en el campo species

orchidaceae <- orchidaceae %>%
  filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 1000)%>%
  filter(species!="orchidaceae")

asp <- asp %>%
  filter(descripcio!="Area Marina de Manejo" & descripcio!="Area marina protegida")

asp_registro <-
  asp %>%
  st_join(orchidaceae) %>%
  group_by(nombre_asp) %>%
  summarize(especies = n_distinct(species,na.rm = TRUE)) 
st_crs(asp_registro) = 4326

color_registro <-
  colorNumeric(palette = "Reds",
               domain = asp_registro$especies,
               na.color = "transparent")

color_especies <-
  colorNumeric(palette = "Reds",
               domain = asp_registro$especies,
               na.color = "transparent")

#### Mapa Leaflet
leaflet() %>%
  addTiles(group = "OSM") %>%
  addPolygons(
    data = asp_registro,
    fillColor = ~ color_especies (asp_registro$especies),
    fillOpacity = 0.8,
    stroke = TRUE,
    color = "grey",
    weight = 1,
    popup = paste(
      paste(
        "<strong>Localidad:</strong>",
        asp_registro$nombre_asp
      ),
      paste(
        "<strong>Cantidad de orquideas:</strong>",
        asp_registro$especies
        
      ),
      sep = '<br/>'
    ),
    group = "Localidad - especies"
  ) %>%
  addLayersControl(baseGroups = c("OSM"),
                   overlayGroups = c("Localidad - especies")) %>%
  addLegend(
    position = "bottomright",
    pal = color_especies,
    values = asp_registro$especies,
    group = "Localidad - especies",
    title = "Cantidad orquideas")      