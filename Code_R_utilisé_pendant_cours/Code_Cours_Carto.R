library(geojsonio)
library(dplyr)
library(tidyr)

etats = geojson_read(
  'https://datahub.io/core/geo-countries/r/countries.geojson',
  what = "sp")

# Sans leaflet
etats_f = fortify(etats)
head(etats_f)

carte = ggplot(etats_f, aes(long, lat, group = group)) +
  geom_polygon(color = "white")
carte + theme_void()

# Définir une couleur par pays, de manière aléatoire avec une loi uniforme
etats@data$var = runif(nrow(etats@data))
etats_f = fortify(etats)

library(tibble) # pour rownames_to_column
etats_fj = etats_f %>%
  inner_join(etats@data %>% 
               rownames_to_column("id"), 
             by = "id"
             )
head(etats_fj)

ggplot(etats_fj, aes(
  long, 
  lat, 
  group = group, 
  fill = var
  )) +
  geom_polygon(color = "white") +
  theme_void()

# Avec leaflet
library(leaflet)
# Création de la carte avec un polygone par pays
carte = leaflet(etats) %>% 
  addTiles() %>%
  addPolygons()

# Affichage de la carte
carte

# Création d'une palette affectant une couleur en fonction d'une variable
palette = colorBin("YlOrRd", domain = etats$var)

# Création de la carte choroplèthe
carte2 = leaflet(etats) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~palette(var), fillOpacity = .5)
# Affichage de la carte
carte2

# Amélioration de la carte choroplèthe
palette = colorBin(
  "magma", 
  domain = etats$var, 
  bins = seq(0, 1, by = .2)
  )

carte3 = leaflet(etats) %>% 
  setView(lat = 0, lng = 0, zoom = 1) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette(var), 
    fillOpacity = .5,
    color = "gray30", 
    weight = 1.5, 
    opacity = 1, 
    dashArray = "2"
  ) %>%
  addLegend(
    pal = palette, 
    values = ~var, 
    opacity = 0.8, 
    title = "Variable aleatoire",
    position = "bottomright"
  )
# Affichage de la carte
carte3
