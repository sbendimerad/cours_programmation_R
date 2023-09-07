# Pour la maison, sur le fixe 
setwd('F:/Pro/Enseignement/IUT/Programmation R/Controles/2019_2020')

# Charger les librairies
library(geojsonio)
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(leaflet)

#########################################################################
#                                                                       #                 
#                          Exercice 1                                   #
#                                                                       #
#########################################################################

## Question 1
g = ggplot(mtcars, aes(mpg,hp))+geom_point()
g1 = g+geom_smooth(method = "lm")+
  labs(
    title = "Horse power vs Miles per gallon",
    x = "Miles per Gallon",
    y = "Horsepower"
  )+
  theme_light()
g1

## Question 2
g = ggplot(mtcars, aes(mpg, fill=cyl))
g1 = g+geom_histogram(aes(y=..density..), bins = 9)+
  facet_grid(facets = cyl~., scales = "free_y")+
  labs(
    title = "Distribution des miles/gallon suivant la cylindrée",
    x = "Miles per Gallon",
    y = "Densité",
    fill = "Cylindrée"
  )+
  theme_light()
g1

#########################################################################
#                                                                       #                 
#                          Exercice 2                                   #
#                                                                       #
#########################################################################

# Charger les données
load('../Donnees/commercesParis.RData')

## Question 1
# Afficher le commerce 86181
commerces %>% filter(ORDRE==86181)

## Question 2
# Afficher tous les commerces au 137 avenue de Versailles
commerces %>% filter(`ADRESSE COMPLETE`=="137 AV VERSAILLES")
# Afficher le nombre de commerces de l'avenue de Versailles
commerces %>% filter(`LIBELLE VOIE`=="VERSAILLES") %>% nrow
commerces %>% filter(`LIBELLE VOIE`=="VERSAILLES") %>% summarise(nombre=n())

## Question 3
comm = commerces %>% select(
  ORDRE,
  `CODE ACTIVITE`,
  `LIBELLE ACTIVITE`,
  `ADRESSE COMPLETE`,
  NUMERO,
  `TYPE VOIE`,
  `LIBELLE VOIE`,
  IRIS,
  ILOT,
  QUARTIER,
  ARRONDISSEMENT,
  longitude,
  latitude
) 
comm %>% View

## Question 4
arrond = commerces %>% group_by(ARRONDISSEMENT) %>% 
  summarise(nombre = n())

## Question 5
comm_nb_type = comm %>% left_join(arrond)
g1 = ggplot(
  comm_nb_type, 
  aes(
    fct_reorder(ARRONDISSEMENT, 
                nombre
    )
  )
)+
  geom_bar()+
  coord_flip()+
  labs(
    title = "Nombre de commerces par arrondissement",
    x = "Arrondissement",
    y = "Effectif"
  )+
  theme_light()
g1

## Question 6
top10_activites = comm %>% group_by(`LIBELLE ACTIVITE`) %>% 
  summarise(nb_type = n()) %>% arrange(desc(nb_type)) %>% 
  slice(1:10)
top10_activites

g3 = ggplot(
  top10_activites, 
  aes(
    fct_reorder(`LIBELLE ACTIVITE`, nb_type), 
    nb_type
  )
)+
  geom_col()+
  labs(
    title = "Les 10 types de commerce les plus presents a Paris",
    x = " ",
    y = "Effectif"
  )+
  coord_flip()+
  theme_light()
g3


## Question 7
# Chargement de la carte avec les arrondissements
paris = geojson_read("../Donnees/arrondissements.geojson", what = "sp")

# On choisit les bureaux en boutique
# Lorsque l'on essaie de faire la jointure de table, il y a un message d'erreur indiquant que
# les variables arrondissement ne sont pas du même type dans les deux tables
# => changement de la variable dans la table bureau en variable de type integer
bureaux$ARRONDISSEMENT = as.integer(bureaux$ARRONDISSEMENT)

# Ajout de la variable à représenter dans la table des arrondissements de Paris,
# ici le vombre de bureaux en boutique
paris@data = left_join(paris@data, bureaux, by = c("c_ar"="ARRONDISSEMENT"))

# Création de la palette de couleurs suivant la variable à représenter
palette = colorBin("magma", 
                   domain = paris$nb_bureaux,
                   bins = seq(50, 850, by = 200)
)

# Création de la carte choroplète
leaflet(paris) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~palette(nb_bureaux),
    fillOpacity = .5,
    color = "black",
    weight = 1.5,
    opacity = 1
  ) %>%
  addLegend(
    pal = palette,
    values = ~nb_bureaux,
    opacity = .8,
    title = "Nombre de bureaux en boutique",
    position = "topright"
  )
