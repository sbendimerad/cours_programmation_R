rm(ls=list())
graphics.off()

getwd()

#########################################################
#                                                       #
#                     EXERCICE 1                        #
#                                                       #
#########################################################
library(ggplot2)

g = ggplot(iris, aes(Sepal.Length)) +geom_histogram()
g
# Changement du nombre de classes (=bins) dans l'histogramme : on essaie plusieurs solutions
g+geom_histogram(bins=5)
g+geom_histogram(bins=6)
g+geom_histogram(bins=7)
g+geom_histogram(bins=8)
g+geom_histogram(bins=9)
g+geom_histogram(bins=10)
# Je garde 8 bins

g+geom_histogram(binwidth=1) 
g+geom_histogram(binwidth=0.5) 
g+geom_histogram(binwidth=0.75) 
g+geom_histogram(binwidth=0.87) 
# Je garde une lagrgeur de bin de 0.75
# Remarque : avec ces options, les fronti?res d?passent l'?tendue de la variable

# Dans toute la suite, je travaille avec l'histogramme ? 8 classes
g1 = g+geom_histogram(bins=8)
g1
g2 = g1+theme_light()
g2

g3 = g2+
  labs(
    title="Distribution de la longeur de Sepale", 
    x="longueur de Sepale", 
    y="Effectif"
    )
g3

# Distribution des esp?ces
g = ggplot(iris, aes(x="", fill=Species))
g1 = g+geom_bar()
g1
g2 = g1+ggtitle("Distribution des especes d'iris")+xlab(" ")
g2
g3 = g2+ylab("Effectif")
g3
g4 = g1+
  labs(
    title="Distribution des especes d'iris",
    x=" ", 
    y="Effectif"
    )
g4

# Boites ? moustaches
g = ggplot(iris, aes(Species, Sepal.Length))
g
g1 = g+geom_boxplot()
g1
g2 = g1+
  labs(
    title="Distribution de la longueur de sepale suivant l'espece", 
    x=" ", 
    y="Longueur de Sepale"
    )
g2

#########################################################
#                                                       #
#                     EXERCICE 2                        #
#                                                       #
#########################################################
library(ggplot2)
library(scales)


# Question 1 
g = ggplot(iris, aes(Sepal.Length)) +geom_histogram(bins=8)
g


g = ggplot(iris, aes(Sepal.Length)) +
  geom_histogram(bins=8,aes(y=..density..))
g

# Dans toute la suite, je travaille avec l'histogramme ? 8 classes

g1 = g+
  labs(
    title="Distribution de la longeur de Sepale", 
    x="longueur de Sepale", 
    y="Densite"
    )
g1

# Question 2 :  histogramme par esp?ce
g1 = ggplot(iris, aes(Sepal.Length, color=Species)) +
    geom_histogram(bins = 8,aes(y=..density..)) +
    labs(
      title="Distribution de la longeur de Sepale", 
      x="longueur de Sepale", 
      y="Densite", 
      color="Espece"
    )
g1


# Question 3 : Distribution des esp?ces et Diagrammes en barres horizontales
g = ggplot(iris, aes(Species))
g1 = g+geom_bar()+coord_flip()
g1
g2 = g1+labs(
  title="Distribution des especes d'iris", 
  x="Espece", 
  y="Effectif"
  )
g2


# Question 4 : Distribution en pourcentage
g1 = g+geom_bar(aes(y=..count../sum(..count..)))+
  scale_y_continuous(labels=percent)+
  coord_flip()+
  labs(
    title="Distribution des especes d'iris", 
    x="Espece", 
    y="Pourcentage"
    )
g1


# Question 5 : Diagramme circulaire
# !! Attention !! l'option position="fill" n'est pas adapt?e ici : on ne souhaite pas avoir des barres toutes de m?me hauteur

g = ggplot(iris, aes("", fill=Species))
g

g1 = g+geom_bar(aes(y=..count../sum(..count..)), width=1 )+
    scale_y_continuous(labels=percent)+
   coord_polar(theta="y")

g1

# Question 6 : 

g2 = g1 + theme_minimal()+
     theme(axis.title = element_blank())+
     labs(title="Distribution des especes", fill="Espece")
g2
# Centrer le titre avec theme()
g3 = g2+theme(plot.title = element_text(hjust = 0.5))
g3

#########################################################
#                                                       #
#                     EXERCICE 3                        #
#                                                       #
#########################################################
library(ggplot2)
library(scales)

# Q1 : Nuage de points 
g = ggplot(iris, aes(Sepal.Width, Sepal.Length))
g1 = g+geom_point()
g1
g2 = g1+geom_smooth(se = FALSE)
g2

# Q2 : Nuage de points par espece
g = ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
g1 = g + geom_point()
g1
g2 = g1 + geom_rug() + geom_smooth(method="lm", se=FALSE)
g2


# Q3 : Sur un graphique par esp?ce
g = ggplot(iris, aes(Sepal.Width, Sepal.Length))
# En colonnes
g1 = g + geom_point() + 
  geom_smooth(method="lm", se = FALSE, fullrange = TRUE) + 
  facet_wrap(facets=~Species)
g1


# En lignes
g2 = g + geom_point() + 
  geom_smooth(method="lm", se = FALSE, fullrange = TRUE) + 
  facet_grid(facets=Species~.)
g2

# Q4 : Histogrammes
g = ggplot(iris, aes(Sepal.Length)) + 
  geom_histogram(aes(y=..density..), bins = 8) + 
  labs(
    title="Distribution de la longeur de Sepale", 
    x="longueur de Sepale", 
    y="Densite"
    )
g
g1 = g + facet_grid(facets = Species~., scales = "free_y")
g1

#########################################################
#                                                       #
#                     EXERCICE 4                        #
#                                                       #
#########################################################
library(ggplot2)
library(scales)
library(tidyr)

# Question 1 : Gestion des l?gendes
g = ggplot(iris, aes(x="", fill=Species))+
  geom_bar()+
  labs(
    title="Distribution des especes d'iris", 
    x=" ", 
    y="Effectif"
    )
g
g1 = g+theme(legend.position = "left")
g1
g2 = g+labs(fill="Espece")
g2

# Question 2 : Nuages de points

table_longue = iris %>% gather(-Sepal.Width, -Species, key = "var", value="value")

g = ggplot(table_longue, aes(Sepal.Width,value)) +geom_point()+
    facet_grid(facets=var~., scales = "free_y")
g

# Question 3 
g3 = ggplot(table_longue, aes(Sepal.Width,value, color=Species)) +geom_point()+
    facet_grid(facets=var~., scales = "free_y")
g3


# Changer les noms des axes et de la l?gende, ajouter un titre
g3 = g3+
  labs(
    title="Nuages de points en fonction de la longueur de Sepale", 
    x="Longueur de Sepale", 
    y="Valeur", 
    color="Espece")
g3

#########################################################
#                                                       #
#                     EXERCICE 5                        #
#                                                       #
#########################################################
library(ggplot2)
library(scales)
library(forcats)

# Question 1 : Histogrammes exercice 3 
# Histogrammes avec couleurs
g = ggplot(iris, aes(Sepal.Length, color=Species))
g1 = g+geom_histogram(aes(y=..density..), bins = 8, fill="white")+
  facet_grid(facets=Species~., scales="free_y")+
  labs(
    title="Distribution de la longeur de Sepale", 
    x="longueur de Sepale", 
    y="Densite", 
    color="Espece"
  )+
  theme_light()+theme(legend.position = "none")
g1

spmean = iris %>% group_by(Species) %>% 
  summarise(
    mean=mean(Sepal.Length, na.rm = TRUE)
  )

g2 = g1+geom_vline(
  data=spmean, 
  mapping = aes(xintercept=mean, color=Species)
)+
  theme(legend.position = "none")
g2
# Question 2 
df = iris %>% group_by(Species) %>% 
  summarise(
    mean=mean(Sepal.Length, na.rm = T),
    sd = sd(Sepal.Length, na.rm = T)
    )

# Question 3 : Boites a moustaches

# Ci dessous le code de l'exercice 3, modifier celui ci pour r?pondre ? la question 
# g = ggplot(iris, aes(Species, Sepal.Length)) +geom_boxplot()+
#   labs(
#     title="Distribution de la longueur de sepale suivant l'espece", 
#     x=" ", 
#     y="Longueur de Sepale"
#   )
# g1

g = ggplot(iris, aes(Species, Sepal.Length, color=Species))+
  geom_boxplot(show.legend=F)+
  labs(
    title="Distribution de la longueur de sepale suivant l'espece",
    x=" ",
    y="Longueur de Sepale"
    )
g



# Question 4 

g1 = g+geom_errorbar(data = df, 
                     mapping = aes(y = mean, 
                                   ymin = mean - sd, 
                                   ymax = mean + sd), 
                     col = "gray70", width = .4, size = 1)
g1


g2 = g1+geom_point(data = df, 
             mapping = aes(y = mean), 
             col = "steelblue", size = 2) + theme_light()
g2


# Question 5

iris_mean = iris %>% left_join(df)

g = ggplot(
  iris_mean, 
  aes(fct_reorder(Species,mean,.desc=TRUE), Sepal.Length, color=Species)
  )+
  geom_boxplot()
g


## Pour am?liorer la boite ? moustache , je regais les m?me fonctions que la question pr?c?d?nte 
g1 = g+geom_errorbar(data = df, 
                     mapping = aes(y = mean, 
                                   ymin = mean - sd, 
                                   ymax = mean + sd), 
                     col = "gray70", width = .4, size = 1)
g1
g2 = g1+geom_point(data = df, 
                   mapping = aes(y = mean), 
                   col = "steelblue", size = 2) + theme_light()
g2

#########################################################
#                                                       #
#                     EXERCICE 6                        #
#                                                       #
#########################################################
library(ggplot2)
library(scales)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)

country = read_csv2("Donnees/WGICountry.csv")
indice = read_csv2("Donnees/WGIType.csv")
serie = read_csv2("Donnees/WGISerie.csv")
values = read_delim("Donnees/WGIValues.csv", delim=";") # Attention : read_csv2 importe les valeurs en charact?res

# Question 1 

# Retravialler la table pour lire les variables et modalites plus facilement
gouvernance = values %>% left_join(indice) %>% left_join(serie) %>% left_join(country) %>% select(-c(CountryCode,TypeCode,SerieCode))
gouvernance %>% View

# Extraction de la table large
estimees = values %>% filter(TypeCode == "EST")
estimees %>% View

# Creation de la table longue
estimees_longue = values %>% 
  gather(
    -CountryCode, -SerieCode, -TypeCode, 
    key = "annee", value = "value"
  ) %>% 
  filter(TypeCode=="EST")

# Ajout de l'indicateur de valeur manquante
valeurs_manquantes = estimees_longue %>% 
      mutate(manquant = is.na(value))


# Question 2 : Repr?sentaiton du pourcentage de valeurs manquantes en fonction de l'ann?e

# Diagramme en barres
g = ggplot(valeurs_manquantes, aes(manquant, fill=annee))+geom_bar()
g


# Construction de la table ? repr?senter 
evolution_na = valeurs_manquantes %>% 
  group_by(annee) %>% 
  summarise(perc_na=100*sum(manquant)/n()) 

# Repr?sentation graphique par une courbe
g1 = ggplot(evolution_na, aes(as.numeric(substring(annee, 2)),perc_na))+
  geom_line()+
  labs(
    title = "Valeurs manquantes dans les indicateurs (1996 - 2016)",
    y = "Pourcentage de valeurs manquantes"
  )
g1

# Question 3 :  Repr?sentation de l'?volution du contr?le de la corruption

# Construction de la table longue ? repr?senter
corruption = estimees_longue %>% filter(SerieCode == "CC")

# Representation en fonction de l'annee
g = ggplot(corruption, aes(annee, value)) +geom_boxplot()+
  labs(
    title = "Controle de la corruption entre 1996 et 2016",
    y = "Controle de la corruption"
    )
g



# Question 4 : Repr?sentation du controle moyen sur tous les pays en fonction de l'ann?e

# Construction de la table
corruption_moyen = corruption %>% 
  group_by(annee) %>% 
  summarise(CC_moyen = mean(value, na.rm = TRUE))


# Repr?sentation graphique (courbe)
g = ggplot(corruption_moyen, aes(as.numeric(substring(annee, 2)),CC_moyen))+ geom_line()+
  labs(
    title = "Controle moyen de la corruption entre 1996 et 2016",
    y = "Controle moyen de la corruption"
  )
g

# Ajout des 3 pays
corruption2 = corruption %>% filter(CountryCode %in% c("FRA", "SOM", "DNK"))

g2 = g+
  geom_line(corruption2, aes(as.numeric(substring(annee, 2)),  value, color = CountryCode ))+
  labs(
    title = "Controle de la corruption entre 1996 et 2016",
    x = "annee",
    y = "Controle de la corruption",
    color = "Pays"
  )
g2

#########################################################
#                                                       #
#                     EXERCICE 7                        #
#                                                       #
#########################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(geojson)
library(leaflet)
library(tibble)

# Importation des données de contour sous format spatial
etats = geojson_read(
  'https://datahub.io/core/geo-countries/r/countries.geojson',
  what = "sp")

# Construction de la carte avec
# - le fond de carte avec les noms des pays
# - les polygones avec des contours personnalisés
carte = leaflet(etats) %>% 
  setView(lat = 0, lng = 0, zoom = 1) %>%
  addTiles() %>%
  addPolygons(
    color = "gray30", 
    weight = 1.5, 
    opacity = 1, 
    dashArray = "2"
  )
carte

# Définition de la variable à représenter et de la palette
library(readr)
country = read_csv2("Donnees/WGICountry.csv")
indice = read_csv2("Donnees/WGIType.csv")
serie = read_csv2("Donnees/WGISerie.csv")
values = read_delim("Donnees/WGIValues.csv", delim=";") # Attention : read_csv2 importe les valeurs en charactères

# Extraction de la table des données de contrôle de la corruption en 2016
corruption16 = values %>% 
  filter(SerieCode == "CC" & TypeCode=="EST") %>% 
  select(CountryCode, Y2016)
colnames(corruption16) = c("ISO_A3", "CC16")
corruption16 %>% View

# Ajout des mesures de contrôle de la corruption à la table etats@data
# avec jointure sur les codes de pays
etats@data = etats@data %>% left_join(corruption16)

# Définition de la palette de couleurs associée au contrôle de la corruption en 2016
palette = colorBin(
  "magma", 
  domain = etats$CC16, 
  bins = 5
)

# Construction de la carte avec
# - le fond de carte avec les noms des pays
# - les polygones avec des contours personnalisés
# - les couleurs par mesure du contrôle de la corruption
# - la légende correspondante
carte1 = leaflet(etats) %>% 
  setView(lat = 0, lng = 0, zoom = 1) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette(CC16), 
    fillOpacity = .5,
    color = "gray70", 
    weight = 1.5, 
    opacity = 1, 
    dashArray = "2"
  ) %>%
  addLegend(
    pal = palette, 
    values = ~CC16, 
    opacity = 0.8, 
    title = "Controle de la corruption en 2016",
    position = "bottomright"
  )
# Affichage de la carte
carte1

