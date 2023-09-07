setwd("ATTENTION METTRE CHEMIN VERS VOTRE ENV DE TRAVAIL")

#################### !!!! Attention !!!! ########################
#  Conflit entre les packages dplyr et MASS
#  Si select() ne fonctionne pas, dÃ©tacher le package MASS
#  detach(package:MASS)
#  Penser Ã  sauvegarder les donnÃ©es si utilisÃ©es Ã  partir de MASS
#################################################################

#########################################################
#                                                       #
#                     EXERCICE 1                        #
#                                                       #
#########################################################
library(dplyr)
library(tibble)

iris %>% select(Species) %>% distinct
iris %>% slice(1:60)
iris %>% slice((n()-49):n())
iris %>% slice(seq(1,n(), by=10))
iris %>% summary
iris %>% filter(Sepal.Length>=6.4 & Petal.Width>=1.8)
iris %>% filter(Sepal.Length>=6.4 & Petal.Width>=1.8) %>% group_by(Species) %>% summarise(n=n())
org_iris = iris %>% arrange(Species,Sepal.Length)
org_iris %>% View
ratio = org_iris %>% mutate(sep_ratio=Sepal.Length/Sepal.Width, pet_ratio=Petal.Length/Petal.Width)
ratio %>% View

library(readr)
country = read_csv2("Donnees/WGICountry.csv")
indice = read_csv2("Donnees/WGIType.csv")
serie = read_csv2("Donnees/WGISerie.csv")
values = read_delim("Donnees/WGIValues.csv", delim=";") # Attention : read_csv2 importe les valeurs en charactÃ¨res

gouvernance = values %>% left_join(indice) %>% left_join(serie) %>% left_join(country) %>% select(-c(CountryCode,TypeCode,SerieCode))
gouvernance %>% View