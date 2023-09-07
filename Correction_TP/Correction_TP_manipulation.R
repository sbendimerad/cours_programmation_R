setwd("ATTENTION METTRE CHEMIN VERS VOTRE ENV DE TRAVAIL")

#################### !!!! Attention !!!! ########################
#  Conflit entre les packages dplyr et MASS
#  Si select() ne fonctionne pas, détacher le package MASS
#  detach(package:MASS)
#  Penser à sauvegarder les données si utilisées à partir de MASS
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
iris %>% filter(Sepal_Length>=6.4 & Petal_Width>=1.8)
iris %>% filter(Sepal_Length>=6.4 & Petal_Width>=1.8) %>% group_by(Species) %>% summarise(n=n())

org_iris = iris %>% arrange(Species,Sepal_Length)
org_iris %>% View
   
ratio = org_iris %>% mutate(sep_ratio=Sepal_Length/Sepal_Width, pet_ratio=Petal_Length/Petal_Width)

library(readr)
country = read_csv2("Cours/Donnees/WGICountry.csv")
indice = read_csv2("Cours/Donnees/WGIType.csv")
serie = read_csv2("Cours/Donnees/WGISerie.csv")
values = read_delim("Cours/Donnees/WGIValues.csv", delim=";") # Attention : read_csv2 importe les valeurs en charactères

gouvernance = values %>% left_join(indice) %>% left_join(serie) %>% left_join(country) %>% select(-c(CountryCode,TypeCode,SerieCode))
gouvernance %>% View

#########################################################
#                                                       #
#                     EXERCICE 2                        #
#                                                       #
#########################################################
library(stringr)

x = c("Je","suis","en","DUT","STID")

x[6] = "alternance"

str_length(x)

x[6]=str_to_title(x[6])
x

str_subset(x,"e")

y = str_c(x, collapse = " ")
y
str_length(y)

str_length(x) %>% sum
str_split(y, " ")

#########################################################
#                                                       #
#                     EXERCICE 3                        #
#                                                       #
#########################################################
library(dplyr)
library(tibble)
library(stringr)

# Question 1
starwars
starwars %>% View

# Question 2
starwars %>% select(name) %>% distinct %>% nrow

# Quelques tests en plus
is.na(starwars$name)
#is.na(starwars)s

# Question 3 
var_na = (starwars %>% is.na %>% colSums) / nrow(starwars)*100
var_na = var_na %>% as_tibble 

# Question 4
var_na_per = (starwars %>% is.na %>% rowSums) / ncol(starwars)*100
var_na_per = var_na_per %>% as_tibble 

# Quelques informations en plus sur le jeu de donnees
## Voir la fonction bind_cols pour rapprocher 2 colonnes 
## Lancer la commande ?bind_cols et lire la documentaiton de la fonction
SW_var_name = starwars %>% colnames %>% as_tibble
SW_var_na = SW_var_name %>% bind_cols(var_na)
SW_var_na %>% View
SW_var_na %>% filter(value1>0)

# Une maniere differente de repondre a la Question 4
starwars_na=starwars %>% transmute(perc_na=rowSums(is.na(starwars))/ncol(starwars)*100)
starwars_na %>% View

# Question 5 
starwars %>% select(species) %>% distinct %>% View
starwars %>% select(species) %>% distinct %>% nrow

# Question 6 
starwars %>% group_by(species) %>% summarise(n=n()) %>% arrange(desc(n)) # Il y a des valeurs manquantes !!
# Meme reponse enregistree dans la table "races"
races=starwars %>% filter(!is.na(species)) %>% group_by(species) %>% summarise(n=n())

## Comprendre la notion de !is.na(starwars$species)
starwars %>% filter(!is.na(species)) %>% group_by(species) %>% summarise(n=n()) %>% arrange(desc(n)) # sans les valeurs manquantes

# Question 7 
races %>% filter(n>=2)

# Je choisis Droid et Gungan
starwars %>% filter(species=="Droid") %>% select(name)
starwars %>% filter(species=="Gungan") %>% select(name)
starwars %>% filter(is.na(species)) %>% select(name)


# Optionnel 1 
## Essais pas ? pas
tri_plus=str_match(starwars$hair_color, "(.+)[,](.+)")
dim(tri_plus)
which(!is.na(tri_plus[,1]))
## On se lance !!!
starwars_vieux=starwars %>% mutate(tri_plus=str_match(starwars$hair_color, "(.+)[,](.+)")[,1])
starwars_vieux %>% filter(!is.na(tri_plus)) %>% select(name)


# Optionnel 2
## Essai pas ? pas
episodes = starwars %>% select(films)
episodes %>% slice(1) %>% typeof # type d'objet
episodes %>% slice(1) %>% length # longueur = 1
episodes %>% slice(1) %>% unlist # sépare les éléments de la liste
episodes %>% slice(1) %>% unlist %>% length # nombre d'éléments dans la liste
## On se lance !!
# Cr?er une nouvelle variable
episodes = starwars %>% select(films)
n=starwars %>% nrow
Nb = tibble(nb_films=numeric(n))
for(i in 1:n){
  # Nb %>% slice(i) = episodes %>% slice(i) %>% unlist() %>% length() ne fonctionne pas
  Nb[i,1] = episodes %>% slice(i) %>% unlist %>% length
}
## Ajouter la nouvelle variable ? la table
starwars_plus = starwars %>% mutate(nb_films=Nb) # ne fonctionne pas
# Utiliser bind_cols ou add_column
starwars_plus = starwars %>% bind_cols(Nb) # Gère mieux les noms de colonnes ajoutées 
# starwars_plus = starwars %>% add_column(Nb)
starwars_plus %>% filter(nb_films>4) %>% select(name)

#########################################################
#                                                       #
#                     EXERCICE 4                        #
#                                                       #
#########################################################
library(dplyr)
library(tibble)

# Question 1
iris %>% summary

# Question 2
# Creation de la table resumant les mesures de Sepales
Sepals = iris %>% group_by(Species) %>% 
  summarise(n=n(), 
            av.Sepal.Length=mean(Sepal.Length), med.Sepal.Length=median(Sepal.Length),
            av.Sepal.Width=mean(Sepal.Width), med.Sepal.Width=median(Sepal.Width))
Sepals %>% View

# Creation de la table resumant les mesures de e?tales
Petals = iris %>% group_by(Species) %>% 
  summarise(n=n(), 
            av.Petal.Length=mean(Petal.Length), med.Petal.Length=median(Petal.Length),
            av.Petal.Width=mean(Petal.Width), med.Petal.Width=median(Petal.Width))
Petals %>% View 

# Question 3
# Creation de la table resumant toutes les mesures
table_j = Petals %>% inner_join(Sepals)
table_j %>% View 

# Question 4
iris %>% select(Species) %>% table %>% barplot
iris %>% select(Species) %>% table %>% pie

# Question 5
starwars %>% select(eye_color, hair_color) %>% table %>% barplot()
