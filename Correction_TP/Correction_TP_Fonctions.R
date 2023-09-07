rm(ls=list())
graphics.off()


#########################################################
#                                                       #
#                     EXERCICE 1                        #
#                                                       #
#########################################################

# Question 1

u1 = 0
for(i in 1:10000){
  u1 = u1+1/i
 }

u2 = 0

for (i in seq(2, 10000, by = 2)){
  u2 = u2+1/i
  }


# Question 2 


u3 = 0
for(i in sort(1:10000, decreasing = TRUE)){
  u3 = u3+1/i
  }
 


u4 = 0
for(i in seq(10000, 1, by=-2)){
  u4 = u4+1/i
 }

### Question 3 : Ajout des conditions

u1 = 0
for(i in 1:10000){
  if(i %in% c(198, 2067, 532, 8934)) u1 = u1 else u1 = u1+1/i
}

u2 = 0
for(i in seq(2, 10000, by = 2)){
  if(i %in% c(198, 2067, 532, 8934)) u2 = u2 else u2 = u2+1/i
}



#########################################################
#                                                       #
#                     EXERCICE 2                        #
#                                                       #
#########################################################

# Question 1

somme = function(n){
  u = 0
  for(i in 1:n){
    u = u+1/i
  }
  u
}

somme_pair = function(n){
  u = 0
  for(i in seq(2,n, by = 2)){
    u = u+1/i
  }
  u
}



somme(10000)
somme_pair(10000)
somme(50)
somme_pair(100)

# Question 2 : Fonction avec param?tre

serie = function(n, pair = FALSE){
  
  if(pair == TRUE) 
    somme_pair(n) 
  else 
    somme(n)
}


serie(10000)
serie(10000,pair=TRUE)
serie(10000, pair = TRUE)
serie(1e+7)
serie(1e+7, pair = TRUE)

#########################################################
#                                                       #
#                     EXERCICE 3                        #
#                                                       #
#########################################################

somme_impair = function(n){
  u = 0
  for(i in seq(1,n, by = 2)){
    u = u+1/i
  }
  u
}

series = function(n){
  u1 = somme(n)
  u2 = somme_pair(n)
  u3 = somme_impair(n)
  resultat = list(n = n, u = c(u1,u2,u3))
  resultat
}

series(10000)
series(1e+7)

#########################################################
#                                                       #
#                     EXERCICE 4                        #
#                                                       #
#########################################################
library(dplyr)

# Moyennes et m?dianes
iris %>% summarise_if(is.numeric, list(mean, median))
# Avec le package purrr
library(purrr)
iris %>% summarise_if(is.numeric, list(~mean(.), ~median(.)))

# Moyennes par esp?ce
iris %>% group_by(Species) %>% summarise_all(mean)
