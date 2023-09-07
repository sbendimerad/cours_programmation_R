
# Conditions ---------------------------------------------------------------
## Juste avec if
a = 5 
if (a>0){
  print("a est postif")
}

## En ajoutant else
b = 6 
if (b>=0){
  print("b est postif")
} else {
  print("b est negatif")
}

## Synthaxe en une seule ligne
ifelse(b>0,"b est postif","b est negatif" )

# Boucles -----------------------------------------------------------------

## for
for(i in (1:5)){
  print(i)
}

## while
i = 8 
while(i > 5 ){
  print(i)
  i = i - 1
}


# Fonctions ---------------------------------------------------------------

## fonctions sans retour
preocedure1 = function(name_user){
  print(paste0("Hello: ", name_user))
  
}

preocedure1("andrea")


## fonctions avec retour
preocedure2 = function(n){
  y = n + 5
  y
}

preocedure2(5)


## fonctions avec paramètres par défaut
preocedure2 = function(n, s = 5){
  y = n + s
  y
}

preocedure2(5)
preocedure2(5,3)