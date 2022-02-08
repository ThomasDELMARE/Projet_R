################################################################################
##                             PROJET : NETFLIX                               ##
##           Par CHENG Wen-Miin, DELMARE Thomas, ELALAOUI Hasnaa              ##
################################################################################

#Script pour le pré-traitement des données

# Installation des packages requis et activation des librairies

install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")

library(lubridate)
library(dplyr) 
library(ggplot2)
library(corrplot)

#Importation des données
mydata <- read.csv2("netflix_titles.csv", header = TRUE, sep = ",", dec = " ")
View(mydata)
summary(mydata)
dim(mydata)
str(mydata)


################
## QUESTION 1 ## ---------------------------------------------------------------
################

# 1 - Est-ce que c'est vrai que le temps moyen entre la sortie du film et son 
# ajout au carnet Netflix s'est raccourci ?

## action sur le dataset de base
# changement du format de la variable char en date
mydata$date_added = mdy(mydata$date_added)
# On passe du format de date complet au format année
mydata$date_added = format(mydata$date_added, format="%Y")
# On passe du format string en int
mydata$date_added = strtoi(mydata$date_added)

#création dataset avec une colonne en plus
mydata_diff = mydata
#calcul de la différence d'années entre la date d'ajout sur Netflix et la date de sortie
mydata_diff$difference = mydata$date_added - mydata$release_year
#voir un résumé des données
summary(mydata_diff)
#dimension du dataset
dim(mydata_diff)

#nombre de NA
sum(is.na(mydata_diff$date_added))
sum(is.na(mydata_diff$release_year))

# pour gérer les NA dans notre dataset, nous avons décidé de supprimer les 10 NA
# car nous avons toujours une grande base de données avec 8797 données aprés suppression, car il n'y a que 10 NA
#suppression des NA dans les dates d'ajouts
mydata_diff_sans_na = mydata_diff[!is.na(mydata_diff$date_added),]

#verifier la suppression des NA
length(mydata$date_added)
length(mydata_diff_sans_na$date_added)
sum(is.na(mydata_diff_sans_na$date_added))

#tri des données
newdata <- mydata_diff_sans_na[order(mydata_diff_sans_na$date_added),] # tri en fonction de la colonne date_added
length(newdata$date_added)
head(newdata,1)
View(newdata)

#étendu des dates d'ajout sur Netflix
# nous observons que l'étendu est de 2008 à 2021
range(newdata$date_added)

#séparation des données historiques (H) et données échantillons (A)
H <- subset(newdata,newdata$date_added!=2021)
A <- subset(newdata,newdata$date_added==2021)
range(H$date_added) #étendu du 2018 à 2020
range(A$date_added) #étendu sur uniquement 2021

# selection uniquement des films
H_films = subset(H,H$type=="Movie")
A_films = subset(A,A$type=="Movie")


################
## QUESTION 2 ##----------------------------------------------------------------
################

# 2 - A votre avis quelle est la cause de la tendance que vous remarquez au 
# point précédent ? Proposez une solution pour pouvoir donner une réponse plus 
# réaliste au point 1) et refaites les calculs en ce sens. 

#observation des données qui sont négatifs en termes de différence
difference1 <- subset(newdata,difference==-3)
View(difference1)

difference2 <- subset(newdata,difference==-2)
View(difference2)

difference3 <- subset(newdata,difference==-1)
View(difference3)

# suppression des valeurs aberrantes
mydata_diff_sans_na_2 = newdata[-c(7113,5659,5678,5395,4845,4846,7064,3169,3288,3370,3434,1552,1697,2921),]
newdata2 = mydata_diff_sans_na_2[order(mydata_diff_sans_na_2$date_added),]

# separation échantillon H et A après traitement
newdata2_H <- subset(newdata2, newdata2$date_added!=2021)
newdata2_A <- subset(newdata2, newdata2$date_added==2021)


################
## QUESTION 3 ##----------------------------------------------------------------
################

# 3 - Est-ce que c'est vrai que la proportion de show télévisées qui sont mis 
# au programme a considérablement augmenté par rapport aux autres programmes 
# (films, séries) ?

# On compte le nombre de séries et de films de 2008 à 2020
series_H=0
films_H =0
total_H=0
for (i in 1:nrow(newdata2_H)){
  if (newdata2_H$type[i]=="TV Show"){
    series_H=series_H+1
  }
  else { 
    films_H=films_H+1
  }
  total_H=total_H+1
}
series_H
films_H 
total_H

# On compte le nombre de séries et de films de 2021
series_A=0
films_A =0
total_A=0
for (i in 1:nrow(newdata2_A)){
  if (newdata2_A$type[i]=="TV Show"){
    series_A=series_A+1
  }
  else { 
    films_A=films_A+1
  }
  total_A=total_A+1
}
series_A
films_A 
total_A


################
## QUESTION 4 ##----------------------------------------------------------------
################

# 4 - Même question que la precedente mais en prenant en compte la durée en minutes.

# on suppose qu'une saison est composée de 12 épisodes et que chaque épisode dure 30 min
duree_saison <- 12*30 

#Traitement des lignes vides par remplacement en NA pour l'échantillon H
newdata2_H[newdata2_H==''] <- NA
#Suppression des NA
newdata2_H <- newdata2_H[!is.na(newdata2_H$duration),] 

# Separation des series et films et transformation pour ne récupérer que les int
newdata_H_series <- subset(newdata2_H, newdata2_H$type=="TV Show")
newdata_H_series <- strtoi(substring(newdata_H_series$duration,0,1))
newdata_H_films <- subset(newdata2_H, newdata2_H$type=="Movie")
newdata_H_films <- strtoi(gsub(' min', '', newdata_H_films$duration))


#Traitement des lignes vides par remplacement en NA pour l'échantillon A
newdata2_A[newdata2_A==''] <- NA
newdata2_A <- newdata2_A[!is.na(newdata2_A$duration),] 

# Separation des series et films et transformation pour ne récupérer que les int
newdata_A_series <- subset(newdata2_A, newdata2_A$type=="TV Show")
newdata_A_series <- strtoi(substring(newdata_A_series$duration,0,1))
newdata_A_films <- subset(newdata2_A, newdata2_A$type=="Movie")
newdata_A_films <- strtoi(gsub(' min', '', newdata_A_films$duration))

