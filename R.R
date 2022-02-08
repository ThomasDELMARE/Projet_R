################################################################################
##                             PROJET : NETFLIX                               ##
##           Par CHENG Wen-Miin, DELMARE Thomas, ELALAOUI Hasnaa              ##
################################################################################

# Script pour effectuer les statistiques et les vérifications d'hypothèses

################
## QUESTION 1 ## ---------------------------------------------------------------
################

# 1 - Est-ce que c'est vrai que le temps moyen entre la sortie du film et son 
# ajout au carnet Netflix s'est raccourci ?

#calcul de la moyenne des différences pour historique et échantillons
Moyenne_H = round(mean(H$difference), 2)
Moyenne_H

Moyenne_A = round(mean(A$difference), 2)
Moyenne_A

################################# Hypothèses ################################### 
# H0 : le temps moyen entre la sortie du film et son ajout au carnet Netflix 
#       s'est raccourci (tps<moy) temps < 4,47 
# H1 : le temps moyen entre la sortie du film et son ajout au carnet Netflix 
#       ne s'est pas raccourci (tps>=moy)
#################################################################################

# calcul des moyennes des différences pour échantillons H et A 
H_films_moy = mean(H_films$difference)
A_films_moy = mean(A_films$difference)
H_films_moy
A_films_moy

# On calcule l'écart-type des deux échantillons
H_films_ec = sd(H_films$difference)
A_films_ec = sd(A_films$difference)
H_films_ec
A_films_ec 

# On calcule le T obs
T = (A_films_moy - H_films_moy ) / (A_films_ec / sqrt(count(A_films)))
T # 7.307045

# on fixe alpha = 5% 

# TEST D'HYPOTHESES
t.test(
  A_films$difference,
  H_films$difference,
  alternative = "less",
  mu = A_films_moy,
  var.equal = FALSE,
  paired = FALSE,
  conf.level = 0.95
)

#p-value = 2.2e-16 < 0.05 donc on rejette H0 et on conclut H1
# Donc le temps moyen entre la sortie du film et son ajout au carnet Netflix ne s'est pas raccourci


################
## QUESTION 2 ##----------------------------------------------------------------
################

# 2 - A votre avis quelle est la cause de la tendance que vous remarquez au 
# point precedent ? Proposez une solution pour pouvoir donner une reponse plus 
# realiste au point 1) et refaites les calculs en ce sens. 

################################# Hypothèses ################################### 
# H0 : il y a une corrélation entre la date de sortie d'un film et sa date d'ajout sur Netflix
# H1 : il n'y a pas de corrélation entre la date de sortie d'un film et sa date d'ajout sur Netflix
################################################################################

###Calcul des moyennes
#Moyenne de de la différence entre date d'ajout et la sortie
# pour l'échantillon H puis pour l'échantillon A 
Moyenne2_H = round(mean(newdata2_H$difference),2)
Moyenne2_A = round(mean(newdata2_A$difference),2)

Moyenne2_H
Moyenne2_A

#visualisation des variables numériques
summary(newdata2)

#extraction des colonnes de type numérique
newdata2_num <- newdata2[,c(7,8,13)]

#Corrélation des valeurs numériques
cor(newdata2_num,method = "spearman")
cor(newdata2_num)
cor_newdata2_num <- cor(newdata2_num)

#TEST DE CORRELATION
cor.test(newdata2$date_added,newdata2$release_year) # 0.1117905 
cor.test(newdata2$date_added,newdata2$difference) # 0.06727881 
cor.test(newdata2$release_year,newdata2$difference) # -0.9839591 

# Il n'y a pas de corrélation entre  la date de sortie 
# et celle d'ajout d'un film ou d'une série sur Netflix

# L'hypothèse H0 est donc rejetée et on en conclut l'hypothèse H1


################
## QUESTION 3 ##----------------------------------------------------------------
################

# 3 - Est-ce que c'est vrai que la proportion de show televises qui sont mis 
# au programme a considerablement augmenté par rapport aux autres programmes 
# (films, series) ?

#Calcul des proportions
propH <- series_H/total_H
propH
propA <- series_A/total_A
propA

#TEST DE PROPORTION
prop.test(series_A,total_A,p=propH,"greater",correct=FALSE)

# Intervalle [0.3173358 , 1.0000000]
# p-value = 0.000276
# probabilité : p = 0.3371162 
# Donc p est dans l'intervalle avec 0.000276 d'erreur
# Ainsi, puisque p-value<0.05 => H0 est vraie


################
## QUESTION 4 ##----------------------------------------------------------------
################

# 4 - Même question que la precedente mais en prenant en compte la durée en minutes.

############################## Hypothèses ######################################
# H0 : la proportion de la durée des show televises qui sont mis au programme 
#       a considerablement augmenté par rapport aux autres programmes 
# H1 : la proportion de la durée des show televises qui sont mis au programme 
#       n'a pas considerablement augmenté par rapport aux autres programmes 
################################################################################

# Calcul de la durée totale en minutes pour l'échantillon H
duree_totale_series_H_saison <- sum(newdata_H_series)
duree_totale_series_H_min <- duree_totale_series_H_saison*duree_saison
duree_totale_films_H_min <- sum(newdata_H_films)
duree_totale_H <- duree_totale_series_H_min + duree_totale_films_H_min

# Calcul de la durée totale en minutes pour l'échantillon A
duree_totale_series_A_saison <- sum(newdata_A_series)
duree_totale_series_A_min <- duree_totale_series_A_saison*duree_saison
duree_totale_films_A_min <- sum(newdata_A_films)
duree_totale_A <- duree_totale_series_A_min + duree_totale_films_A_min

# TEST DE PROPORTION
propH_duree <- duree_totale_series_H_min/duree_totale_H
propH_duree
prop.test(duree_totale_series_A_min,duree_totale_A,p=propH_duree,"greater",correct=FALSE)

# Intervalle [0.7692089 , 1.0000000]
# probabilité : p = 0.7702484 
# Ainsi, puisque p-value<0.05 => H0 est vraie

