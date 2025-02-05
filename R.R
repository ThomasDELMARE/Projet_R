################################################################################
##                             PROJET : NETFLIX                               ##
##           Par CHENG Wen-Miin, DELMARE Thomas, ELALAOUI Hasnaa              ##
################################################################################

# Script pour effectuer les statistiques et les v�rifications d'hypoth�ses

################
## QUESTION 1 ## ---------------------------------------------------------------
################

# 1 - Est-ce que c'est vrai que le temps moyen entre la sortie du film et son 
# ajout au carnet Netflix s'est raccourci ?

#calcul de la moyenne des diff�rences pour historique et �chantillons
Moyenne_H = round(mean(H$difference), 2)
Moyenne_H

Moyenne_A = round(mean(A$difference), 2)
Moyenne_A

################################# Hypoth�ses ################################### 
# H0 : le temps moyen entre la sortie du film et son ajout au carnet Netflix 
#       s'est raccourci (tps<=moy) temps <= 5.34 
# H1 : le temps moyen entre la sortie du film et son ajout au carnet Netflix 
#       ne s'est pas raccourci (tps>moy) temps > 5.34
################################################################################

# calcul des moyennes des diff�rences pour �chantillons H et A 
H_films_moy = mean(H_films$difference)
A_films_moy = mean(A_films$difference)
H_films_moy
A_films_moy

# On calcule l'�cart-type des deux �chantillons
H_films_ec = sd(H_films$difference)
A_films_ec = sd(A_films$difference)
H_films_ec
A_films_ec 

# On calcule le T obs
T = (A_films_moy - H_films_moy) / (A_films_ec / sqrt(count(A_films)))
T # 7.307045

# on fixe alpha = 5% 

# TEST D'HYPOTHESES
# t.test(
#   A_films$difference,
#   H_films$difference,
#   alternative = "less",
#   mu = H_films_moy,
#   var.equal = FALSE,
#   paired = FALSE,
#   conf.level = 0.95
# )

t.test(A_films$difference, H_films$difference, alternative = "greater")

#p-value = 1.146e-11 < 0.05 donc on rejette H0 et on conclut H1
# Donc le temps moyen entre la sortie du film et son ajout au carnet Netflix ne s'est pas raccourci


################
## QUESTION 2 ##----------------------------------------------------------------
################

# 2 - A votre avis quelle est la cause de la tendance que vous remarquez au 
# point precedent ? Proposez une solution pour pouvoir donner une reponse plus 
# realiste au point 1) et refaites les calculs en ce sens. 

################################# Hypoth�ses ################################### 
# H0 : il y a une corr�lation entre la date de sortie d'un film et sa date d'ajout sur Netflix
# H1 : il n'y a pas de corr�lation entre la date de sortie d'un film et sa date d'ajout sur Netflix
################################################################################

###Calcul des moyennes
#Moyenne de de la diff�rence entre date d'ajout et la sortie
# pour l'�chantillon H puis pour l'�chantillon A 
Moyenne2_H = round(mean(newdata2_H$difference),2)
Moyenne2_A = round(mean(newdata2_A$difference),2)

Moyenne2_H
Moyenne2_A

#visualisation des variables num�riques
summary(newdata2)

#extraction des colonnes de type num�rique
newdata2_num <- newdata2[,c(7,8,13)]

#Corr�lation des valeurs num�riques
cor(newdata2_num)
cor_newdata2_num <- cor(newdata2_num)

#TEST DE CORRELATION
cor.test(newdata2$date_added,newdata2$release_year) # 0.1117905 
cor.test(newdata2$date_added,newdata2$difference) # 0.06727881 
cor.test(newdata2$release_year,newdata2$difference) # -0.9839591 

# Il n'y a pas de corr�lation entre  la date de sortie 
# et celle d'ajout d'un film ou d'une s�rie sur Netflix

# L'hypoth�se H0 est donc rejet�e et on en conclut l'hypoth�se H1


################
## QUESTION 3 ##----------------------------------------------------------------
################

# 3 - Est-ce que c'est vrai que la proportion de show televises qui sont mis 
# au programme a considerablement augment� par rapport aux autres programmes 
# (films, series) ?

################################# Hypoth�ses ################################### 
# H0 : la proportion de show t�l�vis�s qui sont mis au programme a considerablement
#       augment� par rapport aux autres programmes 
# H1 : la proportion de show t�l�vis�s qui sont mis au programme n'a pas 
#       considerablement augment� par rapport aux autres programmes 
################################################################################

#Calcul des proportions
propH <- series_H/total_H
propH
propA <- series_A/total_A
propA

#TEST DE PROPORTION
prop.test(series_A,total_A,p=propH,"greater",correct=FALSE)

# Intervalle [0.3173358 , 1.0000000]
# p-value = 0.000276
# valeur estim�e : p = 0.3371162 
# Donc p est dans l'intervalle avec 0.000276 d'erreur
# Ainsi, puisque p-value < 0.05 => rejet H0 et H1 est vraie


################
## QUESTION 4 ##----------------------------------------------------------------
################

# 4 - M�me question que la precedente mais en prenant en compte la dur�e en minutes.

############################## Hypoth�ses ######################################
# H0 : la proportion de la dur�e des show televises qui sont mis au programme 
#       a considerablement augment� par rapport aux autres programmes 
# H1 : la proportion de la dur�e des show televises qui sont mis au programme 
#       n'a pas considerablement augment� par rapport aux autres programmes 
################################################################################

# Calcul de la dur�e totale en minutes pour l'�chantillon H
duree_totale_series_H_saison <- sum(newdata_H_series)
duree_totale_series_H_min <- duree_totale_series_H_saison*duree_saison
duree_totale_films_H_min <- sum(newdata_H_films)
duree_totale_H <- duree_totale_series_H_min + duree_totale_films_H_min

# Calcul de la dur�e totale en minutes pour l'�chantillon A
duree_totale_series_A_saison <- sum(newdata_A_series)
duree_totale_series_A_min <- duree_totale_series_A_saison*duree_saison
duree_totale_films_A_min <- sum(newdata_A_films)
duree_totale_A <- duree_totale_series_A_min + duree_totale_films_A_min

# TEST DE PROPORTION
propH_duree <- duree_totale_series_H_min/duree_totale_H
propH_duree
propA_duree <- duree_totale_series_A_min/duree_totale_A
propA_duree
prop.test(duree_totale_series_A_min,duree_totale_A,p=propH_duree,"greater",correct=FALSE)

# Intervalle [0.7692089 , 1.0000000]
# valeur estim�e : p = 0.7702484 
# Ainsi, puisque p-value < 0.05 => rejet H0, H1 est vraie

