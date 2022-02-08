################################################################################
##                             PROJET : NETFLIX                               ##
##           Par CHENG Wen-Miin, DELMARE Thomas, ELALAOUI Hasnaa              ##
################################################################################

# BONUS

################
## QUESTION 1 ## ---------------------------------------------------------------
################

# 1 - Est-ce que c'est vrai que le temps moyen entre la sortie du film et son 
# ajout au carnet Netflix s'est raccourci ?

#extraction des sous ensembles de données par année
newdata_2008 <- subset(newdata,newdata$date_added==2008)
newdata_2009 <- subset(newdata,newdata$date_added==2009)
newdata_2010 <- subset(newdata,newdata$date_added==2010)
newdata_2011 <- subset(newdata,newdata$date_added==2011)
newdata_2012 <- subset(newdata,newdata$date_added==2012)
newdata_2013 <- subset(newdata,newdata$date_added==2013)
newdata_2014 <- subset(newdata,newdata$date_added==2014)
newdata_2015 <- subset(newdata,newdata$date_added==2015)
newdata_2016 <- subset(newdata,newdata$date_added==2016)
newdata_2017 <- subset(newdata,newdata$date_added==2017)
newdata_2018 <- subset(newdata,newdata$date_added==2018)
newdata_2019 <- subset(newdata,newdata$date_added==2019)
newdata_2020 <- subset(newdata,newdata$date_added==2020)
newdata_2021 <- subset(newdata,newdata$date_added==2021)

###Calcul des moyennes
#Moyenne de de la différence entre date d'ajout et la sortie
Moyenne <- sum(newdata$difference)/length(newdata$difference) #1ere méthode
Moyenne <- mean(newdata$difference) #2e méthode
Moyenne 

Moyenne_2008 <- round(mean(newdata_2008$difference),2)
Moyenne_2009 <- round(mean(newdata_2009$difference),2)
Moyenne_2010 <- round(mean(newdata_2010$difference),2)
Moyenne_2011 <- round(mean(newdata_2011$difference),2)
Moyenne_2012 <- round(mean(newdata_2012$difference),2)
Moyenne_2013 <- round(mean(newdata_2013$difference),2)
Moyenne_2014 <- round(mean(newdata_2014$difference),2)
Moyenne_2015 <- round(mean(newdata_2015$difference),2)
Moyenne_2016 <- round(mean(newdata_2016$difference),2)
Moyenne_2017 <- round(mean(newdata_2017$difference),2)
Moyenne_2018 <- round(mean(newdata_2018$difference),2)
Moyenne_2019 <- round(mean(newdata_2019$difference),2)
Moyenne_2020 <- round(mean(newdata_2020$difference),2)
Moyenne_2021 <- round(mean(newdata_2021$difference),2)

Moyenne_2008
Moyenne_2009 
Moyenne_2010
Moyenne_2011
Moyenne_2012
Moyenne_2013
Moyenne_2014
Moyenne_2015
Moyenne_2016
Moyenne_2017 
Moyenne_2018
Moyenne_2019
Moyenne_2020
Moyenne_2021


# On crée un nouveau dataframe avec les années non-dupliquées
dfParAnnee = newdata$date_added
dfParAnnee <- data.frame(dfParAnnee)
dfParAnnee <- dfParAnnee %>% group_by(dfParAnnee) %>% filter (! duplicated(dfParAnnee))
dfParAnnee

tab_moyenne = matrix(c(Moyenne_2008,Moyenne_2009,Moyenne_2010,Moyenne_2011,Moyenne_2012,
                       Moyenne_2013,Moyenne_2014,Moyenne_2015,Moyenne_2016,Moyenne_2017,
                       Moyenne_2018,Moyenne_2019,Moyenne_2020,Moyenne_2021))

dfParAnnee$moyenne_diff = tab_moyenne
View(dfParAnnee)
summary(dfParAnnee)


#group = dfParAnnee$dfParAnnee
#barplot(dfParAnnee$moyenne_diff,names.arg = group) 

plot(dfParAnnee$dfParAnnee,dfParAnnee$moyenne_diff,
     xlab="Années", ylab = "Années de différence",
     main="Différence entre date de sortie et date d'ajout sur Netflix en années")


################
## QUESTION 2 ##----------------------------------------------------------------
################

# 2 - A votre avis quelle est la cause de la tendance que vous remarquez au 
# point precedent ? Proposez une solution pour pouvoir donner une reponse plus 
# realiste au point 1) et refaites les calculs en ce sens. 

#extraction des sous ensemble de données par année
newdata2_2008 <- subset(newdata2,newdata2$date_added==2008)
newdata2_2009 <- subset(newdata2,newdata2$date_added==2009)
newdata2_2010 <- subset(newdata2,newdata2$date_added==2010)
newdata2_2011 <- subset(newdata2,newdata2$date_added==2011)
newdata2_2012 <- subset(newdata2,newdata2$date_added==2012)
newdata2_2013 <- subset(newdata2,newdata2$date_added==2013)
newdata2_2014 <- subset(newdata2,newdata2$date_added==2014)
newdata2_2015 <- subset(newdata2,newdata2$date_added==2015)
newdata2_2016 <- subset(newdata2,newdata2$date_added==2016)
newdata2_2017 <- subset(newdata2,newdata2$date_added==2017)
newdata2_2018 <- subset(newdata2,newdata2$date_added==2018)
newdata2_2019 <- subset(newdata2,newdata2$date_added==2019)
newdata2_2020 <- subset(newdata2,newdata2$date_added==2020)
newdata2_2021 <- subset(newdata2,newdata2$date_added==2021)

###Calcul des moyennes
#Moyenne de de la différence entre date d'ajout et la sortie
Moyenne <- sum(newdata$difference)/length(newdata$difference)
Moyenne <- mean(newdata$difference)
Moyenne 

Moyenne_2008 <- mean(newdata_2008$difference)
Moyenne_2009 <- mean(newdata_2009$difference)
Moyenne_2010 <- mean(newdata_2010$difference)
Moyenne_2011 <- mean(newdata_2011$difference)
Moyenne_2012 <- mean(newdata_2012$difference)
Moyenne_2013 <- mean(newdata_2013$difference)
Moyenne_2014 <- mean(newdata_2014$difference)
Moyenne_2015 <- mean(newdata_2015$difference)
Moyenne_2016 <- mean(newdata_2016$difference)
Moyenne_2017 <- mean(newdata_2017$difference)
Moyenne_2018 <- mean(newdata_2018$difference)
Moyenne_2019 <- mean(newdata_2019$difference)
Moyenne_2020 <- mean(newdata_2020$difference)
Moyenne_2021 <- mean(newdata_2021$difference)

Moyenne_2008
Moyenne_2009 
Moyenne_2010
Moyenne_2011
Moyenne_2012
Moyenne_2013
Moyenne_2014
Moyenne_2015
Moyenne_2016
Moyenne_2017 
Moyenne_2018
Moyenne_2019
Moyenne_2020
Moyenne_2021

Moyenne_H = (sum(Moyenne_2008,Moyenne_2009,Moyenne_2010,Moyenne_2011,
                 Moyenne_2012,Moyenne_2013, Moyenne_2014,Moyenne_2015,
                 Moyenne_2016,Moyenne_2017,Moyenne_2018,Moyenne_2019,
                 Moyenne_2020)/13)
Moyenne_A <- Moyenne_2021

Moyenne_H
Moyenne_A

# On crée un nouveau dataframe avec les années non-dupliquées
dfParAnnee2 = newdata2$date_added
dfParAnnee2 <- data.frame(dfParAnnee)
dfParAnnee2 <- dfParAnnee2 %>% group_by(dfParAnnee) %>% filter (! duplicated(dfParAnnee))
dfParAnnee2

tab_moyenne = matrix(c(Moyenne_2008,Moyenne_2009,Moyenne_2010,Moyenne_2011,Moyenne_2012,
                       Moyenne_2013,Moyenne_2014,Moyenne_2015,Moyenne_2016,Moyenne_2017,
                       Moyenne_2018,Moyenne_2019,Moyenne_2020,Moyenne_2021))

dfParAnnee2$moyenne_diff = tab_moyenne
View(dfParAnnee2)
summary(dfParAnnee2)

#group = dfParAnnee$dfParAnnee
#barplot(dfParAnnee$moyenne_diff,names.arg = group) 

#graphique evolution diff des années de différence
plot(dfParAnnee2$dfParAnnee,dfParAnnee2$moyenne_diff,
     xlab="Années", ylab = "Années de différence",
     main="Différence entre date de sortie et date d'ajout sur Netflix en années (sans valeurs aberrantes)")


################
## QUESTION 3 ##----------------------------------------------------------------
################

# 3 - Est-ce que c'est vrai que la proportion de show televises qui sont mis 
# au programme a considerablement augmenté par rapport aux autres programmes 
# (films, séries) ?

# Qst 3 v2

# 2008
Compte_films_2008 <- sum(newdata_2008$type=="Movie")
Compte_series_2008 <- sum(newdata_2008$type=="TV Show")
# 2009
Compte_films_2009 <- sum(newdata_2009$type=="Movie")
Compte_series_2009 <- sum(newdata_2009$type=="TV Show")
# 2010
Compte_films_2010 <- sum(newdata_2010$type=="Movie")
Compte_series_2010 <- sum(newdata_2010$type=="TV Show")
# 2011
Compte_films_2011 <- sum(newdata_2011$type=="Movie")
Compte_series_2011 <- sum(newdata_2011$type=="TV Show")
# 2012
Compte_films_2012 <- sum(newdata_2012$type=="Movie")
Compte_series_2012 <- sum(newdata_2012$type=="TV Show")
# 2013
Compte_films_2013 <- sum(newdata_2013$type=="Movie")
Compte_series_2013 <- sum(newdata_2013$type=="TV Show")
# 2014
Compte_films_2014 <- sum(newdata_2014$type=="Movie")
Compte_series_2014 <- sum(newdata_2014$type=="TV Show")
# 2015
Compte_films_2015 <- sum(newdata_2015$type=="Movie")
Compte_series_2015 <- sum(newdata_2015$type=="TV Show")
# 2016
Compte_films_2016 <- sum(newdata_2016$type=="Movie")
Compte_series_2016 <- sum(newdata_2016$type=="TV Show")
# 2017
Compte_films_2017 <- sum(newdata_2017$type=="Movie")
Compte_series_2017 <- sum(newdata_2017$type=="TV Show")
# 2018
Compte_films_2018 <- sum(newdata_2018$type=="Movie")
Compte_series_2018 <- sum(newdata_2018$type=="TV Show")
# 2019
Compte_films_2019 <- sum(newdata_2019$type=="Movie")
Compte_series_2019 <- sum(newdata_2019$type=="TV Show")
# 2020
Compte_films_2020 <- sum(newdata_2020$type=="Movie")
Compte_series_2020 <- sum(newdata_2020$type=="TV Show")
# 2021
Compte_films_2021 <- sum(newdata_2021$type=="Movie")
Compte_series_2021 <- sum(newdata_2021$type=="TV Show")


Moyenne_series_2008 <- (Compte_series_2008/(Compte_films_2008+Compte_series_2008))/100
Moyenne_series_2009 <- (Compte_series_2009/(Compte_films_2009+Compte_series_2009))/100
Moyenne_series_2010 <- (Compte_series_2010/(Compte_films_2010+Compte_series_2010))/100
Moyenne_series_2011 <- (Compte_series_2011/(Compte_films_2011+Compte_series_2011))/100
Moyenne_series_2012 <- (Compte_series_2012/(Compte_films_2012+Compte_series_2012))/100
Moyenne_series_2013 <- (Compte_series_2013/(Compte_films_2013+Compte_series_2013))/100
Moyenne_series_2014 <- (Compte_series_2014/(Compte_films_2014+Compte_series_2014))/100
Moyenne_series_2015 <- (Compte_series_2015/(Compte_films_2015+Compte_series_2015))/100
Moyenne_series_2016 <- (Compte_series_2016/(Compte_films_2016+Compte_series_2016))/100
Moyenne_series_2017 <- (Compte_series_2017/(Compte_films_2017+Compte_series_2017))/100
Moyenne_series_2018 <- (Compte_series_2018/(Compte_films_2018+Compte_series_2018))/100
Moyenne_series_2019 <- (Compte_series_2019/(Compte_films_2019+Compte_series_2019))/100
Moyenne_series_2020 <- (Compte_series_2020/(Compte_films_2020+Compte_series_2020))/100
Moyenne_series_2021 <- (Compte_series_2021/(Compte_films_2021+Compte_series_2021))/100

# On cree un nouveau dataframe avec les annees non-dupliquees
dfParAnnee_2 = newdata$date_added
dfParAnnee_2 <- data.frame(dfParAnnee_2)
dfParAnnee_2 <- dfParAnnee_2 %>% group_by(dfParAnnee_2) %>% filter (! duplicated(dfParAnnee_2))
dfParAnnee_2

tab_moyenne = matrix(c(Moyenne_series_2008,Moyenne_series_2009,Moyenne_series_2010,Moyenne_series_2011,
                       Moyenne_series_2012,Moyenne_series_2013,Moyenne_series_2014,Moyenne_series_2015,
                       Moyenne_series_2016,Moyenne_series_2017,Moyenne_series_2018,Moyenne_series_2019,
                       Moyenne_series_2020,Moyenne_series_2021))

dfParAnnee_2$proportion_series = tab_moyenne
View(dfParAnnee_2)
summary(dfParAnnee_2)


plot(dfParAnnee_2$dfParAnnee_2,dfParAnnee_2$proportion_series,
     xlab="Années", ylab = "Proportion de séries",
     main="Evolution de la proportion de séries au fil des ans")


################
## QUESTION 4 ##----------------------------------------------------------------
################

# 4 - Même question que la precedente mais en prenant en compte la durée en minutes.

