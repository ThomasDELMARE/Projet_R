################################################################################
##                             PROJET : NETFLIX                               ##
##           Par CHENG Wen-Miin, DELMARE Thomas, ELALAOUI Hasnaa              ##
################################################################################

# script pour générer les graphiques 

################
## QUESTION 1 ## ---------------------------------------------------------------
################

# 1 - Est-ce que c'est vrai que le temps moyen entre la sortie du film et son 
# ajout au carnet Netflix s'est raccourci ?


################
## QUESTION 2 ##----------------------------------------------------------------
################

# 2 - A votre avis quelle est la cause de la tendance que vous remarquez au 
# point precedent ? Proposez une solution pour pouvoir donner une reponse plus 
# realiste au point 1) et refaites les calculs en ce sens. 


#avec le traitement des données
netflix<-newdata2
netflix%>%
  group_by(date_added, type)%>%
  summarise(n=n())%>%
  ggplot(aes(date_added, color=type))+
  geom_density(aes(fill=type), alpha=2/10)+
  theme_test()+
  labs(
    x='Date',
    y='Proportion',
    title='Proportion de shows télévisés comparés aux films')

added_titles_month_hist <- ggplot(mydata, aes(x=mydata$date_added, fill=type)) +
  theme_bw() +
  geom_histogram(stat="count", color="black") +
  labs(x='Date d ajout',y='Nombre de films et séries',title="Distribution of added movies and TV shows by year")
print(added_titles_month_hist)


#matrice de corrélation visuelle grâce à corrplot
corrplot(
  cor_newdata2_num
)

################
## QUESTION 3 ##----------------------------------------------------------------
################

# 3 - Est-ce que c'est vrai que la proportion de show televises qui sont mis 
# au programme a considerablement augmenté par rapport aux autres programmes 
# (films, séries) ?


################
## QUESTION 4 ##----------------------------------------------------------------
################

# 4 - Même question que la precedente mais en prenant en compte la durée en minutes.

#Nombre de séries par durée 
dfParAnne_Parduree_Series = subset(newdata,type=="TV Show")

View(dfParAnne_Parduree_Series)
dfParAnne_Parduree_Series = dfParAnne_Parduree_Series[,c(7,10)]

dfParAnne_Parduree_Series %>% count(duration, sort = TRUE) 

dfParAnne_Parduree_Series %>% count(duration, sort = TRUE) %>%
  ggplot(aes(x = duration, y = n))+
  geom_col()+
  labs(x = "Durée", y = "Nombre de séries", title = "Nombre de séries en fonction de la durée")

#Nombre de films par durée 
dfParAnne_Parduree_Films = subset(newdata,type=="Movie")

View(dfParAnne_Parduree_Films)
dfParAnne_Parduree_Films = dfParAnne_Parduree_Films[,c(7,10)]

dfParAnne_Parduree_Films %>% count(duration, sort = TRUE) 

dfParAnne_Parduree_Films %>% count(duration, sort = TRUE) %>%
  ggplot(aes(x = duration, y = n))+
  geom_col()+
  labs(x = "Durée", y = "Nombre de films", title = "Nombre de films en fonction de la durée")

