#Head#
library(dplyr)
library(ggplot2)
library(INWTstyles)

par(family = "sans")

# INWT-ggplot-Theme setzen
setThemeGgplot2(base_size = 11)

###############
#Vorbereitung:#
###############
fbData <- read.csv("datasetFacebook.csv", sep = ";")

fbData$Post.Month <- sprintf("%02d", fbData$Post.Month)
fbData$Month <- paste0("2017-",fbData$Post.Month,"-01") 

fbData <- fbData %>% 
  mutate(Category = as.factor(Category)) %>% 
  mutate(Post.Month = as.factor(Post.Month)) %>% 
  mutate(Post.Weekday = as.factor(Post.Weekday)) %>% 
  mutate(Post.Hour = as.factor(Post.Hour)) %>% 
  mutate(Paid = as.factor(Paid)) %>% 
  mutate(Date = as.Date(Month,"%Y-%m-%d"))

fbData <- fbData[complete.cases(fbData),]


###############
#Deskriptives:#
###############

#7 unabhängige Variablen
#Entwicklung der Gesamten Likes (der Reichweite)
 
dataReichweite <- fbData %>%
  select(Date, Page.total.likes) %>% 
  group_by(Date) %>% 
  summarise(Reichweite = mean(Page.total.likes)) %>% 
  arrange(desc(Reichweite))

plotReichweite <- ggplot(dataReichweite, aes(Date, Reichweite)) +
  geom_line()
plotReichweite


#Anteil der Posts je Typ pro Monat/pro Gesamt
fbData$zielVar <-  fbData$Total.Interactions
fbData$unabhVar <- fbData$Type


dataAnteile <- fbData %>% 
  select(unabhVar,  zielVar) %>%
  group_by(unabhVar) %>% 
  summarise(zielVar = mean(zielVar)) %>% 
  mutate(anteil = (zielVar / sum(zielVar)) * 100)

#Balkendiagramm
plotAnteile <- ggplot(dataAnteile, aes(unabhVar, zielVar )) +
  geom_bar(stat = "identity")
plotAnteile  


#Torten-Diagramm
plotPieAnteile <- ggplot(dataAnteile, aes(x = "", y = anteil, fill = unabhVar),
                         logo = F) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)
plotPieAnteile   




#Anteile der Posts je Categorie pro Monat / pro Gesamt
#Torten-Diagramm


#




###############
#Predictions: #
###############

#Erklärungsgehalt der unabhängigen Variablen auf die abhängige Variable(n)
#Variieren zwischen den Zielvariablen
#Tortendiagramm
#(wie bei Kundensegmentierung von Fashionette?)


#Vorhersage der Reichweite / des Engagements eines fiktiven Posts mit den 
#jeweiligen Eigenschaften