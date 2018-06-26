#zuerst die Analyse:

#Daten müssen noch anonymisiert werden, so dass Ergebnisse brauchbar bleiben,
#aber die Daten fiktiv sind (wegen Copyrights)

#Originalquelle: https://archive.ics.uci.edu/ml/datasets/Facebook+metrics


#Ziele: 
# - Performance von Posts vorhersagen
# - Lifetime Post Consumer als Zielgröße
# - type des Posts als wichtigster Einfluss (36% Relevanz)
# - "a status post captures around twice the attention of the remaining
#   three types (link, photo, video)"
# - 


# - output Variablen: Kommentare, Likes, Shares


library(dplyr)


fbData <- read.csv("datasetFacebook.csv", sep = ";")



fbData <- fbData %>%
  mutate(Post.Month = as.factor(Post.Month), 
         Post.Weekday = as.factor(Post.Weekday),
         Post.Weekday = as.factor(Post.Weekday),) 



%>% 
  mutate(Post.Weekday = as.factor(Post.Weekday)) %>% 
  mutate(Post.Hour = as.factor(Post.Hour)) %>% 
  mutate(Paid = as.factor(Paid)) %>% 
  mutate(Category = as.factor(Category))
  
str(fbData)

#umzusetzende Ideen:

#Anzahl der Likes, Comments, Shares in Histogram mit Filtermöglichkeiten, wie 
#Tag, Monat, Uhrzeit, Typ des Posts




#Regression, um Performance eines hypothetischen Posts vorherzusagen
# - anspruchsvolle Regressionsgrafik mit Unsicherheitsband
# - Regression dynamisch einfärben, je nach Kriterien


#Aufteilen eines hypothetischen Budgets


#Top 5 Posts und lowest 5 Posts...

#Ein Clustering von Blogposts erstellen mit 4 Quadranten