#Shiny Project

#explanations:
#It includes 7 features known prior to post publication and 12 features for evaluating post impact 
#(see Tables 2 and 3 from Moro et al., 2016 - complete reference in the 'Citation Request')

#Head#
library(dplyr)
library(ggplot2)
library(INWTstyles)

par(family = "sans")

# INWT-ggplot-Theme setzen
setThemeGgplot2(base_size = 11)

#Head Ende#


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


sum(is.na(fbData)) # 6 rows have NAs -> remove those 

fbData <- fbData[complete.cases(fbData),]


#Variable für anteil der leute die reagieren
fbData <- fbData %>%  
  mutate(reactingShare = Total.Interactions/Page.total.likes)

#a) change the target variable: 1: Total.Interactions, 2: Likes, 3:Shares, 4:Comments
#b) change the timely horizon: Month, Weekday, Hour

targetVariable <- c(fbData$Total.Interactions, fbData$Likes, fbData$Shares, fbData$Comments, fbData$reactingShare)

plotMonths <- ggplot(fbData, aes(x = Post.Month, y = targetVariable[5])) +
  geom_bar(stat = "identity")
plotMonths

plotWeekday <- ggplot(fbData, aes(x = Post.Weekday, y = targetVariable[5])) +
  geom_bar(stat = "identity")
plotWeekday

plotHour <- ggplot(fbData, aes(x = Post.Hour, y = targetVariable[5])) +
  geom_bar(stat = "identity")
plotHour #plot hour seems to have mistakes - most interactions during night !?

plotHour <- ggplot(fbData, aes(x = Post.Hour, y = comment)) +
  geom_bar(stat = "identity")
plotHour #plot hour seems to have mistakes - most interactions during night !?


plotType <- ggplot(fbData, aes(x = Type, y = targetVariable[5])) +
  geom_bar(stat = "identity")
plotType 


plotPaid <- ggplot(fbData, aes(x = Paid, y = targetVariable[5])) +
  geom_bar(stat = "identity")
plotPaid 


#bad results for linear regression
lm <- lm(reactingShare ~ -1 + Type + Category + Paid + Post.Month + Post.Weekday + Post.Hour, data = fbData)
summary(lm)




#high correlations
cor(fbData$Total.Interactions, fbData$like) #0.997702
cor(fbData$Total.Interactions, fbData$share) #0.9286243
cor(fbData$Total.Interactions, fbData$comment) #0.8648447


ggplot(fbData, aes(x = Total.Interactions, y = like)) +
  geom_line() +
  geom_rug(sides = "b") 
#ein großer Ausreiser


ggplot(fbData, aes(x = Total.Interactions, y = like)) +
  geom_boxplot(fill = "#6b99ce") +
  xlab("Total Interactions") +
  ylab("Likes")

#wenige richtige gute -> finde was diese so gut macht 


#einschub: Timeline:
timeline <- ggplot(fbData, aes(x = Page.total.likes, y = reactingShare)) +
  geom_point()+
  geom_smooth(method = "lm") 
timeline
#kein echtes Muster gefunden...

one <- fbData$Type
two <- fbData$Category
three <- fbData$Post.Month
four <- fbData$Post.Weekday
five <- fbData$Post.Hour

#playing with the variables that shall be coloured
# Change color and shape by groups (cyl)
ggplot(fbData, aes(x = Page.total.likes, y = reactingShare)) +
  geom_point(aes(color = two)) +
  geom_smooth(aes(color = two, fill = two), method = "lm") + 
  geom_rug(aes(color = two))


# Einschub Ende
fbDataRanking <- arrange(fbData$reactingShare)
fbDataRanking



ggplot(fbData, aes(x = Post.Weekday, y = reactingShare)) + geom_boxplot()

ggplot(fbData, aes(x = Type, y = reactingShare)) + geom_boxplot()
ggplot(fbData, aes(x = Category, y = reactingShare)) + geom_boxplot()
#....



plotData <- fbData %>%
  select(Date,Type,reactingShare) %>% 
  group_by(Type, Date) %>% 
  summarise(meanReaction = mean(reactingShare)) %>% 
  arrange(desc(meanReaction))


#Zeithorizont:
ggplot(plotData, aes(Date, meanReaction, color = Type)) +
geom_line()
# +
  #  xlab("Monat") + 
  # theme(legend.position="top",
  #       legend.title = element_blank())




