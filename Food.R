library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(plotly)
library(car)
library(lme4)
library(caret)
library(randomForest)
library(xgboost)
library(rmarkdown)
library(stringr)

'quarto install tinytex'

food1 <- read_csv("C:/Users/SAIL/Documents/world_food_production.csv")


View(food1)

?kable
food2 <- filter(food1, Entity == "Nigeria")
print("Subset with 'Nigeria':")
View(food2)
food3 <- food2[, 1:7]
View(food3)


food1%>%
  group_by(Entity)%>%
  summarise(total= sum(`Maize Production (tonnes)`))%>%
  ggplot(aes(x= Entity, y= total, fill = Entity ))+
  geom_col()



ggplotly(food1%>%
           group_by(Entity)%>%
           summarise(total= sum(`Maize Production (tonnes)`))%>%
           ggplot(aes(x= Entity, y= total, fill = Entity))+
           geom_col()+
           theme(axis.text.x = element_text(angle=90))+
           theme_classic()+
           coord_flip())

food2%>%
  group_by(Entity)%>%
  ggplot(aes(x= Year, y= `Rice  Production ( tonnes)`, fill = Year))+
  geom_col()

ggplotly(food2%>%
           group_by(Entity)%>%
           ggplot(aes(x= Year, y= `Rice  Production ( tonnes)`, fill = Year))+
           geom_col())

nigpop <- read_csv("C:/Users/SAIL/Desktop/Nigeria_Population.csv")
View(nigpop)


nigpop1 <- nigpop[order(nigpop$Year), ]

View(nigpop1)
nigpop2 <- nigpop1[12:72,]
food4 <- data.frame(food2,nigpop2$Population)
View(food4)

food4 <- food4%>%
  rename(Population_times_10000= "nigpop2.Population")
food4$Population_times_10000 <- food4$Population_times_10000*10^-4



ggplotly(food5%>%
  group_by(Entity)%>%
  ggplot(aes(x= Year, y= values,colour =variables))+
  geom_line()+
  geom_point()+
  geom_smooth(method = "lm"))


?rename


food5<- food4%>%
  pivot_longer( cols =c(Rice..Production...tonnes.,Population_times_10000), names_to = "variables", values_to = "values" )

model <- lm(Rice..Production...tonnes.~ Population_times_10000,food4)
summary(model)
