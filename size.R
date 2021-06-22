setwd("C:/Users/saret/Downloads/size-main")

#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggpubr")
#install.packages("cowplot")
#install.packages("gridExtra")
#install.packages("ggplot2")
#install.packages("ggExtra")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggExtra)
library(ggplot2)

traits <- read.table("floral_traits.txt",header=T) %>%
  group_by(Species,Flowers,Individual) %>%
  dplyr::summarize(Surface=mean(Surface.mm2.),Diameter=mean(Diameter),Depth=mean(Depth)) %>%
  dplyr::filter(Species=="Echium") %>%
  #dplyr::filter(Flowers=="Large") %>%
  dplyr::filter(!is.na(Surface))


ggqqplot(traits$Surface)
ggqqplot(traits$Diameter)

res.aov <- aov((Surface) ~ Flowers, data = traits)
summary(res.aov)




beetraits <- read.table("bee_traits2.txt",header=T) 


ggqqplot(sqrt(beetraits$Distal_cell))


res.aov <- aov((Distal_cell) ~ Bees, data = beetraits )
summary(res.aov)



seedset <- read.table("fruits.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  dplyr::filter(Fruits==1) %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Seed_set=mean(Seeds))%>%
  dplyr::filter(Flowers == "Large") 

ggqqplot((seedset$Seed_set))

res.aov <- aov((Seed_set) ~ Bees, data = seedset)
summary(res.aov)


fruitset <- read.table("fruits.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Fruit_set=mean(Fruits), Fecundity = mean(Seeds))%>%
  dplyr::filter(Flowers == "Small") 

ggqqplot(fruitset$Fruit_set)

model <- glm(Fruit_set~ Bees, data = fruitset , family = binomial)
summary(model)



svpd <- read.table("pollen.txt",header=T) %>%
  group_by(Species,Flowers,Bees,Individual,Measurement) %>%
  dplyr::summarize(Grains=mean(Grains)) %>%
  dplyr::filter(Species=="Borago") %>%
  #dplyr::filter(Flowers=="Large") %>%
  mutate(Pollen_presence=if_else(Grains>0,1,0))%>%
  dplyr::filter(Measurement=="SVPD")


ggqqplot((svpd$Grains))

hist((svpd$Grains))

res.aov <- aov((Grains) ~ Flowers, data = svpd )
summary(res.aov)

ggplot(data = svpd , aes(x=Species, y=Grains,fill=Flowers)) + 
  geom_boxplot(aes(fill=Flowers))+
  theme_classic()


surveys_summarised <- surveys %>%
  group_by(Treatment,Species,Flowers,Bees, Bout) %>% 
  dplyr::summarize(Time=mean(Time_seconds),Flowers_visited=mean(Flowers_visited),Time_flower=mean(Time_flower)) %>%
  dplyr::filter(!is.na(Time_flower)) %>%
  mutate(Flowers_per_minute=Flowers_visited/Time*60)

svpd <- surveys_summarised %>%
  dplyr::filter(Species=="Echium") %>%
  dplyr::filter(Flowers=="Small") 


ggqqplot((svpd$Time_flower))

hist(log(svpd$Time_flower))

res.aov <- aov(log(Time_flower) ~ Bees, data = svpd )
summary(res.aov)







