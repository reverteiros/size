
setwd("C:/Users/saret/Desktop/Clips 4.3")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)

####♥
surveys <- read.table("surveys3.txt",header=T) %>%
  dplyr::filter(Species=="Echium")

ggplot(data = surveys, aes(x=Flowers, y=Time_flower,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()

surveys_summarised <- surveys %>%
  mutate(Flowers_per_minute = Flowers_visited*60/Time_seconds)%>%
  group_by(Treatment,Species,Flowers,Bees, Bout) %>% 
  dplyr::summarize(Time=mean(Time_seconds),Flowers_visited=mean(Flowers_visited),Time_flower=mean(Time_flower),Flowers_per_minute=mean(Flowers_per_minute)) %>%
  dplyr::filter(!is.na(Time_flower))

# %>% 
#  group_by(Treatment,Species,Bees) %>% 
#  dplyr::summarize(Nbouts=n()) 

ggplot(data = surveys_summarised, aes(x=Flowers, y=Flowers_per_minute,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  labs(y = "Flowers per minute")+


surveys <- read.table("surveys.txt",header=T) %>%
  dplyr::filter(Species=="Borago")

ggplot(data = surveys, aes(x=Flowers, y=Time_flower,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()

surveys_summarised <- surveys %>%
  mutate(Flowers_per_minute = Flowers_visited*60/Time_seconds)%>%
  group_by(Treatment,Species,Bees, Bout) %>% 
  dplyr::summarize(Time=mean(Time_seconds),Flowers=mean(Flowers_visited),Time_flower=mean(Time_flower),Flowers_per_minute=mean(Flowers_per_minute)) %>%
  dplyr::filter(!is.na(Time_flower))

# %>% 
#  group_by(Treatment,Species,Bees) %>% 
#  dplyr::summarize(Nbouts=n()) 

ggplot(data = surveys_summarised, aes(x=Species, y=Flowers_per_minute,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  labs(y = "Flowers per minute")

traits <- read.table("traits4.txt",header=T) %>%
  dplyr::filter(!is.na(Diameter)) %>%
  dplyr::filter(Species=="Echium")


ggplot(data = traits, aes(x=Flower, y=Diameter)) + 
  geom_boxplot(aes(fill=Flower))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 30))+
  theme(legend.position = "none")



fruits <- read.table("fruits2.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  dplyr::filter(Fruits==1) %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Fecundity=mean(Seeds))
                   
ggplot(data = fruits, aes(x=Treatment, y=Fecundity)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 4))+
  theme(legend.position = "none")



res.aov <- aov(Fecundity ~ Bees, data = fruits)
summary(res.aov)


pollen <- read.table("pollen.txt",header=T) %>%
  dplyr::filter(Measurement == "Stigma") %>%
  dplyr::filter(Flowers =="Large")

ggplot(data = pollen, aes(x=Flowers, y=Grains,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()

res.aov <- aov(Grains ~ Bees, data = pollen)
summary(res.aov)

hist(pollen$Grains)
