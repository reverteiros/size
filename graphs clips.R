setwd("C:/Users/saret/Desktop/size")

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggpubr")
# install.packages("cowplot")
# install.packages("gridExtra")
# install.packages("ggplot2")
# install.packages("ggExtra")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggExtra)
library(ggplot2)
library(grid)

traits <- read.table("floral_traits.txt",header=T) 

colortflowers <- c("grey24","grey89")

m1 <-ggplot(data = traits, aes(x=Species, y=Diameter,fill=Flowers)) + 
  geom_boxplot(aes(fill=Flowers))+
  theme_classic()+
  scale_fill_manual(values=colortflowers)+
  # theme(legend.position = "none") +
  labs(y = "Diameter (cm)")

traitsbees <- read.table("bee_traits.txt",header=T) 

m2 <-ggplot(data = traitsbees , aes(x=Bees, y=Measurement)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  scale_fill_manual(values=colortflowers)+
  # theme(legend.position = "none") +
  labs(y = "Body size (cm)") 


ggarrange(m1, m2, ncol = 2, nrow = 1,labels = c("(a)", "(b)"))




ABdataframe <- as.data.frame(AB)











surveys <- read.table("surveys.txt",header=T) 

surveys_summarised <- surveys %>%
  group_by(Treatment,Species,Flowers,Bees, Bout) %>% 
  dplyr::summarize(Time=mean(Time_seconds),Flowers_visited=mean(Flowers_visited),Time_flower=mean(Time_flower)) %>%
  dplyr::filter(!is.na(Time_flower)) %>%
  mutate(Flowers_per_minute=Flowers_visited/Time*60)

m3 <- ggplot(data = surveys_summarised , aes(x=Flowers, y=Time_flower)) + 
  geom_boxplot(aes(fill=Bees))+
  scale_fill_manual(values=colortflowers)+
  facet_grid(. ~ Species)+
  theme_classic()+
  coord_cartesian(ylim = c(0, 30))

ggarrange(m3, ncol = 1, nrow = 1,labels = c("(c)"))

m4 <- ggplot(data = surveys_summarised , aes(x=Flowers, y=Flowers_per_minute)) + 
  geom_boxplot(aes(fill=Bees))+
  scale_fill_manual(values=colortflowers)+
  facet_grid(. ~ Species)+
  theme_classic()

ggarrange(m4, ncol = 1, nrow = 1,labels = c("(d)"))




pollen_svpd <- read.table("pollen.txt",header=T) %>%
  group_by(Species,Flowers,Bees,Individual,Measurement) %>%
  dplyr::summarize(Grains=mean(Grains)) %>%
  dplyr::filter(Measurement=="SVPD")

m5 <- ggplot(data = pollen_svpd, aes(x=Flowers, y=Grains)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  scale_fill_manual(values=colortflowers)+
  facet_grid(. ~ Species)+
  coord_cartesian(ylim = c(0, 130))+
  labs(y = "SVPD") 

ggarrange(m5, ncol = 1, nrow = 1,labels = c("(e)"))

pollen_stigma <- read.table("pollen.txt",header=T) %>%
  group_by(Species,Flowers,Bees,Individual,Measurement) %>%
  dplyr::summarize(Grains=mean(Grains)) %>%
  dplyr::filter(Measurement=="Stigma")

m6 <- ggplot(data = pollen_stigma, aes(x=Flowers, y=Grains)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  scale_fill_manual(values=colortflowers)+
  facet_grid(. ~ Species)+
  coord_cartesian(ylim = c(0, 180))+
  labs(y = "Life-long pollen") 

ggarrange(m6, ncol = 1, nrow = 1,labels = c("(f)"))



fruitset <- read.table("fruits.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Fruit_set=mean(Fruits))

m7 <-ggplot(data = fruitset, aes(x=Flowers, y=Fruit_set,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  scale_fill_manual(values=colortflowers)+
  theme(legend.position = "none") +
  labs(y = "Fruit set")



seedset <- read.table("fruits.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  dplyr::filter(Fruits==1) %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Seed_set=mean(Seeds))

m8 <-ggplot(data = seedset, aes(x=Flowers, y=Seed_set,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  scale_fill_manual(values=colortflowers)+
  # theme(legend.position = "none") +
  labs(y = "Seed set")



ggarrange(m7, m8, ncol = 2, nrow = 1,labels = c("(g)", "(h)"))





