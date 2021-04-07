
setwd("C:/Users/saret/Desktop/Clips 4.3")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)


############### Time per flower
surveys <- read.table("surveys.txt",header=T) 

ggplot(data = surveys, aes(x=Bees, y=Time_flower,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)+
  coord_cartesian(ylim = c(0, 20))

surveys_borago <- surveys %>%
  dplyr::filter(Species=="Borago")

res.aov <- aov(Time_flower ~ Flowers, data = surveys_borago)
summary(res.aov)
wilcox.test(Time_flower ~ Flowers, data = surveys_borago)

ggplot(data = surveys_borago, aes(x=Flowers, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


res.aov <- aov(Time_flower ~ Bees, data = surveys_borago)
summary(res.aov)
wilcox.test(Time_flower ~ Bees, data = surveys_borago)
ggplot(data = surveys_borago, aes(x=Bees, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_borago_small <- surveys %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Time_flower ~ Bees, data = surveys_borago_small)
summary(res.aov)
wilcox.test(Time_flower ~ Bees, data = surveys_borago_small)
ggplot(data = surveys_borago_small, aes(x=Bees, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_borago_large <- surveys %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Time_flower ~ Bees, data = surveys_borago_large)
summary(res.aov)
wilcox.test(Time_flower ~ Bees, data = surveys_borago_large)
ggplot(data = surveys_borago_large, aes(x=Bees, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_echium <- surveys %>%
  dplyr::filter(Species=="Echium")

res.aov <- aov(Time_flower ~ Flowers, data = surveys_echium)
summary(res.aov)
wilcox.test(Time_flower ~ Flowers, data = surveys_echium)
ggplot(data = surveys_echium, aes(x=Flowers, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


res.aov <- aov(Time_flower ~ Bees, data = surveys_echium)
summary(res.aov)
wilcox.test(Time_flower ~ Bees, data = surveys_echium)
ggplot(data = surveys_echium, aes(x=Bees, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_echium_small <- surveys %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Time_flower ~ Bees, data = surveys_echium_small)
summary(res.aov)
wilcox.test(Time_flower ~ Bees, data = surveys_echium_small)
ggplot(data = surveys_echium_small, aes(x=Bees, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_echium_large <- surveys %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Time_flower ~ Bees, data = surveys_echium_large)
summary(res.aov)
wilcox.test(Time_flower ~ Bees, data = surveys_echium_large)
ggplot(data = surveys_echium_large, aes(x=Bees, y=Time_flower)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))



############### FLowers per minute



surveys_summarised <- surveys %>%
  group_by(Treatment,Species,Flowers,Bees, Bout) %>% 
  dplyr::summarize(Time=mean(Time_seconds),Flowers_visited=mean(Flowers_visited),Time_flower=mean(Time_flower)) %>%
  dplyr::filter(!is.na(Time_flower)) %>%
  mutate(Flowers_per_minute=Flowers_visited/Time*60)

# %>% 
#  group_by(Treatment,Species,Bees) %>% 
#  dplyr::summarize(Nbouts=n()) 

ggplot(data = surveys_summarised, aes(x=Bees, y=Flowers_per_minute,fill=Bees)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  labs(y = "Flowers per minute")+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)


surveys_borago <- surveys_summarised %>%
  dplyr::filter(Species=="Borago")

res.aov <- aov(Flowers_per_minute ~ Flowers, data = surveys_borago)
summary(res.aov)
wilcox.test(Flowers_per_minute ~ Flowers, data = surveys_borago)
surveys_echium_largeggplot(data = surveys_borago, aes(x=Flowers, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


res.aov <- aov(Flowers_per_minute ~ Bees, data = surveys_borago)
summary(res.aov)
wilcox.test(Flowers_per_minute ~ Bees, data = surveys_borago)
ggplot(data = surveys_borago, aes(x=Bees, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_borago_small <- surveys_borago %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Flowers_per_minute ~ Bees, data = surveys_borago_small)
summary(res.aov)
wilcox.test(Flowers_per_minute ~ Bees, data = surveys_borago_small)
ggplot(data = surveys_borago_small, aes(x=Bees, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_borago_large <- surveys_borago %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Flowers_per_minute ~ Bees, data = surveys_borago_large)
summary(res.aov)
wilcox.test(Flowers_per_minute ~ Bees, data = surveys_borago_large)
ggplot(data = surveys_borago_large, aes(x=Bees, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_echium <- surveys_summarised %>%
  dplyr::filter(Species=="Echium")

res.aov <- aov(Flowers_per_minute ~ Flowers, data = surveys_echium)
summary(res.aov)
ggplot(data = surveys_echium, aes(x=Flowers, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


res.aov <- aov(Flowers_per_minute ~ Bees, data = surveys_echium)
summary(res.aov)
ggplot(data = surveys_echium, aes(x=Bees, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_echium_small <- surveys_echium %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Flowers_per_minute ~ Bees, data = surveys_echium_small)
summary(res.aov)
wilcox.test(Flowers_per_minute ~ Bees, data = surveys_echium_small)
ggplot(data = surveys_echium_small, aes(x=Bees, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))


surveys_echium_large <- surveys_echium %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Flowers_per_minute ~ Bees, data = surveys_echium_large)
summary(res.aov)
wilcox.test(Flowers_per_minute ~ Bees, data = surveys_echium_large)
ggplot(data = surveys_echium_large, aes(x=Bees, y=Flowers_per_minute)) + 
  geom_boxplot()+
  theme_classic()+
  coord_cartesian(ylim = c(0, 20))
