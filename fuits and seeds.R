
setwd("C:/Users/saret/Desktop/size")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)


seedset <- read.table("fruits.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  dplyr::filter(Fruits==1) %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Seed_set=mean(Seeds))

seedsetgraph <- ggplot(data = seedset, aes(x=Bees, y=Seed_set, fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  coord_cartesian(ylim = c(1, 4))+
  theme(legend.position = "none")+
  facet_grid(. ~ Flowers)

### compare sizes of bees
res.aov <- aov(Seed_set ~ Bees, data = seedset)
summary(res.aov)
wilcox.test(Seed_set ~ Bees, data = seedset)

ggplot(data = seedset, aes(y=Seed_set,x=Bees)) + 
  geom_boxplot()+
  theme_classic()


############# Seed set
seedset_large <- seedset %>%
  dplyr::filter(Flowers == "Large") 

ggplot(data = seedset_large, aes(x=Seed_set)) + 
  geom_histogram()+
  theme_classic()+
  # theme(legend.position = "none") +
  # labs(y = "Surface (cm2)") +
  facet_grid(. ~ Bees)

res.aov <- aov(Seed_set ~ Bees, data = seedset_large)
summary(res.aov)
wilcox.test(Seed_set ~ Bees, data = seedset_large)


seedset_small <- seedset %>%
  dplyr::filter(Flowers == "Small") 

ggplot(data = seedset_small, aes(x=Seed_set)) + 
  geom_histogram()+
  theme_classic()+
  # theme(legend.position = "none") +
  # labs(y = "Surface (cm2)") +
  facet_grid(. ~ Bees)

res.aov <- aov(Seed_set ~ Bees, data = seedset_small)
summary(res.aov)
wilcox.test(Seed_set ~ Bees, data = seedset_small)




############# Fecundity & Fruit set
fruitset <- read.table("fruits.txt",header=T) %>%
  dplyr::filter(Species=="Echium") %>%
  group_by(Treatment,Individual,Bees, Flowers) %>%
  dplyr::summarize(Fruit_set=mean(Fruits), Fecundity = mean(Seeds))

fruitsetgraph <- ggplot(data = fruitset, aes(x=Bees, y=Fruit_set,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  # coord_cartesian(ylim = c(1, 4))+
  theme(legend.position = "none")+
  facet_grid(. ~ Flowers)

fecunditygraph <- ggplot(data = fruitset, aes(x=Bees, y=Fecundity,fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  # coord_cartesian(ylim = c(1, 4))+
  theme(legend.position = "none")+
  facet_grid(. ~ Flowers)

ggplot(data = fruitset, aes(x=Fruit_set)) + 
  geom_histogram()+
  # theme_classic()+
  facet_grid(Flowers ~ Bees) #flowers rows bees columns


res.aov <- aov(Fruit_set ~ Bees, data = fruitset)
summary(res.aov)
wilcox.test(Fruit_set ~ Bees, data = fruitset)

ggplot(data = fruitset, aes(y=Fruit_set,x=Bees)) + 
  geom_boxplot()+
  theme_classic()


fruitset_large <- fruitset %>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Fruit_set ~ Bees, data = fruitset_large)
summary(res.aov)
wilcox.test(Fruit_set ~ Bees, data = fruitset_large)

res.aov <- aov(Fecundity ~ Bees, data = fruitset_large)
summary(res.aov)
wilcox.test(Fecundity ~ Bees, data = fruitset_large)


fruitset_small <- fruitset %>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Fruit_set ~ Bees, data = fruitset_small)
summary(res.aov)
wilcox.test(Fruit_set ~ Bees, data = fruitset_small)

res.aov <- aov(Fecundity ~ Bees, data = fruitset_small)
summary(res.aov)
wilcox.test(Fecundity ~ Bees, data = fruitset_small)




ggarrange(fruitsetgraph,fecunditygraph,seedsetgraph, ncol = 1, nrow = 3,labels = c("(a)", "(b)", "(c)"))
###• export en 5 x 8 inches

