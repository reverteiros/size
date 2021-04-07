
setwd("C:/Users/saret/Desktop/Clips 4.3")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)


traits <- read.table("floral_traits.txt",header=T) %>%
  group_by(Species,Flowers,Individual) %>%
  dplyr::summarize(Surface=mean(Surface.mm2.),Diameter=mean(Diameter),Depth=mean(Depth))


ggplot(data = traits, aes(x=Species, y=Depth,fill=Flowers)) + 
  geom_boxplot(aes(fill=Flowers))+
  theme_classic()+
  # theme(legend.position = "none") +
  labs(y = "Surface (cm2)")


############# Analysis on Echium. Comparison traits
traits_Echium <- traits %>%
  dplyr::filter(Species == "Echium") 

ggplot(data = traits_Echium, aes(x=Surface)) + 
  geom_histogram()+
  theme_classic()+
  # theme(legend.position = "none") +
  labs(y = "Surface (cm2)") +
  facet_grid(. ~ Flowers)


res.aov <- aov(Surface ~ Flowers, data = traits_Echium)
summary(res.aov)
wilcox.test(Surface ~ Flowers, data = traits_Echium)

res.aov <- aov(Diameter ~ Flowers, data = traits_Echium)
summary(res.aov)
wilcox.test(Diameter ~ Flowers, data = traits_Echium)

res.aov <- aov(Depth ~ Flowers, data = traits_Echium)
summary(res.aov)
wilcox.test(Depth ~ Flowers, data = traits_Echium)

############# Analysis on Borago. Comparison traits
traits_Borago <- traits %>%
  dplyr::filter(Species == "Borago") 

ggplot(data = traits_Borago, aes(x=Surface)) + 
  geom_histogram(binwidth = 0.5)+
  theme_classic()+
  # theme(legend.position = "none") +
  labs(y = "Surface (cm2)") +
  facet_grid(. ~ Flowers)


res.aov <- aov(Surface ~ Flowers, data = traits_Borago)
summary(res.aov)
wilcox.test(Surface ~ Flowers, data = traits_Borago)

res.aov <- aov(Diameter ~ Flowers, data = traits_Borago)
summary(res.aov)
wilcox.test(Diameter ~ Flowers, data = traits_Borago)

wilcox.test(Depth ~ Flowers, data = traits_Borago)

