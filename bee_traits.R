
setwd("C:/Users/saret/Desktop/size")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)


traits <- read.table("bee_traits_retocada.txt",header=T) 
  group_by(Species,Flowers,Individual) %>%
  dplyr::summarize(Surface=mean(Surface.mm2.),Diameter=mean(Diameter),Depth=mean(Depth))

ggplot(data = traits, aes(x=Bees, y=Measurement)) + 
    geom_boxplot(aes(fill=Bees))+
    theme_classic()+
    # theme(legend.position = "none") +
    labs(y = "Distal cell length (cm)")
  
ggplot(data = traits, aes(x=Bees, y=Measurement, fill=Flowers)) + 
  geom_boxplot(aes(fill=Flowers))+
  theme_classic()+
  # theme(legend.position = "none") +
  labs(y = "Distal cell length (cm)")



res.aov <- aov(Measurement ~ Bees, data = traits)
summary(res.aov)

wilcox.test(Measurement ~ Bees, data = traits)


traits <- read.table("bee_traits_retocada.txt",header=T) %>%
  dplyr::filter(Bees=="Small")

res.aov <- aov(Measurement ~ Flowers, data = traits)
summary(res.aov)

wilcox.test(Measurement ~ Flowers, data = traits)