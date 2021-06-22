
setwd("C:/Users/saret/Desktop/size")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)


############# SVPD ECHIUM
svpd <- read.table("pollen.txt",header=T) %>%
  dplyr::filter(Measurement=="SVPD") %>%
  mutate(Pollen_presence=if_else(Grains>0,1,0))

ggplot(data = svpd, aes(x=Bees, y=Grains,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)


svpd_echium <- svpd %>%
  dplyr::filter(Species=="Echium")

ggplot(data = svpd_echium, aes(x=Grains)) + 
  geom_histogram()+
  theme_classic()+
  facet_grid(Flowers ~ Bees)

res.aov <- aov(Grains ~ Flowers, data = svpd_echium)
summary(res.aov)
wilcox.test(Grains ~ Flowers, data = svpd_echium)
res.aov <- aov(Grains ~ Bees, data = svpd_echium)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = svpd_echium)

svpd_echium_small <- svpd %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Grains ~ Bees, data = svpd_echium_small)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = svpd_echium_small)
wilcox.test(Pollen_presence ~ Bees, data = svpd_echium_small)

svpd_echium_large <- svpd %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Grains ~ Bees, data = svpd_echium_large)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = svpd_echium_large)
wilcox.test(Pollen_presence ~ Bees, data = svpd_echium_large)



############# SVPD BORAGO
svpd <- read.table("pollen.txt",header=T) %>%
  dplyr::filter(Measurement=="SVPD") %>%
  dplyr::filter(Species=="Borago")%>%
  mutate(Pollen_presence=if_else(Grains>0,1,0))

ggplot(data = svpd, aes(x=Bees, y=Grains,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)


svpd_borago <- svpd %>%
  dplyr::filter(Species=="Borago")

ggplot(data = svpd_borago, aes(x=Grains)) + 
  geom_histogram()+
  theme_classic()+
  facet_grid(Flowers ~ Bees)

res.aov <- aov(Grains ~ Bees, data = svpd_borago)
summary(res.aov)

svpd_borago_small <- svpd %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Small") 


res.aov <- aov(Grains ~ Bees, data = svpd_borago_small)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = svpd_borago_small)
# wilcox.test(Pollen_presence ~ Bees, data = svpd_borago_small)

svpd_borago_large <- svpd %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Grains ~ Bees, data = svpd_borago_large)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = svpd_borago_large)



############# STIGMAS ECHIUM
stigma <- read.table("pollen.txt",header=T) %>%
  dplyr::filter(Measurement=="Stigma") %>%
  dplyr::filter(Species=="Echium") %>%
  mutate(Pollen_presence=if_else(Grains>0,1,0))

ggplot(data = stigma, aes(x=Bees, y=Pollen_presence,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)


stigma_echium <- stigma %>%
  dplyr::filter(Species=="Echium")

ggplot(data = stigma_echium, aes(x=Grains)) + 
  geom_histogram()+
  theme_classic()+
  facet_grid(Flowers ~ Bees)

res.aov <- aov(Grains ~ Flowers, data = stigma_echium)
summary(res.aov)
res.aov <- aov(Grains ~ Bees, data = stigma_echium)
summary(res.aov)

stigma_echium_small <- stigma %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Grains ~ Bees, data = stigma_echium_small)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = stigma_echium_small)
wilcox.test(Pollen_presence ~ Bees, data = stigma_echium_small)

stigma_echium_large <- stigma %>%
  dplyr::filter(Species=="Echium")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Grains ~ Bees, data = stigma_echium_large)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = stigma_echium_large)
wilcox.test(Pollen_presence ~ Bees, data = stigma_echium_large)


############# STIGMAS BORAGO
stigma <- read.table("pollen.txt",header=T) %>%
  dplyr::filter(Measurement=="Stigma") %>%
  dplyr::filter(Species=="Borago") %>%
  mutate(Pollen_presence=if_else(Grains>0,1,0))

ggplot(data = stigma, aes(x=Bees, y=Grains,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)


stigma_borago <- stigma %>%
  dplyr::filter(Species=="Borago")

ggplot(data = stigma_borago, aes(x=Grains)) + 
  geom_histogram()+
  theme_classic()+
  facet_grid(Flowers ~ Bees)

res.aov <- aov(Grains ~ Flowers, data = stigma_borago)
summary(res.aov)
res.aov <- aov(Grains ~ Bees, data = stigma_borago)
summary(res.aov)

stigma_borago_small <- stigma %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Small") 

res.aov <- aov(Grains ~ Bees, data = stigma_borago_small)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = stigma_borago_small)

stigma_borago_large <- stigma %>%
  dplyr::filter(Species=="Borago")%>%
  dplyr::filter(Flowers == "Large") 

res.aov <- aov(Grains ~ Bees, data = stigma_borago_large)
summary(res.aov)
wilcox.test(Grains ~ Bees, data = stigma_borago_large)

ggplot(data = stigma, aes(x=Bees, y=Grains,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)
