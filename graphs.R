
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

seedsetgraph <- ggplot(data = seedset, aes(x=Bees, y=Seed_set, fill=Bees)) + 
  geom_boxplot(aes(fill=Bees))+
  theme_classic()+
  coord_cartesian(ylim = c(1, 4))+
  theme(legend.position = "none")+
  facet_grid(. ~ Flowers)

ggarrange(fruitsetgraph,fecunditygraph,seedsetgraph, ncol = 1, nrow = 3,labels = c("(a)", "(b)", "(c)"))
###• export en 5 x 8 inches



ggplot(data = svpd, aes(x=Bees, y=Grains,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)+
  coord_cartesian(ylim = c(0, 120))



ggplot(data = stigma, aes(x=Bees, y=Grains,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)+
  coord_cartesian(ylim = c(0, 200))



ggplot(data = surveys, aes(x=Bees, y=Time_flower,fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)+
  coord_cartesian(ylim = c(0, 20))



ggplot(data = surveys_summarised, aes(x=Bees, y=Flowers_per_minute,fill=Bees)) + 
  geom_boxplot(aes(fill=Treatment))+
  theme_classic()+
  labs(y = "Flowers per minute")+
  theme(legend.position = "none")+
  facet_grid(Species ~ Flowers)
