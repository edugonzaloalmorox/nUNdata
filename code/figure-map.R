library(rgdal)
library(foreign)
library(ggplot2)
library(choroplethr)
library(XML)
library(maptools)
library(dplyr)
library(tidyverse)
library(stringr)
library(rio)
library(grid)
library(gridExtra)
library(viridis)
library(readxl)
library (janitor)
library(forcats)
library(cowplot)
library(ggthemes)

sectors<- readOGR("/Users/Personas/Downloads/USOS_2016",
                  "USOS_2016")

sectors@data$id = rownames(sectors@data)

sectors.points = fortify(sectors, region = "id")




sectors.df = inner_join(sectors.points, sectors@data, by = "id") 

lev = unique(levels(sectors.df$USP_TX_DEN))

zonas_verdes = c( "DOTACIONAL ZONAS VERDES",                                                                      
                  "DOTACIONAL ZONAS VERDES BARRIO BÁSICO",                                                        
                  "DOTACIONAL ZONAS VERDES BÁSICO",                                                               
                  "DOTACIONAL ZONAS VERDES PARQUE DISTRITO  BÁSICO" ,                                             
                 "DOTACIONAL ZONAS VERDES PARQUE METROPOLITANO  SINGULAR",                                       
                 "DOTACIONAL ZONAS VERDES PARQUE URBANO SINGULAR",                                               
                  "DOTACIONAL ZONAS VERDES SINGULAR" )




sectors.df = sectors.df %>% mutate(use_variable = ifelse(USP_TX_DEN %in% zonas_verdes, "green", "other")) #data frame that we use for drawing the map 


map_madrid = ggplot(sectors.df, aes(long, lat, group=group)) +
  geom_polygon(aes(fill = factor(use_variable)), colour = alpha("white", 1 /8), size = 0.1) + 
  labs(fill = "Zonas verdes vs otros usos") +
  labs(title = "Zonas verdes en Madrid") +
  scale_fill_manual(values = viridis(5)[2:3])+
  theme(axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_text(size = 4)
        ,legend.text = element_text(size = 6)
        ,legend.position = "right"
        ,legend.background = element_blank()
        ,panel.background = element_blank())


ggsave("./output/figures/madrid.png", map_madrid, scale = 0.5)
