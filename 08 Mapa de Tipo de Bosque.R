


library(ggplot2)
library(raster)
library(tidyverse)
library(elevatr)
library(sf)
library(sp)
library(ggspatial)
library( maptools)
library(ggnewscale)
library(tmap)
library(ggthemes)
library(hrbrthemes)

library(tidyverse)      # data manipulation and viz
library(ggtern)         # plot ternary diagrams
library(gridExtra)      # arrange subplots
library(lubridate)      # easy manipulations with dates
library(ggthemes)       # themes for ggplot2
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(eurostat)       # grab data
library(rgdal)          # deal with shapefiles
library(rgeos)
library(maptools)
library(pacman)         # deal with packages
library(tricolore) 
library(shinyjs)
library(colorspace)
hclwizard()
#-----------------------------------------------------------------------------------

Peru_p  <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Peru_  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru_p_xy <- cbind(Peru_, st_coordinates(st_centroid(Peru_$geometry)))
MDD     <- subset(Peru_p, NAME_1  == "Madre de Dios")
MDD_xy <- cbind(MDD , st_coordinates(st_centroid(MDD$geometry)))
Tipo_bosque <- st_read ("SHP/Tipo de bosque.geojson")
col_palette <-colorspace::sequential_hcl(n = 28, h = c(0, 90), c = c(80, NA, 30), l = c(30, 90), power = c(0.2, 2), register = "Custom-Palette")

Mapa= ggplot()+
  geom_sf(data = Peru_p, fill = "grey90",color = "grey90")+
  geom_sf(data = MDD, fill = "grey90",color = "white")+
  geom_sf(data = Tipo_bosque,aes(fill = CobVeg2013),  alpha = 0.7, linetype = 0.2 )  +
  scale_fill_manual(values = col_palette)+
  geom_sf_text(data = Peru_p_xy, aes(x= X, y=Y, label = NAME_1), size = 2.5, color="black", fontface = "bold",fontfamily = "serif")+
  geom_label(data =  MDD_xy , aes(x= X, y=Y, label = NAME_3), size = 2.5, color="black", fontface = "bold",fontfamily = "serif")+
  coord_sf(xlim = c(-73,-67.6), ylim = c(-13.5,-9),expand = FALSE)+
  theme_map(base_family = font_rc)+
  theme(panel.border = element_rect(color = "black",size = .5,fill = NA),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        legend.title =element_text(size=10, face = "bold"), #tamaño de titulo de leyenda
        legend.position = c(.75, .4),
        legend.text =element_text(size=9))+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL)+
  annotate(geom = "text", x=-68.7, y = -13, hjust = 0, vjust = 0,
           label = "Gorky Florez Castillo", fontface = 2,
           size = 4, family = font_rc, color = "grey20")+
  annotate(geom = "text", x = -68.7, y = -13.1, hjust = 0, vjust = 0,
           label = "Ing. Forestal y Medio Ambiente",
           size = 3 , family = font_rc, color = "grey40")+
  annotate(geom = "text", x = -68.5, y = -13.2, hjust = 0, vjust = 0,
           label = "&", fontface = 2,
           size = 5, family = font_rc, color = "#35978f")+
  annotate(geom = "text", x = -68.2, y = -13.3, hjust = 1, vjust = 0,
           label = "Codigo en Githab", fontface = 2,
           size = 3, family = font_rc, color = "grey20")+
  annotate(geom = "text", x = -67.8, y = -13.4, hjust = 1, vjust = 0,
           label = "https://github.com/GorkyFlorez/Mapa_tipo_bosque_MDD",
           size = 3, family = font_rc, color = "grey40")+
  # date
  annotate(geom = "text", x = -69.3, y = -13.4, hjust = 0, vjust = 0,
           label = "2022", fontface = 2,
           size = 5, family = font_rc, color = "#35978f")+
  # data
  annotate(geom = "text", x = -73, y = -13.4, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
           size = 3, family = font_rc, color = "grey50")+
  # title
  annotate(geom = "text", x = -73, y = -9.5, hjust = 0, vjust = 1, 
           label = "MAPA DE TIPO DE BOSQUE\n                         EN MADRE DE DIOS",
           size = 8, family="serif", color = "grey20")+
  guides(fill = guide_legend(nrow = 28, ncol=1))
  
# save
ggsave("Mapas/Tipo de Bosque.png", Mapa, width = 14, height = 11.76, 
       dpi = 900, type = "cairo-png")


