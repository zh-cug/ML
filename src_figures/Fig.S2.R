rm(list=ls())
library(maptools)
library(patchwork)
library(ggpp)

setwd("./Fidata/")
load("mp.Rdata")
site<-read.csv('Fig.s1.csv')
china<-readShapePoly("./GIS/bou2_4p.shp")
china<-fortify(china)
scs<-readShapeLines("./GIS/九段线.shp")
scs<-fortify(scs)

p<-ggplot(scs, aes(long, lat, group=group))+
  geom_path(size=0.4)+
  geom_path(data=china, aes(long, lat, group=group),  colour="black", size=0.3)+
  scale_x_continuous(expand = c(0, 0), limits = c(105, 125))+
  scale_y_continuous(expand = c(0, 0), limits=c(4, 25))+
  theme(panel.background = element_rect(fill="white", colour = "black"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank())

fig.s2<-merge(subset(mp, model!="rf_nt"), site[, c("city", "lon", "lat")], by="city")

ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=fig.s2,
             aes(lon, lat, fill=100*NMB), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 0)+
  scale_size_area(max_size=3)+
  scale_x_continuous(breaks = seq(80, 120, 20), limits = c(73, 136),
                     labels = c(expression(80^o* "E"), 
                                expression(100^o* "E"), 
                                expression(120^o* "E")))+
  scale_y_continuous(breaks = seq(20, 50, 10), limits = c(18, 54),
                     labels = c(expression(20^o* "N"), 
                                expression(30^o* "N"), 
                                expression(40^o* "N"),
                                expression(50^o* "N")))+
  annotate("plot_npc", npcx = 0, npcy = 0, label = p, vp.width=0.2, vp.height=0.25) +
  facet_wrap(model~., ncol=3)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(l = 0, r = 0, t = -5, b = 0),
        legend.background = element_blank(),
        legend.text = element_text(size=7),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  guides(fill=guide_colorbar(title.position = "bottom", 
                             title.theme = element_text(angle = 0, size=8),
                             title.hjust = 0.5, 
                             order=0,
                             title = "NMB(%)"),
         size="none")+
  xlab("Lon")+
  ylab("Lat")

export::graph2jpg(file="Fig.S2.jpg", width=19/2.54, height=14/2.54)
