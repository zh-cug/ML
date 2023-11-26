rm(list = ls())
library(maptools)
library(ggplot2)
library(ggpp)

setwd("./Figdata/")

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


fig.s14<-read.csv("./Figdata/Fig.s1.csv")
ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", size=0.3)+
  geom_point(data=subset(fig.s14, type=="ap"), aes(lon, lat, colour=region))+
  scale_color_discrete(limits=c("BTH", "PRD", "YRD", "Other"))+
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
  theme_bw()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        axis.text = element_text(size=9),
        legend.text = element_text(size =7),
        legend.background = element_blank())+
  xlab("Lon")+
  ylab("Lat")

export::graph2jpg(file="Fig.S14.jpg", width=9/2.54, height=9/2.54)
