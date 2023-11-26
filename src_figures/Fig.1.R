rm(list=ls())
library(maptools)
library(patchwork)
library(ggpp)


setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/Figdata/")
load("mp.Rdata")
site<-read.csv('Fig.s1.csv')
china<-readShapePoly("/Users/zhenghuang/Desktop/GIS/bou2_4p.shp")
china<-fortify(china)
scs<-readShapeLines("/Users/zhenghuang/Desktop/GIS/九段线.shp")
scs<-fortify(scs)



fig.data<-rbind(subset(mp, model%in%c("CMAQ", "GC") & res=="m"),
                subset(mp, model%in%c("MLR", "KZ", "RF", "XGB") & res!="m"))

fig.data<-merge(fig.data, site[, c("city", "lon", "lat")])




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


                          
a<-ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=ddply(fig.data, c("lon", "lat"), summarise, mean=mean(r), sd=sd(r)),
             aes(lon, lat, fill=mean, size=sd), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient(low="orange", high="red")+
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
  theme_bw()+
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.45, 'cm'),
        legend.direction = "horizontal",
        legend.margin = margin(l = 0, r = 0, t = -5, b = 0),
        legend.background = element_blank(),
        legend.text = element_text(size=6),
        axis.title = element_text(size=9),
        axis.text = element_text(size=7))+
  guides(fill=guide_colorbar(title.position = "right", 
                             title.theme = element_text(angle = 0, size=7),
                             title.vjust = 1, 
                             order=0,
                             title = "r"),
         size="none")+
  xlab("Lon")+
  ylab("Lat")



b<-ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=ddply(fig.data, c("lon", "lat"), summarise, mean=100*mean(NMB), sd=100*sd(NMB)),
             aes(lon, lat, fill=mean, size=sd), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient2(low="blue", mid ="white", high="red", midpoint = 0)+
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
  theme_bw()+
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.45, 'cm'),
        legend.direction = "horizontal",
        legend.margin = margin(l = 0, r = 0, t = -5, b = 0),
        legend.background = element_blank(),
        legend.text = element_text(size=6),
        axis.title = element_text(size=9),
        axis.text = element_text(size=7))+
  guides(fill=guide_colorbar(title.position = "right", 
                             title.theme = element_text(angle = 0, size=7),
                             title.vjust = 1, 
                             order=0,
                             title = "NMB(%)"),
         size="none")+
  xlab("Lon")+
   ylab("Lat")
  


c<-ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=ddply(fig.data, c("lon", "lat"), summarise, mean=mean(IOA), sd=sd(IOA)),
             aes(lon, lat, fill=mean, size=sd), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient(low="orange", high="red")+
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
  theme_bw()+
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.45, 'cm'),
        legend.direction = "horizontal",
        legend.margin = margin(l = 0, r = 0, t = -5, b = 0),
        legend.background = element_blank(),
        legend.text = element_text(size=6),
        axis.title = element_text(size=9),
        axis.text = element_text(size=7))+
  guides(fill=guide_colorbar(title.position = "right", 
                             title.theme = element_text(angle = 0, size=7),
                             title.vjust = 1, 
                             order=0,
                             title = "IOA"),
         size="none")+
  xlab("Lon")+
  ylab("Lat")




d<-ggplot(fig.data, aes(model, r, fill=model))+
  geom_hline(yintercept = 0.7, size=0.5, colour="black", lwd=0.5, lty=2)+
  geom_hline(yintercept = 0.4, colour="gray", lwd=0.5, lty=2)+
  stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  annotate("text", x=6, y=0.75, label="goal", size=2.5)+
  annotate("text", x=6, y=0.35, label="criteria", size=2.5, colour="gray")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=7),
        legend.title = element_blank())+
  xlab("Model")



e<-ggplot(fig.data, aes(model, 100*NMB, fill=model))+
  geom_hline(yintercept = 10, size=0.5, colour="black", lwd=0.5, lty=2)+
  geom_hline(yintercept = 30, colour="gray", lwd=0.5, lty=2)+
  geom_hline(yintercept = -10, size=0.5, colour="black", lwd=0.5, lty=2)+
  geom_hline(yintercept = -30, colour="gray", lwd=0.5, lty=2)+
  stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  #scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  #scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=7))+
  xlab("Model")+
  ylab("NMB (%)")



f<-ggplot(fig.data, aes(model, IOA, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  #scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  #scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=7))+
  ylab("IOA")+
  xlab("Model")


cowplot::plot_grid(a, b, c, d, e, f, ncol=3, align = "vh",
                   labels = letters[1:6], label_size = 9)


export::graph2pdf(file="/Users/zhenghuang/Desktop/Processing/ML-CMAQ/Figs/Fig.1.pdf", width=19/2.54, height=12/2.54)























