rm(list = ls())
library(ggplot2)
library(maptools)
library(ggpp)
library(ggsignif)

setwd("./Figdata/")
load("td.Rdata")

site<-read.csv('Fig.s1.csv')
china<-readShapePoly("/Users/zhenghuang/Desktop/GIS/bou2_4p.shp")
china<-fortify(china)
scs<-readShapeLines("/Users/zhenghuang/Desktop/GIS/九段线.shp")
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

fig.data<-subset(td, model!="rf_nt")
fig.data<-ddply(fig.data, c("city", "var"),  summarise, mean=mean(slope), sd=sd(slope))
fig.data<-merge(fig.data, site[, c("city", "lon", "lat", "region")])

a<-ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=subset(fig.data, var=="LT"), 
             aes(lon, lat, fill=mean, size=sd), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient2(low="blue",  mid= "white", high="red", midpoint = 0)+
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
                             title = expression(PM[2.5]^OBS)),
         size="none")+
  xlab("Lon")+
  ylab("Lat")

b<-ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=subset(fig.data, var=="EMI"), 
             aes(lon, lat, fill=mean, size=sd), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient2(low="blue",  mid= "white", high="red", midpoint = 0)+
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
                             title = expression(PM[2.5]^EMI)),
         size="none")+
  xlab("Lon")+
  ylab("Lat")

c<-ggplot()+
  geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
  geom_point(data=subset(fig.data, var=="MET"), 
             aes(lon, lat, fill=mean, size=sd), shape=21, colour="gray", stroke=0.3)+
  scale_fill_gradient2(low="blue",  mid= "white", high="red", midpoint = 0)+
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
                             title = expression(PM[2.5]^MET)),
         size="none")+
  xlab("Lon")+
  ylab("Lat")

d<-ggplot(subset(td, model!="rf_nt" & var=="LT"),
          aes(model, slope, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  geom_signif(
    comparisons = list(c("CMAQ", "MLR"), c("CMAQ", "RF"), c("CMAQ", "XGB")),
    map_signif_level = TRUE, textsize = 2,
    step_increase = 0.1
  )+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size=8))+
  xlab("Model")+
  ylab(expression(PM[2.5]^OBS*" ("*mu*g*" m"^-3*" yr"^-1*")"))

e<-ggplot(subset(td, model!="rf_nt" & var=="EMI"),
          aes(model, slope, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  geom_signif(
    comparisons = list(c("CMAQ", "KZ"), c("CMAQ", "MLR"), c("CMAQ", "RF"), c("CMAQ", "XGB")),
    map_signif_level = TRUE, textsize = 2,
    step_increase = 0.1
  )+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size=8))+
  xlab("Model")+
  ylab(expression(PM[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"))

f<-ggplot(subset(td, model!="rf_nt" & var=="MET"),
          aes(model, slope, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  geom_signif(
    comparisons = list(c("CMAQ", "RF"), c("CMAQ", "XGB"), 
                       c("GC", "RF"), c("GC", "XGB"),
                       c("RF", "XGB")),
    map_signif_level = TRUE, textsize = 2,
    step_increase = 0.1
  )+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size=8))+
  xlab("Model")+
  ylab(expression(PM[2.5]^MET*" ("*mu*g*" m"^-3*" yr"^-1*")"))

cowplot::plot_grid(a, b, c, d, e, f, ncol=3, labels = letters[1:6], label_size = 9, align = "vh")

export::graph2pdf(file="Fig.2.pdf", width=19/2.54, height=12/2.54)


