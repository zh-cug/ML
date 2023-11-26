rm(list = ls())
library(ggplot2)
library(maptools)
library(ggpp)

setwd("./Figdata")
load("td.Rdata")
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

figdata<-merge(subset(td, model!="rf_nt"), site[, c("city", "lon", "lat")], by="city")

fig.s6<-list()
vars=c("LT", "EMI", "MET")
labs<-c(expression(PM[2.5]^OBS* " ("*mu*g*" m"^-3*" yr"^-1*")"),
        expression(PM[2.5]^EMI* " ("*mu*g*" m"^-3*" yr"^-1*")"),
        expression(PM[2.5]^MET* " ("*mu*g*" m"^-3*" yr"^-1*")"))

for (i in 1:3){
  fig.s6[[i]]<-ggplot()+
    geom_polygon(data=china, aes(long, lat, group=group), fill="white", colour="black", linewidth=0.3)+
    geom_point(data=subset(figdata, var==vars[i]), aes(lon, lat, fill=slope),
               shape=21, colour="gray", stroke=0.3)+
    scale_fill_gradient2(low="blue", mid ="white", high="red", midpoint = 0)+
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
    facet_wrap(model~., ncol=6)+
    theme_bw()+
    theme(legend.position = "right",
          legend.key.height = unit(0.6, 'cm'),
          legend.key.width = unit(0.2, 'cm'),
          legend.margin = margin(t=0, l=-5, r=-5, b=0),
          axis.text = element_text(size=8),
          axis.title = element_blank(),
          strip.background = element_blank())+
    guides(fill=guide_colorbar(title.position = "right", 
                               title.theme = element_text(angle = 90, size=7),
                               title.hjust = 0.5, 
                               title = labs[i]))
}

cowplot::plot_grid(fig.s6[[1]], 
                   fig.s6[[2]]+
                     theme(strip.text = element_blank()),
                   fig.s6[[3]]+
                     theme(strip.text = element_blank()), 
                   ncol=1, align = "vh")

export::graph2jpg(file="Fig.S6.jpg", width=29/2.54, height=17/2.54)
