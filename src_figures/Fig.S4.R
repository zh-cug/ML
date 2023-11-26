rm(list = ls())
library(ggsignif)
library(ggplot2)

setwd("./Figdata/")
load("mp.Rdata")

a<-ggplot(subset(mp,  model%in%c("MLR", "KZ", "RF", "XGB")), aes(model, r, fill=res))+
  stat_boxplot(geom = "errorbar",width=0.5, position=position_dodge(0.8))+
  geom_boxplot(width=0.5, position=position_dodge(0.8))+
  stat_summary(mapping = aes(group=res), position=position_dodge(0.8), fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  stat_signif(annotations = c("*", "***", "***", "***"),
              xmin = c(0.8, 1.8, 2.8, 3.8), xmax=c(1.2, 2.2, 3.2, 4.2),
              y_position = c(0.97, 0.95, 1.02, 1.02))+
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.position = "top")+
  xlab("Model")+
  ylim(0.65, 1.05)
  
b<-ggplot(subset(mp,  model%in%c("MLR", "KZ", "RF", "XGB")), aes(model, IOA, fill=res))+
  stat_boxplot(geom = "errorbar",width=0.5, position=position_dodge(0.8))+
  geom_boxplot(width=0.5, position=position_dodge(0.8))+
  stat_summary(mapping = aes(group=res), position=position_dodge(0.8), fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  scale_fill_discrete(labels=c("h", "d", "m"))+
  stat_signif(annotations = c("*", "***", "***", "***"),
              xmin = c(0.8, 1.8, 2.8, 3.8), xmax=c(1.2, 2.2, 3.2, 4.2),
              y_position = c(0.88, 0.85, 1.02, 1.02))+
  theme_bw()+
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.position = "none")+
  xlab("Model")+
  ylim(0.55, 1.05)

legend<-cowplot::get_legend(a)
cowplot::plot_grid(cowplot::plot_grid(a+theme(legend.position = "none"), b, ncol=2),
          legend, rel_heights = c(10, 1), ncol=1)

export::graph2jpg(file="Fig.S4.jpg", width=14/2.54, height=7/2.54)
