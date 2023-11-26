rm(list = ls())
library(ggplot2)
library(ggsignif)
setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
load("./Figdata/td.Rdata")

vars<-c("LT", "EMI", "MET")
ylabs<-c(expression(PM[2.5]^OBS*" ("*mu*g*" m"^-3*" yr"^-1*")"),
         expression(PM[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"),
         expression(PM[2.5]^MET*" ("*mu*g*" m"^-3*" yr"^-1*")"))

fig.s<-list()
for (i in 1:3){
  fig.s[[i]]<-ggplot(subset(td, model!="rf_nt" & var==vars[i]),
            aes(model, slope, fill=model))+
    stat_boxplot(geom = "errorbar",width=0.5)+
    geom_boxplot(width=0.5)+
    stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
    geom_signif(
      comparisons = list(c("CMAQ", "GC"), c("CMAQ", "KZ"), c("CMAQ", "MLR"), c("CMAQ", "RF"), c("CMAQ", "XGB"),
                         c("GC", "KZ"), c("GC", "MLR"), c("GC", "RF"), c("GC", "XGB"),
                         c("KZ", "MLR"), c("KZ", "RF"), c("KZ", "XGB"),
                         c("MLR", "RF"), c("MLR", "XGB"),
                         c("RF", "XGB")),
      map_signif_level = TRUE, textsize = 2,
      step_increase = 0.1
    )+
    theme_bw()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9),
          axis.text = element_text(size=8))+
    xlab("Model")+
    ylab(ylabs[i])
}
cowplot::plot_grid(fig.s[[1]], fig.s[[2]], fig.s[[3]], ncol=3)

export::graph2jpg(file="./Figs/Fig.S7_.jpg", width=19/2.54, height=7/2.54)
