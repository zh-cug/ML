rm(list = ls())
library(ggpmisc)

setwd("./Figdata/")
load("td.Rdata")
city<-read.csv("./Figdata/Fig.s1.csv")
figdata<-dcast(subset(td, model!="rf_nt"), city+var~model, value.var = "slope")
figdata<-melt(figdata, measure.vars = 4:8, variable.name = "model", value.name = "y")
figdata<-merge(figdata, city[, c("city", "region")], by="city")

vars<-c("LT", "EMI", "MET")
ylabs<-c(expression(PM[2.5]^OBS*" by other models ("*mu*g*" m"^-3*" yr"^-1*")"),
         expression(PM[2.5]^EMI*" by other models ("*mu*g*" m"^-3*" yr"^-1*")"),
         expression(PM[2.5]^MET*" by other models ("*mu*g*" m"^-3*" yr"^-1*")"))

xlabs<-c(expression(PM[2.5]^OBS*" by CMAQ ("*mu*g*" m"^-3*" yr"^-1*")"),
         expression(PM[2.5]^EMI*" by CMAQ ("*mu*g*" m"^-3*" yr"^-1*")"),
         expression(PM[2.5]^MET*" by CMAQ ("*mu*g*" m"^-3*" yr"^-1*")"))


fig.s9<-list()
for (i in 1:3){
  fig.s9[[i]]<-ggplot(subset(figdata, var==vars[i]), aes(CMAQ, y, colour=model, fill=model))+
    geom_point(shape=1)+
    scale_colour_manual(values = c("#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"))+
    scale_fill_manual(values = c("#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"))+
    theme_bw()+
    stat_smooth(method = "lm", show.legend = F)+
    stat_poly_eq(mapping = use_label(c("eq", "rr")), 
                 formula = y ~ x, coef.digits = 3, small.r = T, size=2)+
    theme(axis.title = element_text(size=9),
          axis.text = element_text(size=8),
          legend.title = element_blank(),
          legend.position = c(0.99, 0.01),
          legend.justification = c(1, 0),
          legend.text = element_text(size=7),
          legend.background = element_blank(),
          legend.key.size = unit(8, "pt"))+
    xlab(xlabs[i])+
    ylab(ylabs[i])
}

cowplot::plot_grid(fig.s9[[1]], 
                   fig.s9[[2]]+theme(legend.position = "none"), 
                   fig.s9[[3]]+theme(legend.position = "none"), ncol=1, align = "vh")

export::graph2jpg(file="Fig.S9.jpg", width=9/2.54, height=21/2.54)
