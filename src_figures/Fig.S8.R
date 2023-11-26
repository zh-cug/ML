rm(list = ls())
library(openair)
library(ggpmisc)
load("./Figdata/td.Rdata")

setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
site<-read.csv('./Figdata/Fig.s1.csv')

fig.data<-dcast(subset(td, model!="rf_nt"), city+model~var, value.var = "slope")

a<-ggplot(fig.data, aes(LT, EMI, colour=model, fill=model))+
  geom_point(shape=1)+
  # scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  # scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  stat_smooth(method = "lm", show.legend = F)+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 2, small.r = T, size=2.5)+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.text = element_text(size=6),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.key.size = unit(10, "pt"),
        legend.background = element_blank())+
  xlab(expression(PM[2.5]^OBS*" ("*mu*g*" m"^-3*" yr"^-1*")"))+
  ylab(expression(PM[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"))

b<-ggplot(fig.data, aes(LT, MET, colour=model, fill=model))+
  geom_point(shape=1)+
  # scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  # scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  stat_smooth(method = "lm", show.legend = F)+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 2, small.r = T, size=2)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  xlab(expression(PM[2.5]^OBS*" ("*mu*g*" m"^-3*" yr"^-1*")"))+
  ylab(expression(PM[2.5]^MET*" ("*mu*g*" m"^-3*" yr"^-1*")"))+
  ylim(-5, 2)

cowplot::plot_grid(a, b, ncol=1)

export::graph2jpg(file="Fig.S8.jpg", width=9/2.54, height=15/2.54)
