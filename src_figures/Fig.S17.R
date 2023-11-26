rm(list = ls())
library(ggplot2)
library(ggpmisc)

setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
load("./Figdata/sens_t.Rdata")

ggplot(subset(sens_t, var=="EMI"), aes(times, slope, colour=city, fill=city))+
  geom_point(shape=1)+
  stat_smooth(method = "lm", show.legend = F)+
  stat_poly_eq(mapping = use_label(c("eq", "rr", "p.value")), 
               formula = y ~ x, coef.digits = 3, small.r = T, p.digits = 3, size=2, label.y.npc = 0.95)+
  facet_wrap(city~., ncol=3, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        strip.background = element_blank(),
        strip.text = element_text(size=8))+
  xlab("Number of prediction in weather normalization")+
  ylab(expression(PM[2.5]^EMI*" ("*mu*g* " m"^-3* " yr"^-1*")"))

export::graph2jpg(file="./Figs/Fig.S17_.jpg", width=19/2.54, height=12/2.54)
