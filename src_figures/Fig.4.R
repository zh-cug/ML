rm(list = ls())
library(readxl)
library(ggnewscale)
library(plyr)

setwd("./Figdata/")
load("td_rm.Rdata")
load("td.Rdata")

fig.4<-subset(td, model%in%c("CMAQ", "XGB"))
names(fig.4)[4]<-"method"
fig.4[which(fig.4$method=="XGB"), "method"]<-"G"
fig.4<-rbind(fig.4, td_rm)
fig.4<-dcast(fig.4, city+var~method, value.var = "slope")
fig.4<-melt(fig.4, measure.vars = 4:7, variable.name = "method", value.name = "trend")
fig.4$delta<-fig.4$trend-fig.4$CMAQ


bias<-ddply(fig.4, c("var", "method"), function(data){
  fitting<-lm(trend~CMAQ, data)
  return(100*(coef(fitting)[2]-1))
})
names(bias)[3]<-"bias"
fig.4<-merge(fig.4, bias, by=c("var", "method"))

a<-ggplot(subset(fig.4, var=="EMI"), aes(method, delta, fill=bias))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.3)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  scale_fill_gradient(low="blue", high="white")+
  geom_signif(
    comparisons = list(c("G", "M"), c("G", "V30"), 
                       c("G", "V5"), c("M", "V30"),
                       c("V30", "V5")),
    map_signif_level = TRUE, textsize = 2,
    step_increase = 0.1)+
  theme_bw()+
  theme(legend.key.height = unit(0.6, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.position = "right",
        panel.background = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size = 9),
        legend.text = element_text(size=7),
        legend.margin = margin(l=-5, 0, 0, 0))+
  xlab("Resampling strategy")+
  ylab(expression(Delta*" PM"[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"))+
  guides(fill=guide_colorbar(order=1,
                             title = expression("Bias in PM"[2.5]^EMI*" (%)"),
                             title.position = "right",
                             title.theme = element_text(angle = 90, 
                                                        size = 7, 
                                                        hjust=0.5)))

b<-ggplot(subset(fig.4, var=="MET"), aes(method, delta, fill=bias))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.3)+
  stat_summary(fun.y="mean", geom="point", shape=23, size=2, fill="white")+
  scale_fill_gradient(low="blue", high="white")+
  geom_signif(
    comparisons = list(c("G", "M"), c("G", "V30"), 
                       c("G", "V5"), c("M", "V30"),
                       c("V30", "V5")),
    map_signif_level = TRUE, textsize = 2,
    step_increase = 0.1
  )+
  theme_bw()+
  theme(legend.key.height = unit(0.6, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.position = "right",
        panel.background = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size = 9),
        legend.text = element_text(size=7),
        legend.margin = margin(l=-5, 0, 0, 0))+
  xlab("Resampling strategy")+
  ylab(expression(Delta*" PM"[2.5]^MET*" ("*mu*g*" m"^-3*" yr"^-1*")"))+
  guides(fill=guide_colorbar(order=1,
                             title = expression("Bias in PM"[2.5]^MET*" (%)"),
                             title.position = "right",
                             title.theme = element_text(angle = 90, 
                                                        size = 7, 
                                                        hjust=0.5)))

cowplot::plot_grid(a, b, ncol=1, labels = letters[1:2], label_size = 9)

export::graph2pdf(file="Fig.4.pdf", width=9/2.54, height=11/2.54)
