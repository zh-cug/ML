rm(list = ls())
setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
load("./Figdata/td.Rdata")
fig.a<-dcast(subset(td, model%in%c("CMAQ", "RF", "rf_nt")), city+var~model, value.var = "slope")
fig.a<-melt(fig.a, measure.vars = 4:5, variable.name = "model", value.name = "trend")


a<-ggplot(subset(fig.a, var=="MET"), aes(CMAQ, trend, colour=model, fill=model))+
  geom_point( shape=1)+
  stat_smooth(method = "lm", show.legend = F)+
  scale_color_manual(values = c("#619CFF", "#A58AFF"),
                     labels=c("tu", "ut"),
                     breaks = c("RF", "rf_nt"))+
  scale_fill_manual(values = c("#619CFF", "#A58AFF"))+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2.5)+
  theme_bw()+
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, 'pt'),
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  guides(fill="none")+
  ylab(expression(PM[2.5]^MET* " by RF ("*mu*g* " m"^-3*" yr"^-1*")"))+
  xlab(expression(PM[2.5]^MET* " by CMAQ ("*mu*g* " m"^-3*" yr"^-1*")"))



fig.b<-dcast(subset(td, model%in%c("GC", "RF", "rf_nt")), city+var~model, value.var = "slope")
fig.b<-melt(fig.b, measure.vars = 4:5, variable.name = "model", value.name = "trend")


b<-ggplot(subset(fig.b, var=="EMI"), aes(GC, trend, colour=model, fill=model))+
  geom_point( shape=1)+
  stat_smooth(method = "lm",show.legend = F)+
  scale_fill_manual(values = c("#619CFF", "#A58AFF"))+
  scale_colour_manual(values = c("#619CFF", "#A58AFF"), 
                      labels=c("tu", "ut"),
                      breaks = c("RF", "rf_nt"))+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2.5)+
  theme_bw()+
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, 'pt'),
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  guides(fill="none")+
  ylab(expression(PM[2.5]^EMI* " by RF ("*mu*g* " m"^-3*" yr"^-1*")"))+
  xlab(expression(PM[2.5]^EMI* " by GC ("*mu*g* " m"^-3*" yr"^-1*")"))


c<-ggplot(subset(fig.b, var=="MET"), aes(GC, trend, colour=model, fill=model))+
  geom_point( shape=1)+
  stat_smooth(method = "lm", show.legend = F)+
  scale_fill_manual(values = c("#619CFF", "#A58AFF"))+
  scale_colour_manual(values = c("#619CFF", "#A58AFF"), 
                      labels=c("tu", "ut"),
                      breaks = c("RF", "rf_nt"))+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2.5)+
  theme_bw()+
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, 'pt'),
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  ylab(expression(PM[2.5]^MET* " by RF ("*mu*g* " m"^-3*" yr"^-1*")"))+
  xlab(expression(PM[2.5]^MET* " by GC ("*mu*g* " m"^-3*" yr"^-1*")"))


cowplot::plot_grid(a, b, c, ncol=1, align = "vh", labels = letters[1:3], label_size = 9)
export::graph2jpg(file="./Figs/Fig.S13.jpg", width=9/2.54, height=21/2.54)
