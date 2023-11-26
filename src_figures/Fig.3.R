rm(list = ls())
library(ggplot2)
library(ggsignif)
library(ggpmisc)
library(reshape2)

setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
load("./Figdata/mp.Rdata")
load("./Figdata/td.Rdata")




fig.3a<-subset(mp, model%in%c("RF", "rf_nt") & res=="h")
#fig.4a$NMB<-fig.4a$NMB*100
fig.3a<-melt(fig.3a, measure.vars = c("r", "NMB", "IOA"), variable.name = "var", value.name = "value")

nmb<-ggplot(subset(mp, model%in%c("RF", "rf_nt") & res=="h"), aes(model, NMB, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5, position=position_dodge(0.8))+
  geom_boxplot(width=0.5, position=position_dodge(0.8))+
  stat_summary(mapping = aes(group=model), position=position_dodge(0.8), fun.y="mean",
               geom="point", shape=23, size=2, fill="white")+
  scale_fill_manual(values = c("#619CFF", "gray"), 
                    labels=c("tu", "ut"),
                    breaks = c("RF", "rf_nt"))+
  stat_signif(comparisons = list(c("RF", "rf_nt")),
              map_signif_level = T,
              textsize = 2)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size=8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())+
  ylim(-0.006, 0.019)



a<-ggplot(fig.3a, aes(var, value, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5, position=position_dodge(0.8))+
  geom_boxplot(width=0.5, position=position_dodge(0.8))+
  stat_summary(mapping = aes(group=model), position=position_dodge(0.8), fun.y="mean",
               geom="point", shape=23, size=2, fill="white")+
  scale_fill_manual(values = c("#619CFF", "gray"), 
                    labels=c("tu", "ut"),
                    breaks = c("RF", "rf_nt"))+
  stat_signif(annotations = c("***", "NS.", "***"),
              xmin = c(0.8, 1.8, 2.8), xmax=c(1.2, 2.2, 3.2),
              y_position = c(1.02, 0.04, 0.95),
              textsize = 2.5)+
  annotate("plot_npc", npcx = 0.15, npcy = 0, label = nmb, vp.width=0.55, vp.height=0.5) +
  theme_bw()+
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, "pt"),
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  ylab("Value")+
  xlab("Statistics")+
  ylim(c(0.6, 1.03))




fig.3b<-subset(td, model%in%c("RF", "rf_nt"))
fig.3b$var<-factor(fig.3b$var, levels = c("LT", "EMI", "MET"))

b<-ggplot(fig.3b, aes(var, slope, fill=model))+
  stat_boxplot(geom = "errorbar",width=0.5, position=position_dodge(0.8))+
  geom_boxplot(width=0.5, position=position_dodge(0.8))+
  stat_summary(mapping = aes(group=model), position=position_dodge(0.8), fun.y="mean",
               geom="point", shape=23, size=2, fill="white")+
  scale_x_discrete(limits=c("LT", "EMI", "MET"),
                   labels=c(expression(PM[2.5]^OBS),
                            expression(PM[2.5]^EMI),
                            expression(PM[2.5]^MET)))+
  scale_fill_manual(values = c("#619CFF", "gray"), 
                    labels=c("tu", "ut"),
                    breaks = c("RF", "rf_nt"))+
  stat_signif(annotations = c("NS.", "NS.", "***"),
              xmin = c(0.8, 1.8, 2.8), xmax=c(1.2, 2.2, 3.2),
              y_position = c(0.6, 0.8, 1.6),
              textsize = 2.5)+
  theme_bw()+
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, "pt"),
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  ylab(expression(Trend* " ("*mu*g* " m"^-3*" yr"^-1*")"))+
  xlab("Type")


fig.3c<-dcast(subset(td, model%in%c("CMAQ", "RF", "rf_nt")), city+var~model, value.var = "slope")
fig.3c<-melt(fig.3c, measure.vars = 4:5, variable.name = "model", value.name = "trend")


c<-ggplot(subset(fig.3c, var=="EMI"), aes(CMAQ, trend, colour=model, fill=model))+
  geom_point( shape=1)+
  stat_smooth(method = "lm",show.legend = F)+
  scale_fill_manual(values = c("#619CFF", "gray"))+
  scale_color_manual(values = c("#619CFF", "gray"), 
                    labels=c("tu", "ut"),
                    breaks = c("RF", "rf_nt"))+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2.5,
               label.x = "left", label.y = "bottom")+
  theme_bw()+
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
  xlab(expression(PM[2.5]^EMI* " by CMAQ ("*mu*g* " m"^-3*" yr"^-1*")"))

cowplot::plot_grid(a, b, c, ncol=3, align = "vh", labels = letters[1:3], label_size = 9)

export::graph2pdf(file="./Manuscript/revision_2nd/Figures/Fig.3.pdf", width=19/2.54, height=6.5/2.54)



