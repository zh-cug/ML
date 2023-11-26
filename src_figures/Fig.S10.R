rm(list = ls())
library(mlr3)
library(mlr3learners)

setwd("./Figdata/")
load("td.Rdata")
load("td_train.Rdata")
load("td_rf.Rdata")
load("mp.Rdata")

source("./src/dw.kz.R")
source("./src/dw.mlr.R")
source("./src/kz-rf.R")
source("./src/uf.R")

#city<-read.csv("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/city.csv")
#mp<-list()
#td_rf<-list()
#for (i in 1:74){
#  site<-city$city[i]
#  input<-import.data(dir_ap = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/PM2.5/data/city_d/",
#                     dir_met = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/met/",
#                     dir_clus = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/",
#                     res = "day",
#                     pollutant = "PM2.5",
#                     city = site)
# kz<-dw.kz(mydata = input,
            pollutant = "PM2.5",
            city = site)
# mlr<-dw.mlr(mydata = input,
              pollutant = "PM2.5",
              city = site)
# rf<-kz.rf(mydata = input,
#            pollutant = "PM2.5",
#            city = site)
#  mp[[i]]<-rbind(kz$mp, mlr$mp, rf$mp)
#  td_rf[[i]]<-rf$td
#  print (site)
#}

#mp<-do.call(rbind, mp)
#td_rf<-do.call(rbind, td_rf)
#save(td_rf, file="/Users/zhenghuang/Desktop/Processing/ML-CMAQ/Figdata/td_rf.Rdata")

fig.data.a<-merge(subset(td, model=="CMAQ" & var=="EMI")[, c("city", "slope")],
                  rbind(subset(td, model%in%c("KZ", "MLR") & var=="EMI")[, c("city", "slope", "model")],
                        subset(td_rf, var=="EMI")[, c("city", "slope", "model")]),
                  by="city")
fig.data.a$bias<-fig.data.a$slope.y-fig.data.a$slope.x
fig.data.a<-merge(fig.data.a, 
                  mp[, c("city", "sse", "slope", "model")], by=c("city", "model"))

a<-ggplot(subset(fig.data.a, model!="KZ.RF"), aes(slope, bias, colour=model, fill=model))+
  geom_point(shape=1)+
  stat_smooth(method = "lm", show.legend = F)+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2)+
  scale_color_manual(values = c("#00BA38", "#00BFC4"), labels=c("KZ", "MLR"))+
  scale_fill_manual(values = c("#00BA38", "#00BFC4"), labels=c("KZ", "MLR"))+
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.text = element_text(size=7),
        legend.background = element_blank())+
  xlab("Slope")+
  ylab(expression(Delta*" PM"[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"))

b<-ggplot(subset(fig.data.a, model!="KZ.RF"), aes(sse, bias, colour=model, fill=model))+
  geom_point(shape=1)+
  stat_smooth(method = "lm", show.legend = F)+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2)+
  scale_color_manual(values = c("#00BA38", "#00BFC4"), labels=c("KZ", "MLR"))+
  scale_fill_manual(values = c("#00BA38", "#00BFC4"), labels=c("KZ", "MLR"))+
  scale_x_log10()+
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.text = element_text(size=7),
        legend.background = element_blank())+
  xlab("SSE")+
  ylab(expression(Delta*" PM"[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"))

c<-ggplot(merge(fig.data.a, ddply(fig.data.a, "model", summarise, mean=mean(slope)), by="model"),
          aes(model, bias, fill=mean))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.3)+
  stat_summary(fun="mean", geom="point", shape=23, size=2, fill="white")+
  scale_x_discrete(limits=c("MLR", "KZ", "KZ.RF"))+
  scale_fill_gradient(low="blue", high="red")+
  theme_bw()+
  theme( legend.position = c(0.99, 0.01),
         legend.justification = c(1,0),
         legend.direction = "horizontal",
         legend.key.height = unit(0.3, "cm"),
         legend.key.width = unit(0.6, "cm"),
         legend.title = element_text(size=8),
         legend.text = element_text(size=7),
         legend.background = element_blank(),
         axis.title = element_text(size=9),
         axis.text = element_text(size=8))+
  xlab("Model")+
  ylab(expression(Delta*" PM"[2.5]^EMI*" ("*mu*g*" m"^-3*" yr"^-1*")"))+
  guides(fill=guide_colorbar(title = "Mean slope",
                             title.position = "top",
                             title.hjust = 0.5))


load("res_td.Rdata")
names(res_td)[1]<-"model"

res_td$model<-as.character(res_td$model)
res_td[which(res_td$model=="EMI.nt"), "model"]<-"RF.nt"

fig.data.b<-melt(dcast(rbind(subset(td, var=="EMI" & model%in%c("CMAQ", "RF"))[, c("city", "slope", "model")],
                             subset(res_td, model=="RF.nt")[, c("city", "slope", "model")]),
                       city~model, value.var = "slope"),
                 measure.vars = 3:4, variable.name = "model", value.name = "y")

d<-ggplot(fig.data.b, aes(CMAQ, y, colour=model, fill=model))+geom_point(shape=1)+
  stat_smooth(method = "lm", show.legend = F)+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), label.x = "left", label.y = "bottom",
               formula = y ~ x, coef.digits = 3, small.r = T, size=2)+
  scale_color_manual(values = c("#619CFF", "#00BFC4"), labels=c("RF_wt", "RF_nt"))+
  scale_fill_manual(values = c("#619CFF", "#00BFC4"), labels=c("RF_wt", "RF_nt"))+
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.title = element_blank(),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.text = element_text(size=7),
        legend.background = element_blank())+
  xlab(expression(PM[2.5]^EMI*" by CMAQ ("*mu*g*" m"^-3*" yr"^-1*")"))+
  ylab(expression(PM[2.5]^EMI*" by RF ("*mu*g*" m"^-3*" yr"^-1*")"))

cowplot::plot_grid(a, b, c, d, ncol=2, labels = letters[1:4], label_size = 9) 

export::graph2jpg(file="Fig.S10.jpg", width=14/2.54, height=12/2.54)
