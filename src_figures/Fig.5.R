library(ggplot2)
library(reshape2)
library(plyr)
library(ggpmisc)
library(lubridate)
setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
load("./Figdata/ts.Rdata")
load("./Figdata/ts_train.Rdata")
load("./Figdata/td.Rdata")
load("./Figdata/td_train.Rdata")



ts_train$model<-"RF_wt"
ts_train$date<-ymd(ts_train$date)

fig.a<-rbind(subset(ts, model%in%c("CMAQ", "RF"))[, c("city", "date", "EMI", "model")],
             ts_train[,c("city", "date", "EMI", "model")])

models<-c("CMAQ", "RF", "RF_wt")

fig.data.a<-list()
for (i in 1:3){
  data<-subset(fig.a, model==models[i])[, c("date", "city", "EMI")]
  data<-dcast(data, date~city, value.var = "EMI")
  data[, 2:75]<-apply(data[, 2:75], 2, function(x) scale(x))
  data$mean<-apply(data[, 2:75], 1, mean, na.rm=T)
  data$low<-apply(data[, 2:75], 1, function(x) as.numeric(t.test(x)$conf.int)[1])
  data$up<-apply(data[, 2:75], 1, function(x) as.numeric(t.test(x)$conf.int)[2])
  data<-data[, c("date", "mean", "low", "up")]
  data$model<-models[i]
  fig.data.a[[i]]<-data
  print(i)
}
fig.data.a<-do.call(rbind, fig.data.a)

a<-ggplot(fig.data.a, aes(date, mean, colour=model))+geom_line()+
  geom_ribbon(aes(ymax=up, ymin=low, fill=model, colour=model), 
              alpha=0.4, colour=NA, show.legend = F)+
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#c51b8a"))+
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#c51b8a"),
                      labels=c("CMAQ", "RF_nt", "RF_wt"))+
  theme_bw()+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  xlab("Year")+
  ylab(expression("Scaled PM"[2.5]^EMI))



td_train$model<-"RF_wt"
fig.data.b<-dcast(rbind(subset(td, model%in%c("CMAQ", "RF") & var=="EMI")[, c("city",  "model", "slope")],
                        subset(td_train, var=="EMI")[, c("city",  "model", "slope")]),
                  city~model, value.var = "slope")
fig.data.b<-melt(fig.data.b, measure.vars = 3:4, variable.name = "model", value.name = "RF")

b<-ggplot(fig.data.b, aes(CMAQ, RF, colour=model, fill=model))+
  geom_point(shape=1)+
  stat_smooth(method = "lm",show.legend = F)+
  # geom_abline(slope=1, intercept = 0, lty=2)+
  scale_fill_manual(values = c("#619CFF", "#c51b8a"))+
  scale_color_manual(values = c("#619CFF", "#c51b8a"), 
                     labels=c("RF_nt", "RF_wt"))+
  stat_poly_eq(mapping = use_label(c("eq", "rr")), 
               formula = y ~ x, coef.digits = 3, small.r = T, size=2.5)+
  theme_bw()+
  theme_bw()+
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  guides(fill="none")+
  ylab(expression(PM[2.5]^EMI* " by RF ("*mu*g* " m"^-3*" yr"^-1*")"))+
  xlab(expression(PM[2.5]^EMI* " by CMAQ ("*mu*g* " m"^-3*" yr"^-1*")"))

cowplot::plot_grid(a, b, ncol=2, labels = letters[1:2], label_size = 9, rel_widths = c(2, 1))

export::graph2pdf(file="./Manuscript/revision_2nd/Figures/Fig.5.pdf", width=19/2.54, height=6/2.54)
