rm(list = ls())
library(ggplot2)

setwd("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/")
load("./Figdata/td.Rdata")
load("./Figdata/ts.Rdata")


models<-c("CMAQ", "GC", "KZ", "MLR", "RF", "XGB")
vars<-c("LT", "EMI", "MET")
pd<-list()
for (i in 1:6){
  for (j in 1:3){
    data<-subset(ts, model==models[i])[, c("date", "city",  vars[j])]
    data<-dcast(data, date~city, value.var = vars[j])
    data[, 2:75]<-apply(data[, 2:75], 2, function(x) scale(x))
    data$mean<-apply(data[, 2:75], 1, mean, na.rm=T)
    data$low<-apply(data[, 2:75], 1, function(x) as.numeric(t.test(x)$conf.int)[1])
    data$up<-apply(data[, 2:75], 1, function(x) as.numeric(t.test(x)$conf.int)[2])
    data<-data[, c("date", "mean", "low", "up")]
    data$model<-models[i]
    data$var<-vars[j]
    pd[[(i-1)*6+j]]<-data
    print(j)
  }
  print(models[i])
}
pd<-do.call(rbind, pd)  


a<-ggplot(subset(pd, var=="LT"), aes(date, mean, colour=model))+geom_line()+
  geom_ribbon(aes(ymax=up, ymin=low, fill=model, colour=model), alpha=0.4, colour=NA)+
 # scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  #scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  theme_bw()+
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  xlab("Year")+
  ylab(expression("Scaled PM" [2.5]^OBS))


b<-ggplot(subset(pd, var=="EMI"), aes(date, mean, colour=model))+geom_line()+
  geom_ribbon(aes(ymax=up, ymin=low, fill=model, colour=model), alpha=0.4, colour=NA)+
  #scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  #scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  xlab("Year")+
  ylab(expression("Scaled PM"[2.5]^EMI))

c<-ggplot(subset(pd, var=="MET"), aes(date, mean, colour=model))+geom_line()+
  geom_ribbon(aes(ymax=up, ymin=low, fill=model, colour=model), alpha=0.4, colour=NA)+
  #scale_fill_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  #scale_colour_manual(values = c("#f03b20", "#d95f0e", "#2c7fb8", "#31a354", "#c51b8a", "#dd1c77"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=9),
        axis.text = element_text(size=8))+
  xlab("Year")+
  ylab(expression("Scaled PM"[2.5]^MET))
cowplot::plot_grid(a, b, c, ncol=1)

export::graph2jpg(file="./Figs/Fig.S5_.jpg", width=9/2.54, height=18/2.54)
