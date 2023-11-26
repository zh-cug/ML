library(lubridate)
library(openair)
library(plyr)
source("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/src/uf.R")


input<-import.data(dir_ap = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/PM2.5/data/city_d/",
                   dir_met = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/met/",
                   city = "Foshan",
                   pollutant = "PM2.5")

mlr_window<-function(mydata=NA, 
                     window=5){
  mydata<-timeAverage(mydata, avg.time = "10 day")
  mydata[, 12:21]<-apply(mydata[, 2:11], 2, function(x) x-mean(RcppRoll::roll_mean(x, window, align = "center", fill=NA, na.rm = T), na.rm=T))
  mydata<-na.omit(mydata)
  ts<-mydata[, c("date", "V1")]
  names(ts)[2]<-"value"
  ts$window<-paste0(window, "0-d moving window")
  ts
}

figdata<-rbind(mlr_window(mydata = input, window = 5),
               mlr_window(mydata = input, window = 9))

ggplot(figdata, aes(date, value, colour=window))+
  geom_path()+
  scale_colour_manual(values = c("black", "red"))+
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        axis.title.x = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=7),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.85))+
  ylab(expression(PM[2.5]*" (anomalies "*mu*g*" m"^-3*")"))

export::graph2jpg(file="./Figs/Fig.S16_.jpg", width=19/2.54, height=6/2.54)















