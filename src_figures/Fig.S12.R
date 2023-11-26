library(profvis)
library(openair)
library(reshape2)
library(dplyr)
library(mlr3)
library(mlr3learners)
library(plyr)
library(ggrepel)

#use the profvis to determine the resources and time needed to run MLR, RF, and XGB model.
# source("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/src/uf.R")
# source("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/src/dw.rf_new.R")
# source("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/src/dw.xgb_new.R")
# 
# 
# load("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/Figdata/ap.Rdata")
# input<-import.data(dir_ap = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/res/city_h/",
#                    dir_met = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/res/met/",
#                    dir_clus = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/res/clus/",
#                    pollutant = "PM2.5",
#                    res = "hour",
#                    city="Beijing")
# 
# 
# ap<-import.data(dir_ap = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/res/city_d/",
#                 dir_met = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/res/met/",
#                 dir_clus = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/res/clus/",
#                 pollutant = "PM2.5",
#                 res = "day",
#                 city = "Beijing")
# ap<-add_time_variables(ap)
# 
# 
# profvis({
#   mlr<-dw.mlr(mydata = ap, pollutant = "PM2.5", city="Beijing")
#   kz<-dw.kz(mydata = ap, pollutant = "PM2.5", city="Beijing")
#   rf<-dw.rf(mydata = ap,
#             pollutant = "PM2.5",
#             vars = c(colnames(ap)[3:12], "date_unix", "day_julian","weekday"),
#             met= colnames(ap)[3:11],
#             #n_sample = 500,
#             tuning = F)
#   xgb<-dw.xgb(mydata = ap,
#              pollutant = "PM2.5",
#              vars = c(colnames(ap)[3:12], "date_unix", "day_julian","weekday"),
#              met= colnames(ap)[3:11],
#             # n_sample = 300,
#              tuning = F)
# })


# hyper_pm_rf<-read.csv("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/hyper_pm2.5_rf.csv")
# hyper_pm_xgb<-read.csv("/Users/zhenghuang/Desktop/Processing/ML-CMAQ//hyper_pm2.5_xgb.csv")
# city<-read.csv("/Users/zhenghuang/Desktop/Processing/ML-CMAQ/city.csv")
# 
# 
# 
# 
# 
# met<-import.met(dir_met = "/Users/zhenghuang/Desktop/Processing/ML-CMAQ/met/",
#                 year=c(2013, 2017),
#                 city = "Beijing")
# 
# ap<-add_time_variables(ap)
# task<-as_task_regr(na.omit(ap[, -c(1,14,16,17)]), target=pollutant)
# # 
# # #define learn
# learner_xgb<-lrn("regr.xgboost")
# #learner_rf_nt<-lrn("regr.ranger")
# #learner_rf_tu<-lrn("regr.ranger")
# 
# #set hyper parameters
# learner_xgb$param_set$set_values(nrounds=hyper_pm_xgb$nrounds[i],
#                                  max_depth=hyper_pm_xgb$max_depth[i],
#                                  eta=hyper_pm_xgb$eta[i])
# # learner_rf_tu$param_set$set_values(num.trees=hyper_pm_rf$num.trees[i],
# #                                    mtry=hyper_pm_rf$mtry[i], 
# #                                    min.node.size=hyper_pm_rf$min.node.size[i])
# # 
# #learner_rf_nt$param_set$set_values(num.trees=300, mtry=3, min.node.size=5)
# 
# #learner$param_set$values
# splits = partition(task, ratio = 0.8)
# # 
# # 
# # #train the model
# learner_xgb$train(task, splits$train)
# learner_rf_nt$train(task, splits$train)
# learner_rf_tu$train(task, splits$train)
# 
# 
# 
# 
# met_g<-resample_met(ap_data = data.frame(task$data()),
#                     met_data = met,
#                     method = "G",
#                     base_year = 2017,
#                     variables = colnames(met)[2:10],
#                     n=5)
# 


resource<-data.frame(model=c("GC", "KZ", "MLR", "RF", "XGB"),
               time=c(10800, 0.6, 0.44, 96, 36),
               ram =c (25600, 13.2, 0.2, 4300, 3539),
               bias=c(6, -23, -13, -2.8, -3),
               r = c(0.821, 0.861, 0.819, 0.969, 0.974))

ggplot(resource, aes(time, ram, size=abs(bias), colour=r))+geom_point(alpha=0.5)+
  # scale_x_continuous(limits = c(0, 7), breaks = seq(1, 6, 1), 
  #                    labels = c("ms", "s", "min", "hour", "day", "month"))+
  scale_x_log10(breaks = c(1, 60, 3600), labels=c("s", "min", "hour"), limits=c(0.1, 14400))+
  scale_y_log10(limits=c(0.1, 50000), breaks=c(0.1, 1, 1000), labels=c("KB", "MB", "GB"))+
  # scale_y_continuous(limits = c(0, 3), breaks = c(1, 2, 3),
  #                    labels = c("MB", "GB", "TB"))+
  #scale_color_gradient2(low="blue", mid = "white", high="red", midpoint = 0)+
  scale_color_gradientn(colors = c("blue", "orange", "red"))+
  scale_size_area(max_size=12)+
  geom_text_repel(data=resource, aes(time, ram, label=model), colour="black",
                  min.segment.length = Inf, seed = 42, point.padding = 0.25, size=2.5, point.size=NA) +
  theme_bw()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1,0),
        legend.direction = "horizontal",
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        legend.background = element_blank(),
        axis.line = element_line(arrow = arrow(length = unit(0.2, 'cm'))))+
  xlab("Elapsed time")+
  ylab("Required RAM")+
  guides(size = "none",
         colour=guide_colorbar(title = "r",
                               title.position = "top",
                               title.hjust = 0.5))

export::graph2pdf(file="Fig.S12.jpg", width=9/2.54, height=7/2.54)
