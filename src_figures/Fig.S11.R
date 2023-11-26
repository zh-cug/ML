rm(list = ls())

setwd("./Figdata/")
load("td.Rdata")
bias<-read.csv("bias.csv")

a<-ggplot(subset(bias, type=="EMI"), aes(benchmark, model, fill=bias))+
  geom_tile()+
  scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 0)+
  theme_bw()+
  theme(legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.position = "right",
        legend.box.spacing = unit(0.4, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),#space between different legend
        panel.background = element_blank(),
        axis.title = element_text(size=9),
        legend.title = element_text(vjust = 1, size=8),
        axis.text = element_text(size=8),
        legend.text = element_text(size=7),
        legend.margin = margin(l=-5, 0, 0, 0))+
  xlab("Reference model")+
  ylab("Model")+
  guides(fill=guide_colorbar(title = expression("Bias in PM"[2.5]^EMI*" (%)"),
                             title.position = "right",
                             title.theme = element_text(angle = 90, size=8),
                             title.hjust = 0.5))

b<-ggplot(subset(bias, type=="MET"), aes(benchmark, model, fill=bias))+
  geom_tile()+
  scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 0)+
  theme_bw()+
  theme(legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.position = "right",
        legend.box.spacing = unit(0.4, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),#space between different legend
        panel.background = element_blank(),
        axis.title = element_text(size=9),
        legend.title = element_text(vjust = 1, size=8),
        axis.text = element_text(size=8),
        legend.text = element_text(size=7),
        legend.margin = margin(l=-5, 0, 0, 0))+
  xlab("Reference model")+
  ylab("Model")+
  guides(fill=guide_colorbar(title = expression("Bias in PM"[2.5]^MET*" (%)"),
                             title.position = "right",
                             title.theme = element_text(angle = 90, size=8),
                             title.hjust = 0.5))

cowplot::plot_grid(a, b, ncol=1, labels = letters[1:2], label_size = 9)

export::graph2jpg(file="Fig.S11.jpg", width=9/2.54, height=12/2.54)
