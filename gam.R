setwd("C:/Users/xupeng/Desktop/HNDenv")
getwd()
#设置工作路径
rm(list=ls())#调用R包
library(tidygam)
library(mgcv)
library(dplyr)
library(ggplot2)

#读取数据
#丰度
chem <-  read.csv("SEM.csv", row.names = 1)
gs<-gam( CI ~ s(Pic, k = 3), data = chem ,family=poisson)
summary(gs)
gs_pred<-predict_gam(gs)
gs_pred %>%plot(series="Pic")
predict_gam(gs,tran_fun=exp)%>% plot(series = "Pic")

