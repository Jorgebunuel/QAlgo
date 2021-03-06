
library(GGally)
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(plotly)
library(alphavantager)
library(quantmod)
library(quantstrat)

#1V0 DIRECTORIO DE TRABAJO ##############################################################
#masterPath<-"D:/DATOS_GESTAMP/jorge_datos/TRADING_QUANTITATIVO/01Dise?oSistema/"
masterPath<-"./"
#1V0 DIRECTORIO DE TRABAJO ##############################################################
library(PerformanceAnalytics)
<<<<<<< HEAD
library(tidyquant)
=======
>>>>>>> b2dab97be56a627b05a581994d0e4fa45d22d314
source(file = paste0(masterPath,"Librerias/all.R"))
source(file = paste0(masterPath,"BIBLIOTECA/ReadFunXTB.R"))

############################################################################################
## Analysis del Grafico
############################################################################################
EURUSD<-LecturaXTBForex("EURUSD","1h")
EURJPY<-LecturaXTBForex("EURJPY","1h")
<<<<<<< HEAD
names(EURUSD)

EURUSD$Ret<-c(0,diff(log((EURUSD$Close))))
EURJPY$Ret<-c(0,diff(log((EURJPY$Close))))

EURUSD%>%
  mutate(M=month(Datetime),Y=year(Datetime))%>%
  group_by(M,Y)%>%
  summarise(R=sum(Ret),sd=sd(Ret))%>%
  ggplot(aes(x=sd,y=R,colour=as.factor(Y)))+geom_point()


D1<-EURUSD%>%
  mutate(M=month(Datetime),Y=year(Datetime),d=day(Datetime))%>%
  group_by(M,Y,d)%>%
  summarise(R=sum(Ret),sd=sd(Ret),fx="EURUSD")
D2<-EURJPY%>%
  mutate(M=month(Datetime),Y=year(Datetime),d=day(Datetime))%>%
  group_by(M,Y,d)%>%
  summarise(R=sum(Ret),sd=sd(Ret),fx="EURJPY")
D<-rbind(D1,D2)
D%>%
  ggplot(aes(x=sd,y=R,colour=as.factor(M)))+geom_point()
D%>%
  mutate(sharpe=R/sd)%>%
  filter(sharpe<2&sharpe>(-2))%>%
  ggplot(aes(y=sharpe,fill=fx,x=as.factor(M)))+geom_boxplot(outlier.shape = NA)
plot(EURUSD$Close)








serie1<-xts(EURUSD$Close,order.by=(as.Date(EURUSD$Datetime)))
serie2<-xts(EURJPY$Close,order.by=(as.Date(EURJPY$Datetime)))

diff(log((serie1)))[-1]
comparison <- cbind(cumsum( diff(log((serie1)))[-1]), cumsum( diff(log((serie2)))[-1]))
colnames(comparison)  <- c("s1", "s2")
chart.TimeSeries(comparison, legend.loc = "topleft",colorset =c("green", "red"))
charts.PerformanceSummary(cbind(( diff(log((serie1)))[-1]), ( diff(log((serie2)))[-1])))
t(table.CalendarReturns(comparison))
chart.Boxplot(comparison)
chart.Correlation(comparison)
chart.RiskReturnScatter(comparison, Rf = .04/12, add.boxplots = TRUE)


layout(rbind(c(1,2),c(3,4)))
chart.Histogram(comparison[,1,drop=F], main = "Plain", methods = NULL)
chart.Histogram(comparison[,1,drop=F], main = "Density", breaks=40, methods = c("add.density", "add.normal"))
chart.Histogram(comparison[,1,drop=F], main = "Skew and Kurt", methods = c("add.centered", "add.rug"))
chart.Histogram(comparison[,1,drop=F], main = "Risk Measures", methods = c("add.risk"))


table.CAPM(comparison,comparison, Rf = comparison)



=======
>>>>>>> b2dab97be56a627b05a581994d0e4fa45d22d314


serie1<-xts(EURUSD$Close,order.by=(as.Date(EURUSD$Datetime)))
serie2<-xts(EURJPY$Close,order.by=(as.Date(EURJPY$Datetime)))

diff(log((serie1)))[-1]
comparison <- cbind(cumsum( diff(log((serie1)))[-1]), cumsum( diff(log((serie2)))[-1]))
colnames(comparison)  <- c("s1", "s2")
chart.TimeSeries(comparison, legend.loc = "topleft",colorset =c("green", "red"))

