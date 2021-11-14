
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
source(file = paste0(masterPath,"Librerias/all.R"))
source(file = paste0(masterPath,"BIBLIOTECA/ReadFunXTB.R"))

############################################################################################
## Analysis del Grafico
############################################################################################
EURUSD<-LecturaXTBForex("EURUSD","1h")
EURJPY<-LecturaXTBForex("EURJPY","1h")


serie1<-xts(EURUSD$Close,order.by=(as.Date(EURUSD$Datetime)))
serie2<-xts(EURJPY$Close,order.by=(as.Date(EURJPY$Datetime)))

diff(log((serie1)))[-1]
comparison <- cbind(cumsum( diff(log((serie1)))[-1]), cumsum( diff(log((serie2)))[-1]))
colnames(comparison)  <- c("s1", "s2")
chart.TimeSeries(comparison, legend.loc = "topleft",colorset =c("green", "red"))

