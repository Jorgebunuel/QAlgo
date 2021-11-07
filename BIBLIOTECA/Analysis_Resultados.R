
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
#install.packages("quantstrat")

source(file = "D:/Users/jjbunuel/Desktop/TRADING_QUANTITATIVO/BIBLIOTECA/SQL_CALLS.R")
source(file = "D:/Users/jjbunuel/Desktop/TRADING_QUANTITATIVO/BIBLIOTECA/Funciones_Datos_historicos.R")
source(file = "D:/Users/jjbunuel/Desktop/TRADING_QUANTITATIVO/BIBLIOTECA/ESTRATEGIAS.R")

############################################################################################
## Analysis del Grafico
############################################################################################
DATA_ALL<-read.csv(file = "D:/Users/jjbunuel/Desktop/TRADING_QUANTITATIVO/MSFT.txt")
TD<-nrow(DATA_ALL)
DTr<-round(TD*0.66,0)
DTs<-TD-DTr

DATA_ALL%>%
  ggplot(aes(x=as.Date(DayTime),y=C))+geom_line()+geom_vline(xintercept = as.Date(DATA_ALL$DayTime[DTr]))


############################################################################################
## Llamada datos
############################################################################################





DATA_ALL<-QResults_DB_BT_1(818)
summary(DATA_ALL)

DATA_ALL%>%
  mutate(BT=ifelse(RC>0,"G","P"))%>%
  ggplot(aes(x=RC_Ts_n,fill=BT))+geom_histogram()


DATA_ALL_new<-DATA_ALL%>%
  group_by(nn)%>%
  mutate(best_index=ifelse(max(RC)==RC, 1, 0))

DATA_ALL_new<-DATA_ALL_new%>%
  filter(best_index==1)





