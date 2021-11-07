library(stringr)
library(data.table)
library(ggplot2)
library(dplyr)
#funcion de selecion de timeframe
PathXTBDATA <- function(timeframe) {
  #path<-paste0("D:/DATOS_GESTAMP/jorge_datos/TRADING_QUANTITATIVO/XTB_VisualStudio/Data_XTB/",timeframe)
  #path<-paste0("../",timeframe)
  path<-paste0(timeframe)
  return(path)
}
#funcion de lectura ficheros forex
LecturaXTBForex <- function(symbol,timeframe) {
  #PathXTBDATA
  path<-paste0(PathXTBDATA(timeframe),"/",symbol,".txt")
  Data<-as.data.frame(read.csv2(path,sep = ";",dec = ",",header = T))
  for (i in 2:ncol(Data)-1) {
    Data[,i]<-as.numeric(str_replace(Data[,i],",","."))
  }
  Data$Datetime<-Data$Datetime/1000
  Data$Datetime<-as.POSIXct(as.numeric(Data$Datetime),origin="1970-01-01", tz="GMT" )
  Data$Open <- Data$Open / 10000
  Data$High <- Data$High/ 10000
  Data$Low <- Data$Low / 10000
  Data$Close<- Data$Close / 10000
  return(Data)

}
#anlisis de calidad de los ficheros
DataTimeCheck <- function(Data,maxfilter=100,minfilter=1) {
  Data$timeDelay<-Data$Datetime-shift(Data$Datetime,1)
  plot<-Data%>%
    filter(timeDelay<maxfilter&timeDelay>minfilter)%>%
    mutate(y=as.factor(year(Datetime)))%>%
    ggplot(aes(x=timeDelay,fill=y))+geom_histogram()
  return(plot)
  
}
Data<-LecturaXTBForex("EURUSD","1h")
DataTimeCheck(Data,5,1)


summary(Data)

  





