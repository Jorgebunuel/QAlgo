######Heikin Ashi
HeikinAshi <- function(Data) {
  AuxData<-Data
  
  #Calcular la apertura = (apertura de la barra anterior + cierre de la barra anterior) dividida por 2
  AuxData$Open<- ((c(NULL,Lag(Data$Open,1)))+(c(NULL,Lag(Data$Close,1))))/2
  #Calcular el cierre = (valor abierto + cerrado + alto + bajo de la barra actual) dividido por 4
  AuxData$Close<- (Data$Open+Data$Close+Data$High+Data$Low)/4
  #The Heikin-Ashi High is the maximum of: the current period's high, the current Heikin-Ashi open or the current Heikin-Ashi close. 
  AuxData$High<- apply(X = cbind(AuxData$Open,AuxData$Close,Data$High), MARGIN = 1, FUN = max)
  #The Heikin-Ashi low is the minimum : the current period's low, the current Heikin-Ashi open or the current Heikin-Ashi close.
  AuxData$Low<- apply(X = cbind(AuxData$Open,AuxData$Close,Data$Low), MARGIN = 1, FUN = min)
  
  Data$Open<-AuxData$Open
  Data$Close<-AuxData$Close
  Data$High<-AuxData$High
  Data$Low<-AuxData$Low
  return(Data)
  chartSeries(xts(OHLC(Data),order.by=(as.Date(Data$Datetime)))['2021-10-5'],theme = chartTheme("white"))
}

##################