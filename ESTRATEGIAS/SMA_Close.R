#1V0 DIRECTORIO DE TRABAJO ##############################################################
masterPath<-"D:/DATOS_GESTAMP/jorge_datos/TRADING_QUANTITATIVO/01DiseñoSistema/"
#1V0 DIRECTORIO DE TRABAJO ##############################################################
source(file = paste0(masterPath,"Librerias/all.R"))
source(file = paste0(masterPath,"BIBLIOTECA/ReadFunXTB.R"))

#2v0 datos ##############################################################
Data<-LecturaXTBForex("EURUSD","1h")
#2v0 datos ##############################################################

#3v0 Indicadores ##############################################################
Data$Estado<-"Espera"
Data$aux<-(c(NULL,lags(Data$Close,1)[1]))
Data$RetLog<-log(Data$Close/Data$aux[1])
Data$AcumRLog<-0
Data$SMAL<-SMA(Data$Close,10)
Data$Señal<-SMA(Data$Close,2)
#eliminacion datos mas restricitivos
Data<-Data[!is.na(Data$SMAL),]
#eliminacion datos mas restricitivos
Data$Posicion<-ifelse(Data$SMAL>Data$Señal,"UP","DW")
#3v0 Indicadores ##############################################################


#4V0 Loop ##############################################################
for (n in c(2:nrow(Data))) {
  if (Data$Estado[n]=="Espera") {
    
    #if (Data$Posicion[n-1]==Data$Posicion[n]) {
    #  
    #}
    Data$AcumRLog[n]<-ifelse(Data$Posicion[n]=="UP",Data$RetLog[n]+Data$AcumRLog[n-1],-Data$RetLog[n]+Data$AcumRLog[n-1])

  }
}
#4v0 Loop ##############################################################
plot(Data$AcumRLog)
