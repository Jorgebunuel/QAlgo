#1V0 DIRECTORIO DE TRABAJO ##############################################################
#masterPath<-"D:/DATOS_GESTAMP/jorge_datos/TRADING_QUANTITATIVO/01Dise?oSistema/"
masterPath<-"./"
#1V0 DIRECTORIO DE TRABAJO ##############################################################
source(file = paste0(masterPath,"Librerias/all.R"))
source(file = paste0(masterPath,"BIBLIOTECA/ReadFunXTB.R"))

#2v0 datos ##############################################################
Data<-LecturaXTBForex("EURUSD","1D")
#2v0 datos ##############################################################

#3v0 Indicadores ##############################################################
nPeriodos<-14
nOfset<-1
dsma<-3
#############################################
Data$aux<-(c(NULL,Lag(Data$Close,1)))
Data$RetLog<-log(Data$Close/Data$aux)

Data$AcumRLogTotal<-0
Data$AcumRLogB<-0
Data$AcumRLogC<-0
Data$Posicion<-"Pending"
#Definimos el filtro de cruce
Data$smafilter<-SMA(Cl(Data),dsma)
#calculamos el atr
Data$ATR_Data<-as.data.frame(ATR(HLC(Data),nPeriodos))
#caluclamos el up level -
Data$Up_Lev<-(Hi(Data)+Lo(Data))/2-nOfset*Data$ATR_Data$atr
#calculamos el dn level +
Data$Dn_Lev<-(Hi(Data)+Lo(Data))/2+nOfset*Data$ATR_Data$atr
#generamos lagas de las se?ales anteriores
Data$Up_Lev_1<-Lag(Data$Up_Lev,k = 1)
Data$Dn_Lev_1<-Lag(Data$Dn_Lev,k = 1)
#predefinimos variables que seran modificadas en el loop
#Data$UP_Trend<-Data$Up_Lev
#Data$down_Trend<-Data$Up_Lev
#Data$LimiteConfianza<-Data$Up_Lev
#definimos los niveles de tendencia
Data$UP_Trend<-0
Data$down_Trend<-0
Data$LimiteConfianza<-1
#definimos cuando devemos de iniciar el loop
starting<-nPeriodos+max(nPeriodos,nOfset,dsma)
i<-starting
Data$NOperacion<-0
Data$NdiaOperacion<-0
for (i in c(starting:nrow(Data))) {
  
  #              up_trend   := close[1] > up_trend[1]   ?             max(          up_lev,           up_trend[1])   : up_lev
  Data$UP_Trend[i]<-ifelse(Data$smafilter[i-1]>Data$UP_Trend[i-1],max(Data$Up_Lev[i],Data$UP_Trend[i-1]),Data$Up_Lev[i])
  
  #down_trend :=                       close[1] < down_trend[1] ?         min(dn_lev,                     down_trend[1]) : dn_lev
  Data$down_Trend[i]<-ifelse(Data$smafilter[i-1]<Data$down_Trend[i-1],min(Data$Dn_Lev[i],Data$down_Trend[i-1]),Data$Dn_Lev[i])
  
  # trend :=                      close >            down_trend[1] ? 1:                 close < up_trend[1] ? -1 : nz(trend[1], 1)
  if (as.numeric(Data$smafilter[i])>as.numeric(Data$down_Trend[i-1])) {
    
    Data$LimiteConfianza[i]<-Data$down_Trend[i-1]
    Data$Posicion[i]<-"Long"
    
  }else{
    if (as.numeric(Data$smafilter[i])<as.numeric(Data$UP_Trend[i-1])) {
      
      Data$LimiteConfianza[i]<-Data$UP_Trend[i-1]
      Data$Posicion[i]<-"Short"
      
    }else{
      
      Data$LimiteConfianza[i]<-Data$LimiteConfianza[i-1]
      #Data$Posicion[i]<-substr(Data$Posicion[i-1], 1, 1) 
      Data$Posicion[i]<-Data$Posicion[i-1]  
      
    }
  }
  #Data$AcumRLog$A[i]<-ifelse(Data$Posicion[i]=="L",Data$RetLog[i]+Data$AcumRLog[i-1],-Data$RetLog[i]+Data$AcumRLog[i-1])
  #Data$AcumRLogA[i]<-ifelse(Data$Posicion[i]=="L",Data$RetLog[i]+Data$AcumRLogA[i-1],
  #                           ifelse(Data$Posicion[i]=="S",-Data$RetLog[i]+Data$AcumRLogA[i-1],+Data$AcumRLogA[i-1]))
  #
  #Data$AcumRLogB[i]<-ifelse(Data$Posicion[i]=="L"|Data$Posicion[i]=="Short",Data$RetLog[i]+Data$AcumRLogB[i-1],
  #                           ifelse(Data$Posicion[i]=="S"|Data$Posicion[i]=="Long",-Data$RetLog[i]+Data$AcumRLogB[i-1],+Data$AcumRLogB[i-1]))
  #
  #Data$AcumRLogC[i]<-ifelse(Data$Posicion[i]=="Short",Data$RetLog[i]+Data$AcumRLogC[i-1],
  #                           ifelse(Data$Posicion[i]=="Long",-Data$RetLog[i]+Data$AcumRLogC[i-1],+Data$AcumRLogC[i-1]))
  
  Data$AcumRLogTotal[i]<-ifelse(
  (Data$Posicion[i]=="Long" & Data$Posicion[i]==Data$Posicion[i-1])
  |
  (Data$Posicion[i]=="Short"&Data$Posicion[i-1]=="Long")
  ,Data$RetLog[i]+Data$AcumRLogTotal[i-1],
  ifelse(
  (Data$Posicion[i]=="Short"&Data$Posicion[i]==Data$Posicion[i-1])
  |
  (Data$Posicion[i]=="Long"&Data$Posicion[i-1]=="Short"),
  -Data$RetLog[i]+Data$AcumRLogTotal[i-1],
  +Data$AcumRLogTotal[i-1]))
  
  #Se considera que la vela en el que se ejecuta el cambio es el FIN de la operacion anterior
  Data$NOperacion[i]<-ifelse(Data$Posicion[i]==Data$Posicion[i-1]&Data$Posicion[i-1]!=Data$Posicion[i-2],
                             Data$NOperacion[i-1]+1,Data$NOperacion[i-1]
  )
  Data$NdiaOperacion[i]<-ifelse(Data$Posicion[i]==Data$Posicion[i-1]&Data$Posicion[i-1]!=Data$Posicion[i-2],
                                  1,Data$NdiaOperacion[i-1]+1
  )
}


#write.csv(Data,"D:/DATOS_GESTAMP/jorge_datos/TRADING_QUANTITATIVO/01Dise?oSistema/ESTRATEGIAS/limiteconfi.txt")

Data%>%
  #filter(LimiteConfianza>1.08)%>%
  #filter(Datetime>"2020-09-22 02:00:00")%>%
  #ggplot(aes(x=Datetime,y = LimiteConfianza,colour=Posicion))+geom_point(aes(x=Datetime,y=Close))+geom_line(aes(x=Datetime,y = LimiteConfianza))
  ggplot(aes(x=Datetime,y = AcumRLogTotal,colour=Posicion))+geom_point()
Data%>%
  filter(Posicion!="Pending")%>%
  group_by(NdiaOperacion,Posicion)%>%
  summarise(sum=mean(RetLog))%>%
  ggplot(aes(x=NdiaOperacion,y=sum,fill=Posicion))+geom_col()

Data%>%
  filter(Posicion!="Pending")%>%
  group_by(NOperacion,Posicion)%>%
  summarise(sum=sum(RetLog),sd=sd(RetLog),S=sum/sd)%>%
  ggplot(aes(x=sd,y=sum,colour=Posicion))+geom_point()
Data%>%
  
  group_by(NOperacion,Posicion)%>%
  summarise(sum=sum(RetLog),sd=sd(RetLog),S=sum/sd)%>%
  filter(S>-5)%>%
  ggplot(aes(x=S,fill=Posicion))+geom_density(alpha=0.5)

max(Data$NOperacion)
table(Data$Posicion)/nrow(Data)*100

sum(Data$RetLog[Data$Posicion=="L"])
sum(Data$RetLog[Data$Posicion=="S"])
sum(Data$RetLog[Data$Posicion=="Long"])
sum(Data$RetLog[Data$Posicion=="Short"])






Data$Estado<-"Espera"
Data$aux<-(c(NULL,lags(Data$Close,1)[1]))
Data$RetLog<-log(Data$Close/Data$aux[1])
Data$AcumRLog<-0
Data$SMAL<-SMA(Data$Close,10)
Data$Se?al<-SMA(Data$Close,2)
#eliminacion datos mas restricitivos
Data<-Data[!is.na(Data$SMAL),]
#eliminacion datos mas restricitivos
Data$Posicion<-ifelse(Data$SMAL>Data$Se?al,"UP","DW")
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
