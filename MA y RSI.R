library(TTR)
library(rpart)
library(rpart.plot)

datos <- read.csv("C:/Users/Juan Pablo/Downloads/EURUSD60.csv", header=FALSE)
colnames(datos)<-c("Fecha","Hora","Apertura","Max","Min","Cierre","Volumen")

ema.14<- as.data.frame(EMA(datos[,6],14))
colnames(ema.14)<-"ema14"
ema.30<- as.data.frame(EMA(datos[,6],30))
colnames(ema.30)<-"ema30"
rsi.14<- as.data.frame(RSI(datos[,6],14),maType="EMA")
colnames(rsi.14)<-"rsi"
macd<-as.data.frame(MACD(datos[,6],nFast=12,nSlow=26,nSig=9))
ret<-datos[,6]-lag(datos[,6],k=1)
seg<-datos[-1,6]
prim<-datos[-length(datos[,1]),6]
ret<- as.data.frame((seg-prim)/prim)
colnames(ret)<-"ret"
ret<-rbind("NA",ret)

datos<-cbind(datos,ret,ema.14,ema.30,rsi.14,macd,"dir"=rep(0,length(datos[,1])))
datos<-na.omit(datos)
datos[which(datos$ret<0),14]<-"Baja"
datos[which(datos$ret>0),14]<-"Sube"
datos[which(datos$ret==0),14]<-"Mantiene"
dir<- as.data.frame(datos[,14])
dir<-as.data.frame(dir[-1,])
dir<-rbind(dir,NA)
colnames(dir)<-"dir"
datos<-datos[,-14]
datos<-cbind(datos,dir)
datos<-na.omit(datos)
#año,mes,día
datos<-datos[-c(1:64000),]
datos<-datos[,-c(1:5,6,8)]
summary(datos)

mod1<-rpart(datos$dir~.,data=datos)#,control = rpart.control(cp = 0.01))
prp(mod1)
plotcp(mod1)

pred = predict(mod1, type="class")
table(pred)
table(pred, datos$dir)
conf<-table(pred, datos$dir)
sum(diag(conf))/sum(conf)
