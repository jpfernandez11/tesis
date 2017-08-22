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
bands<-as.data.frame(PBands(datos[,6],n=20,maType="SMA",sd=2))
wpr<-as.data.frame(WPR(datos[,6],n=14))
colnames(wpr)<-"wpr"
roc<-as.data.frame(ROC(datos[,6], n = 1, type = "discrete", na.pad = TRUE))
colnames(roc)<-"roc"
ret<-datos[,6]-lag(datos[,6],k=1)
seg<-datos[-1,6]
prim<-datos[-length(datos[,1]),6]
ret<- as.data.frame((seg-prim)/prim)
colnames(ret)<-"ret"
ret<-rbind("NA",ret)

datos<-cbind(datos,ret,ema.14,ema.30,rsi.14,macd,"dir"=rep(0,length(datos[,1])),bands,wpr,roc)
datos<-datos[-c(1:64600),]
datos<-na.omit(datos)
#returns
ret<-as.numeric(datos$ret)

datos[,14]<-"Mantiene"
datos[which(as.numeric(datos$ret)<=quantile(ret,.30)),14]<-"Baja"
datos[which(as.numeric(datos$ret)>=quantile(ret,.70)),14]<-"Sube"
summary(as.factor(datos[,14]))
dir<- as.data.frame(datos[,14])
dir<-as.data.frame(dir[-1,])
dir<-rbind(dir,NA)
colnames(dir)<-"dir"
datos<-datos[,-14]
datos<-cbind(datos,dir)
datos<-na.omit(datos)

#datos<-datos[-c(1:64000),]
datos<-datos[,-c(1:5)]
datos$ret<-as.numeric(datos$ret)
#Dividir en entrenamiento y predicción
datospred<- datos[301:999,]
datos<-datos[1:300,]
summary(datos)
summary(datospred)

mod1<-rpart(datos$dir~.,data=datos)#,control = rpart.control(cp = 0.01))
prp(mod1)
plotcp(mod1)

pred = predict(mod1, type="class")
table(pred)
table(pred, datos$dir)
conf<-table(pred, datos$dir)
sum(diag(conf))/sum(conf)

pred1=predict(mod1,type="class",newdata=datospred)
table(pred1)
table(pred1, datospred$dir)
conf<-table(pred1, datospred$dir)
sum(diag(conf))/sum(conf)

