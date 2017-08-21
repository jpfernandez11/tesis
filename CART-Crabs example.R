library(rpart)
library(rpart.plot)
View(crabs)
datos<-crabs
datos<- cbind(datos,control=datos[,1])
datos$control <- paste(datos$sp,datos$sex,sep="")
datos<-datos[,-c(1,2,3)]
a<-rpart(datos$control~.,data=datos,control = rpart.control(cp = 0.05))
prp(a)
plotcp(a)

pred = predict(a, type="class")
table(pred)
table(pred, datos$control)

b<-rpart(datos$control~.,data=datos)#,control = rpart.control(cp = 0.01))
prp(b)
plotcp(b)

predb = predict(b, type="class")
table(predb)
conf<-table(predb, datos$control)
sum(diag(conf))/sum(conf)

b$cptable

cptable <- as.data.frame(b$cptable)
cptable$errsd <- cptable$xerror + cptable$xstd
cpvalue <- cptable[which.min(cptable$errsd),"CP"]
#Find out the best CP value for tree
pruneModel <- prune(b,cpvalue)
#prune the tree
rpart.plot(pruneModel)

predprune = predict(pruneModel, type="class")
table(predprune)
conf<-table(predprune, datos$control)
sum(diag(conf))/sum(conf)
