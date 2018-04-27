library(kernlab)
library(dplyr)

#Variável Dependente 
y <- as.numeric(dados_europa$percent)
y[2] <- 0
y <- na.omit(y)
y.lag <- as.numeric(dados_europa$percent)
y.lag[1:2] <- 0

#Variável Independente 
colunas <- c(3:6)
x <- subset(dados_europa, select = colunas)
X <- as.matrix(cbind(y.lag, x))
ids<-sample(1:nrow(dados_europa),0.7*nrow(dados_europa))

#Training Process
a.Vet<-seq(0.1,5,0.05)
MSE<-1e+10
R <- length(a.Vet)

suportVector<-data.frame("Kernel"="Mexican","Parameter1"=NA,"MSE"=NA)
for(i in 1:R){
  #Mexican Kernel parameter
  a<-a.Vet[i]
  
  #Kernel matrix
  K<-as.kernelMatrix(apply(X,1,function(x) apply(X,1,function(y) prod((1-(((x-y)^2)/(a^2)))*exp(-((x-y)^2)/(2*a))))))
  
  #Training Kernel
  K.train <- as.kernelMatrix(K[-ids,-ids]) 
  
  #Training
  svr <- ksvm(K.train, y[-ids], kernel='matrix', type="eps-svr",epsilon=0.1,C=1)
  
  #Validation
  K.Valida <- as.kernelMatrix(K[ids, -ids][,SVindex(svr), drop=F])
  
  #Prediction
  preds <- as.data.frame(predict(svr, K.Valida))
  
  #Observed
  obsv  <- as.data.frame(y[ids])
  
  #Error
  res<-preds-obsv
  MSE0<- sum(res^2)/length(res) 
  
  #Record
  temp<-data.frame("Kernel"="Mexican","Parameter1"=a.Vet[i],"MSE"=MSE0)
  suportVector<-rbind(suportVector,temp)
}
suportVector<-suportVector[-1,]
write.csv(suportVector,"MexicanHat.csv")
