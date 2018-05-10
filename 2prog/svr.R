library(kernlab)
library(dplyr)
library(tidyr)

#Variável Dependente
dados_europa <- dados_europa[-1,]
y <- as.numeric(dados_europa$percent)
y <- na.omit(y)
y.lag <- as.numeric(dados_europa$percent)
Y <- cbind(y, y.lag)
Y <- Y[-1,]
y <- Y[,1]
y.lag <- Y[,2] 


#Variável Independente 
dados_europa <- na.omit(dados_europa)
colunas <- c(3:6)
x <- subset(dados_europa, select = colunas)
X <- as.matrix(cbind(y.lag, x))
ids <- 1:nrow(dados_europa)
ids_train <- sample(ids, 0.7*nrow(dados_europa))
ids_valid <- ids[-ids_train]
ids_valid <- sample(ids_valid, 0.2*nrow(dados_europa))
ids_test <- ids[-ids_valid]
ids_test <- sample(ids_test, 0.1*nrow(dados_europa))

#Training Process
a.Vet <- seq(0.1,5,0.05)
MSE <- 1e+10
R <- length(a.Vet)

suportVector <- data.frame("Kernel" = "Mexican", "Parameter1" = NA, "MSE" = NA)
for(i in 1:R){
  #Mexican Kernel parameter
  a<-a.Vet[i]
  
  #Kernel matrix
  K<-as.kernelMatrix(apply(X, 1, function(x) apply(X, 1, function(y) prod((1-(((x-y)^2)/(a^2)))*exp(-((x-y)^2)/(2*a))))))
  
  #Training Kernel
  K.train <- as.kernelMatrix(K[-ids_valid,-ids_valid]) 
  
  #Training
  svr <- ksvm(K.train, y[-ids_valid], kernel='matrix', type="eps-svr",epsilon=0.1,C=1)
  
  #Validation
  K.Valida <- as.kernelMatrix(K[ids_valid, -ids_valid][,SVindex(svr), drop=F])
  
  #Prediction
  preds <- as.data.frame(predict(svr, K.Valida))
  
  #Observed
  obsv  <- as.data.frame(y[ids_valid])
  obsv[which(is.na(obsv)),1] <- mean(obsv$`y[ids_valid]`, na.rm = TRUE)
  
  #Error
  res<-preds-obsv
  MSE0<- sum(res^2)/length(res) 
  
  #Record
  temp<-data.frame("Kernel"="Mexican","Parameter1"=a.Vet[i],"MSE"=MSE0)
  suportVector<-rbind(suportVector,temp)
}
suportVector_europa <-suportVector[-1,]
paramsMexican<-suportVector_europa[which.min(suportVector_europa$MSE),]
write.csv(suportVector,"MexicanHat.csv")


############################################## Polynomial 

#Training Process
scale<-seq(0.1,5,0.05)
power<-seq(1,5,1)
a.Vet<-expand.grid(scale,power)
MSE<-1e+10
R <- nrow(a.Vet)

suportVector<-data.frame("Kernel"="Polinomial","Parameter1"=NA,"Parameter2"=NA,"MSE"=NA)
for(i in 1:R){
  #Polinomual Kernel parameter
  scale<-a.Vet[i,1]
  power<-a.Vet[i,2]
  
  #Kernel matrix
  K<-as.kernelMatrix(apply(X,1,function(x) apply(X,1,function(y) 
    (scale*sum(x*y))^(power))))
  
  #Training Kernel
  K.train <- as.kernelMatrix(K[-ids_valid,-ids_valid]) 
  
  #Training
  svr <- ksvm(K.train, y[-ids_valid], kernel='matrix', type="eps-svr",epsilon=0.1,C=1)
  
  #Validation
  K.Valida <- as.kernelMatrix(K[ids_valid, -ids_valid][,SVindex(svr), drop=F])
  
  #Prediction
  preds <- as.data.frame(predict(svr, K.Valida))
  
  #Observed
  obsv  <- as.data.frame(y[ids_valid])
  
  #Error
  res<-preds-obsv
  MSE0<- sum(res^2)/length(res) 
  
  #Record
  temp<-data.frame("Kernel"="Polinomial","Parameter1"=scale,"Parameter2"=power,"MSE"=MSE0)
  suportVector<-rbind(suportVector,temp)
}
suportVector<-suportVector[-1,]
write.csv(suportVector,"Polinomial.csv")
