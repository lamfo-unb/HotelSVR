library(kernlab)
library(dplyr)

#Variável Dependente
y <- as.numeric(dados_oceania$percent)
y[2] <- 0
y <- y[-1]
y.lag <- as.numeric(dados_oceania$percent)
y.lag <- y.lag[-1]


#Variável Independente 
colunas <- c(3:6)
x <- subset(dados_oceania, select = colunas)
X <- as.matrix(cbind(y.lag, x))
ids <- 1:nrow(dados_oceania)
ids_train <- sample(ids, 0.7*nrow(dados_oceania))
ids_valid <- ids[-ids_train]
ids_valid <- sample(ids_valid, 0.2*nrow(dados_oceania))
ids_test <- ids[-ids_valid]
ids_test <- sample(ids_test, 0.1*nrow(dados_oceania))

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
  K.train <- as.kernelMatrix(K[-ids,-ids]) 
  
  #Training
  svr <- ksvm(K.train, y[-ids], kernel='matrix', type="eps-svr",epsilon=0.1,C=1)
  
  #Validation
  K.Valida <- as.kernelMatrix(K[ids, -ids][,SVindex(svr), drop=F])
  
  #Prediction
  preds <- as.data.frame(predict(svr, K.Valida))
  
  #Observed
  obsv  <- as.data.frame(y[ids])
  obsv[which(is.na(obsv)),1] <- mean(obsv$`y[ids]`, na.rm = TRUE)
  
  #Error
  res<-preds-obsv
  MSE0<- sum(res^2)/length(res) 
  
  #Record
  temp<-data.frame("Kernel"="Mexican","Parameter1"=a.Vet[i],"MSE"=MSE0)
  suportVector<-rbind(suportVector,temp)
}
suportVector<-suportVector[-1,]
write.csv(suportVector,"MexicanHat.csv")
