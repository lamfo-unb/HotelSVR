library(kernlab)
library(dplyr)
library(readxl)
library(ggplot2)


paramsMexican<-read_xlsx("1data/Parametros - Mexican Hat.xlsx")
parmsPolynom<- read_xlsx("1data/Parametros - Polinomial.xlsx")


#Variável Dependente 
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
x <- subset(dados_asia, select = colunas)
X <- as.matrix(cbind(y.lag, x))
ids <- 1:nrow(dados_europa)
ids_train <- sample(ids, 0.7*nrow(dados_europa))
ids_valid <- ids[-ids_train]
ids_valid <- sample(ids_valid, 0.2*nrow(dados_europa))
ids_test <- ids[-ids_valid]
ids_test <- sample(ids_test, 0.1*nrow(dados_europa))

y_test <- y[ids_test]

#Mexican Kernel parameter 
a<-paramsMexican[1,"Parâmetro"]

#Kernel matrix (Mexican Hat)
K<-as.kernelMatrix(apply(X,1,function(x) apply(X,1,function(y) prod((1-(((x-y)^2)/(a^2)))*exp(-((x-y)^2)/(2*a))))))

#Training Kernel
K.train <- as.kernelMatrix(K[ids_test,ids_test]) 

#Training
svr <- ksvm(K.train, y_test, kernel='matrix', type="eps-svr",epsilon=0.1,C=1)
K.pred <- as.kernelMatrix(K[,SVindex(svr), drop=F]) 
preds <- as.data.frame(predict(svr, K.pred))

ggplot(dados_europa) + 
  geom_line(aes(x = mes_ano, y = percent, group = 1)) +
  geom_line(aes(x = dados_europa$mes_ano, y = V1, group = 1), preds, color = "red")


#RMSE
obsv  <- as.data.frame(y[ids])
res<-preds-obsv
MSE0<- sum(res^2)/length(res)
write.csv(MSE0, "ErroPolinomialEuropa.csv")