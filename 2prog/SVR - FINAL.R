library(kernlab)
library(dplyr)
library(readxl)
library(ggplot2)


paramsMexican<-read_xlsx("C:/Users/b2680605/HotelSVR/1data/Parametros - Mexican Hat.xlsx")
parmsPolynom<- read_xlsx("ParametroPolinomial.xlsx")


#Variável Dependente 
treino <- filter(dados_europa,  mes_ano < "2012-01-01" & mes_ano > "1989-02-01" )
treino <- treino[,-1]
y <- as.numeric(treino$percent)
y <- na.omit(y)
y.lag <- as.numeric(treino$percent)
y.lag[2] <- 0
y.lag <- na.omit(y.lag)

#Variável Independente 
colunas <- c(3:6)
x <- subset(treino, select = colunas)
X <- as.matrix(cbind(y.lag, x))
ids<-sample(1:nrow(treino))

#Mexican Kernel parameter 
a<-paramsMexican[1,"Parametro"]

#Kernel matrix (Mexican Hat)
K<-as.kernelMatrix(apply(X,1,function(x) apply(X,1,function(y) prod((1-(((x-y)^2)/(a^2)))*exp(-((x-y)^2)/(2*a))))))

#Training Kernel
K.train <- as.kernelMatrix(K[ids,ids]) 

#Training
svr <- ksvm(K.train, y, kernel='matrix', type="eps-svr",epsilon=0.1,C=1)
K.pred <- as.kernelMatrix(K[,SVindex(svr), drop=F]) 
preds <- as.data.frame(predict(svr, K.pred))
treino$preds <- preds

ggplot(treino, aes(x = mes_ano, y = percent)) + geom_point()

