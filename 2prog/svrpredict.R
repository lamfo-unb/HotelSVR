library(e1071)

#Criar modelo de regressão linear
plot(dados_europa$mes_ano,dados_europa$Chegadas, pch=16)
model <- lm(Chegadas ~ mes_ano, dados_europa)
abline(model)

#Fazer previsão 
predicted_Chegadas <- predict(model, dados_europa)

points(dados_europa$mes_ano, predicted_Chegadas, col = "blue", pch=4)

#RMSE

rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- dados_europa$Chegadas - predicted_Chegadas

predictionRMSE <- rmse(error) #35344.5319141946

#Support Vector Regression 

model1 <- svm(Chegadas ~ mes_ano, dados_europa)
predicted_Chegadas_europa <- predict(model1, dados_europa)
points(dados_europa$mes_ano[1:334], predicted_Chegadas_europa, col = "green", pch=4)
error <- dados_europa$Chegadas[1:334] - predicted_Chegadas_europa
svrPredictionRMSE <- rmse(error) #27128.38773665
