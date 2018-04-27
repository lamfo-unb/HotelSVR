library(e1071)

#Criar modelo de regressão linear
plot(dados_europa$mes_ano[3:336],dados_europa$percent[3:336], pch=16)
model <- lm(percent[3:336] ~ mes_ano[3:336], dados_europa)
abline(model)

#Fazer previsão 
predicted_percent <- predict(model, dados_europa)

points(dados_europa$mes_ano[3:336], predicted_percent, col = "blue", pch=4)

#RMSE

rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- dados_europa$percent[3:336] - predicted_percent

predictionRMSE <- rmse(error) #35344.5319141946

#Support Vector Regression 

model1 <- svm(percent ~ mes_ano, dados_europa)
predicted_percent_europa <- predict(model1, dados_europa)
points(dados_europa$mes_ano[1:334], predicted_percent_europa, col = "green", pch=4)
error <- dados_europa$percent[3:336] - predicted_percent_europa
svrPredictionRMSE <- rmse(error) #27128.38773665


#Tunar resultado 

tuneResult <- tune(svm, percent ~ mes_ano,  data = dados_europa,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tuneResult)






