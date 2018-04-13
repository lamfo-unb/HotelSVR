library(dplyr)
library(data.table)
library(tidyr)
library(zoo)
library(graphics)
library(stringr)


dados <- fread("chegada_turistas_compilado.csv")
dados <- dados[,-1]
dados <- dados %>% 
  filter(!Continente == "América do Sul")
names(dados)[11] <- "Ordem_mes"
i <- 1
while (i <= dim(dados)[1]){
  if (nchar(dados$Ordem_mes[i]) == 1){
    dados$Ordem_mes[i] <- paste0(0,dados$Ordem_mes[i])
  }
  i <- i + 1
}
dados <- unite(dados, ano_mes, ano, Ordem_mes, sep = "-", remove =  FALSE)

dados_sum <- dados %>% 
  group_by(Continente, País, UF,ano_mes) %>% 
  summarise(sum(Chegadas))

dados_ano <- dados %>% 
  group_by(Continente, ano) %>% 
  summarise()

plot(x= dados_ano$Chegadas, y = dados_ano$ano)



names(dados_sum)[5] <- "Chegadas" 
dates <- dados_sum$ano_mes

z <- as.yearmon(dates)
dados_sum[,"ano_mes"] <- z
names(dados_sum_ano)[5] <- "Chegadas"
str(dados_sum)
write.csv(dados_sum, "chegadas_ano.csv")
plot(x= dados_sum$ano_mes, y = dados_sum$Chegadas)
plot(x= dados_sum_ano$ano, y = dados_sum_ano$Chegadas)

     