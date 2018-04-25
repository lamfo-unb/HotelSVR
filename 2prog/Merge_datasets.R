rm(list=ls())
library(tidyr)
library(ggplot2)
## Function to read data
path <- "C:/Users/b2680605/HotelSVR/1data/"
files <- list.files("C:/Users/b2680605/HotelSVR/1data")
files_list <- paste(path, files, sep = "")
func <- function(x){
  d <- read.csv(x, header = T, sep = ";")
  colnames(d) <-  c("Continente", "Ordem continente", "País", "Ordem país", "UF", "Ordem UF", "Via de acesso", "Ordem via de acesso", "ano", "Mês", "Ordem mês", "Chegadas")
  return(d)
}

## Merge files into one csv
dados <- lapply(files_list , function(x) func(x)) %>%
  bind_rows
dados <- dados[,-13]
dados <- na.omit(dados)

#write.csv(dados, file = "chegada_turistas_compilado.csv")

names(dados)[11] <- "Ordem_mes" 

i <- 1
while (i <= dim(dados)[1]){
  if (nchar(dados$Ordem_mes[i]) == 1){
    dados$Ordem_mes[i] <- paste0(0,dados$Ordem_mes[i])
  }
  i <- i + 1
}
  
dados <- unite(dados, mes_ano,Ordem_mes, ano, sep = "-", remove = FALSE)
i <- 1
while (i <= dim(dados)[1]){
  if (nchar(dados$mes_ano[i]) == 7){
    dados$mes_ano[i] <- paste0("01-",dados$mes_ano[i])
  }
  i <- i + 1
}

dados$mes_ano <- as.Date(dados$mes_ano, format = "%d-%m-%Y")
ggplot(filter(dados, ano >= 2015)) +
  geom_bar(aes(x = mes_ano, y = Chegadas), stat = "identity")



#Sazonalidade 
dados$verao <- NA
dados$primavera <- NA
dados$inverno <- NA
dados$outono <- NA
i <- 1
while (i <= dim(dados)[1]){
  if(dados$Ordem_mes[i] %in% c("09", "10","11")){
         dados$primavera[i] <- 1
       } else {dados$primavera[i] <- 0}
     i <- i + 1
}
i <- 1
while (i <= dim(dados)[1]){
  if(dados$Ordem_mes[i] %in% c("01", "02", "12")){
    dados$verao[i] <- 1
  } else {dados$verao[i] <- 0}
  i <- i + 1
}
i <- 1
while (i <= dim(dados)[1]){
  if(dados$Ordem_mes[i] %in% c("03", "04", "05")){
    dados$outono[i] <- 1
  } else {dados$outono[i] <- 0}
  i <- i + 1
}
i <- 1
while (i <= dim(dados)[1]){
  if(dados$Ordem_mes[i] %in% c("06","07","08")){
    dados$inverno[i] <- 1
  } else {dados$inverno[i] <- 0}
  i <- i + 1
}

write.csv(dados, file = "chegada_turistas_compilado.csv")
