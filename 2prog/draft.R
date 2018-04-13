library(data.table)
library(dplyr)
library(tidyr)
y
path <- "C:/Users/b2680605/HotelSVR/1data/"
files <- list.files("C:/Users/b2680605/HotelSVR/1data")

chegadas <- lapply(files, function(x) fread(paste0(path,x)))
chegadas <- rbindlist(chegadas, )


chegadas_1 <- read.csv("C:/Users/b2680605/HotelSVR/chegada_turistas_compilado.csv", sep = ",")
names(chegadas)
chegadas_rio <- subset(chegadas, chegadas$UF == "Rio de Janeiro")
chegadas_rio_jan  <- subset(chegadas_rio, chegadas_rio$Ordem_mes == "1")
reg.model <- lm(Chegadas~Ordem_mes, data=chegadas_rio)
names(dados)[11] <- "ordem_mes"
#Plot
plot(Chegadas~Ordem_mes,
     data=chegadas_rio, 
     pch=16, 
     main = "Regression Model")
# Add the fitted line
abline(reg.model,
       col="red")



names(dados)[9] <- "ano_mes"
dados$ano_mes <- as.Date(dados$ano_mes, format = "%Y-%m")
library(lubridate)
make_date(dados$ano_mes)

anos <- unique(dados$ano)
dados_rio_1 <- dados_rio %>% 
  filter(!Continente == "América do Sul")
