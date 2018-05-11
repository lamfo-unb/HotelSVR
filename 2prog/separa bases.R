library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rworldmap)
#separar base por continente
dados <- fread("chegada_turistas_compilado.csv")
dados_africa <- dados %>% 
  select(Continente, mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "África") %>% 
  group_by(Continente, mes_ano,  verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

dados_europa <- dados %>% 
  select(Continente, mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "Europa") %>% 
  group_by(Continente, mes_ano, verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

dados_americacentral <- dados %>% 
  select(Continente, mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "América Central e Caribe") %>% 
  group_by(Continente, mes_ano,  verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

dados_americanorte <- dados %>% 
  select(Continente, mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "América do Norte") %>% 
  group_by(Continente, mes_ano,  verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

dados_asia <- dados %>% 
  select(Continente,  mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "Ásia") %>% 
  group_by(Continente, mes_ano,  verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

dados_oceania <- dados %>% 
  select(Continente, mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "Oceania") %>% 
  group_by(Continente, mes_ano,  verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

dados_naoespecificado <- dados %>% 
  select(Continente, mes_ano, Chegadas, verao, primavera, outono, inverno) %>% 
  filter(Continente == "Continente não especificado") %>% 
  group_by(Continente, mes_ano,  verao, primavera, outono, inverno) %>% 
  summarise(Chegadas = sum(Chegadas))

#Função para calculo do retorno
calcula_retorno <- function(x, k){
  d <- c(NA, diff(x, lag = k))
  y <- c(rep(NA, k), lag(x, k = k))[1:length(x)]
  z <- d/y
  return(z)
}

calcula_logretorno <- function(x, k){
  d <- c(NA, diff(log(x), lag = k))
  y <- c(rep(NA, k), lag(x, k = k))[1:length(x)]
  z <- d/y
  return(z)
}

#Criar Defasagens usando retorno discreto
dados_africa$percent <- calcula_retorno(dados_africa$Chegadas, k = 1) 
dados_americacentral$percent <- calcula_retorno(dados_americacentral$Chegadas, k=1)
dados_americanorte$percent <- calcula_retorno(dados_americanorte$Chegadas, k = 1)
dados_asia$percent <- calcula_retorno(dados_asia$Chegadas, k = 1)
dados_europa$percent <- calcula_retorno(dados_europa$Chegadas, k = 1)
dados_oceania$percent <- calcula_retorno(dados_oceania$Chegadas, k = 1)
dados_naoespecificado$percent <- calcula_retorno(dados_naoespecificado$Chegadas, k = 1)

#Criar defasagens usando log-retorno
dados_africa$percent <- calcula_logretorno(dados_africa$Chegadas, k = 1) 
dados_americacentral$percent <- calcula_logretorno(dados_americacentral$Chegadas, k=1)
dados_americanorte$percent <- calcula_logretorno(dados_americanorte$Chegadas, k = 1)
dados_asia$percent <- calcula_logretorno(dados_asia$Chegadas, k = 1)
dados_europa$percent <- calcula_logretorno(dados_europa$Chegadas, k = 1)
dados_oceania$percent <- calcula_logretorno(dados_oceania$Chegadas, k = 1)
dados_naoespecificado$percent <- calcula_logretorno(dados_naoespecificado$Chegadas, k = 1)

acf(dados_europa$percent, na.action = na.pass)

#Salvando dados
write.csv(dados_africa, file = "africa_logret.csv")
write.csv(dados_europa, file = "europa_logret.csv")
write.csv(dados_americacentral, file = "america_central_logret.csv")
write.csv(dados_americanorte, file = "america_norte_logret.csv")
write.csv(dados_oceania, file = "oceania_logret.csv")
write.csv(dados_asia, file = "Asia.csv")

#Dados de Crescimento Populacional 
pop <- fread("API_SP.POP.GROW_DS2_en_csv_v2.csv", header = T, sep = ",")
pop_temp <- str_split_fixed(pop$`Country Name,""Country Code"`, pattern = ",", 2)
pop <- pop[,-c(1,2)]
pop <- cbind(pop_temp, pop)
pop <- data.frame(lapply(pop, function(x) gsub(pattern =  '\\"', replacement =  "", x)))
names(pop)
head(pop)
pop <- pop[,-c(2:32)]
pop <- pop[,-c(30, 31)]
names(pop)[1:29] <- c("Pais", 1989:2016)
pop <- data.frame(t(pop))
names(pop) <- pop_temp[,1]
pop <- pop[-1,]
head(pop)
names(pop)


