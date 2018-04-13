rm(list=ls())
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


##Gráfico - Chegadas - Rio de Janeiro
dados_rio <- subset(dados, dados$UF == "Rio de Janeiro")
names(dados_rio)[11] <- "Ordem_mes"

i <- 1
while (i <= dim(dados_rio)[1]){
  if (nchar(dados_rio$Ordem_mes[i]) == 1){
    dados_rio$Ordem_mes[i] <- paste0(0,dados_rio$Ordem_mes[i])
  }
  i <- i + 1
}?
  
  dados_rio <- unite(dados_rio, ano_mes, ano, Ordem_mes, sep = "", remove = FALSE)
dados_rio$mes_ano <- as.Date(dados_rio$mes_ano, format = "%m-%Y")
ggplot(filter(dados_rio, ano >= 2015)) +
  geom_bar(aes(x = ano_mes, y = Chegadas), stat = "identity")

ggplot(dados_rio) +
  geom_point(aes(x = Ordem_mes, y = Chegadas))

