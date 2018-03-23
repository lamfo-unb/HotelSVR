## Function to read data
func <- function(x){
  d <- read.csv("1data/chegadas_1989.csv", header = T, sep = ";")
  colnames(d) <-  c("Continente", "Ordem continente", "País", "Ordem país", "UF", "Ordem UF", "Via de acesso", "Ordem via de acesso", "ano", "Mês", "Ordem mês", "Chegadas")
  return(d)
}

## Merge files into one csv
dados <- list.files(path = "1data", full.names = TRUE) %>%
  lapply(func) %>%
  bind_rows

dados <- na.omit(dados)

write.csv(dados, file = "chegada_turistas_compilado.csv")
