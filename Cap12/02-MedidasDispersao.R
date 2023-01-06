# Parte 2 Medidas de Dispersão

# Mudando local de trabalho
setwd("C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap12")
getwd()

# Carregando Dataset
vendas <- read.csv("vendas.csv", fileEncoding = "windows-1252")

# Variância
var(vendas$Valor)

# Desvio Padrão
sd(vendas$Valor)