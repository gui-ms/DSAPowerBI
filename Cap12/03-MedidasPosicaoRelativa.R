

# Mudando local de trabalho
setwd("C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap12")
getwd()

# Carregando Dataset
vendas <- read.csv("vendas.csv", fileEncoding = "windows-1252")

# Resumo dos Dados
head(vendas)
tail(vendas)
View(vendas)
summary(vendas)
summary(vendas$Valor)
summary(vendas[c("Valor", "Custo")])

# Variáveis Numéricas

mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.99))
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.2))
IQR(vendas$Valor) # Diferença entre Q3 e Q1
range(vendas$Valor)
diff(range(vendas$Valor))
