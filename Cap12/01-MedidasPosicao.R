setwd("C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap12")
getwd()

vendas <- read.csv("vendas.csv", fileEncoding = "windows-1252")

# Resumo do dataset

View(vendas)
str(vendas)
summary(vendas$Valor)
summary(vendas$Custo)

#Média
?mean
mean(vendas$Valor)
summary(vendas$Valor)
summary(vendas$Custo)
mean(vendas$Custo)

# Média Ponderada
?weighted.mean
weighted.mean(vendas$Valor,vendas$Custo)

#Mediana
?median
median(vendas$Valor)
unique(vendas$Valor)


#Criando Função Moda
moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

#Obtenção da Moda
resultado <- moda(vendas$Valor)
print(resultado)

# Criando Gráfico de Média de Valor por Estado
#Instalando Pacote
install.packages("ggplot2")
library(ggplot2)

#Gráfico
ggplot(vendas) + 
  stat_summary(aes(x=Estado, y=Valor),
               fun=mean,
               geom = "bar",
               fill = "lightgreen",
               col = "grey50") +
  labs(title = "Média de Valor por Estado")

