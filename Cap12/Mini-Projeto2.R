setwd("C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap12")
getwd()

#Instala Pacotes
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")

#Carrega Pacotes
library(dplyr)
library(data.table)
library(ggplot2)

#Carrega Dados
dados_iris <- iris
View(dados_iris)

#Tarefa 1 - Sumarizar dados com médias de cada coluna
medias_iris <- summarize(group_by(dados_iris,Species),
                         media_sepal_length = mean(Sepal.Length),
                         media_sepal_width = mean(Sepal.Width),
                         media_Petal_length = mean(Petal.Length),
                         media_Petal_width = mean(Petal.Width)
                         )

View(medias_iris)


#Tarefa 2 - Extrair valor inteiro de uma das colunas decimais
dados_iris_id <- data.table(dados_iris)
View(dados_iris_id)
dados_iris_id$Sepal.Length <- as.integer(dados_iris_id$Sepal.Length)
View(dados_iris_id)

#Tarefa 3 - Construir um Gráfico Mostrando a relação de 2 variáveis
#Numéricas com as 3 Categorias da Variável Categórica
ggplot(data = dados_iris, aes(x = Petal.Width, y = Petal.Length))+
  geom_point(aes(color = Species), size = 3)+
  ggtitle("Largura e comprimento das Pétalas")+
  labs(x = "Largura da Pétala",
       y = "Comprimento da Pétala") + 
  theme_bw()+
  theme(title = element_text(size = 15, color = "thistle3")
        )

