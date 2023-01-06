setwd("C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap12")
getwd()

dados <- read.table("usuarios.csv",dec = ".", sep = ",",
                    h = T, #Header = true
                    fileEncoding = "windows-1252")

#Visualização dos dados
View(dados)
summary(dados)
names(dados)
str(dados)

#Tabela Frequências Absolutas
freq <- table(dados$grau_instrucao)
View(freq)

#Frequências Relativas
freq_rel <- prop.table(freq)
View(freq_rel)

#Porcentagem das Frequências Relativas
percent_freq_rel <- 100*prop.table(freq_rel)
View(percent_freq_rel)

freq <- c(freq, sum(freq))
names(freq)[4] <- "Total"
View(freq)

#Tabela Final
freq_rel <- c(freq_rel, sum(freq_rel))
percent_freq_rel <- c(percent_freq_rel,sum(percent_freq_rel))

tabela_final <- cbind(freq,
                      freq_rel = round(freq_rel,
                                       digits = 2),
                      percent_freq_rel = round(percent_freq_rel,
                                       digits = 2)
                      )
View(tabela_final)
