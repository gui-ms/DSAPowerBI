#Mini-Projeto 4: Prevendo a Inadimplência de Clientes com Machine 
#Learning e Power BI

setwd('C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap15')
getwd()

#Instalando pacotes necessários:
install.packages('Amelia')
install.packages('caret')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('reshape2')
install.packages('randomForest')
install.packages('e1071')
install.packages('listenv')

#Carregando os pacotes:
library('Amelia')
library('caret')
library('ggplot2')
library('reshape2')
library('randomForest')
library('dplyr')
library('e1071')

#Carregando Dataset:
dados_clientes <- read.csv('dados/dataset.csv')

#Visualização dos dados:
summary(dados_clientes)
View(dados_clientes)
str(dados_clientes)

########################Análise Exploratória, Limpeza e Transformação####
#Removendo a primeira coluna(ID):
dados_clientes$ID <- NULL

#Verificando nomes das colunas e renomeando a última:
colnames(dados_clientes)
colnames(dados_clientes)[24] <- 'Inadimplente'
colnames(dados_clientes)
View(dados_clientes)

#Verificando e removendo valores nulos do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = 'Valores Missing Observados')
dados_clientes <- na.omit(dados_clientes)

#Renomeando Colunas:
colnames(dados_clientes)
colnames(dados_clientes)[2] <- 'Sexo'
colnames(dados_clientes)[3] <- 'Escolaridade'
colnames(dados_clientes)[4] <- 'Estado_Civil'
colnames(dados_clientes)[5] <- 'Idade'
View(dados_clientes)

#Alterando Genero para variável categórica:
View(dados_clientes$Sexo)
str(dados_clientes$Sexo)
?cut
dados_clientes$Sexo <- cut(dados_clientes$Sexo,c(0,1,2),
                           labels = c('Masculino','Feminino'))

View(dados_clientes$Sexo)
str(dados_clientes$Sexo)
summary(dados_clientes$Sexo)

#Alterando as outras variáveis para categóricas:
#Escolaridade
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,c(0,1,2,3,4),
                           labels = c('Pos_Graduado','Graduado','Ensino_Medio','Outros'))

View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

#Estado Civil
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,c(-1,0,1,2,3),
                           labels = c('Desconhecido','Casado','Solteiro','Outros'))

View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

#Covertendo idade para faixa etária
View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)
dados_clientes$Idade <- cut(dados_clientes$Idade,c(0,30,50,100),
                            labels = c('Jovem','Adulto','Idoso'))
View(dados_clientes)
summary(dados_clientes$Idade)

#convertendo a Variável de pagamento para o tipo fator (fator = categórico)
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#Verificando o dataset após transformações:
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = 'Valores Missing Observados')
dados_clientes <- na.omit(dados_clientes)
dim(dados_clientes)

#Alterando coluna 'Inadimplente' para o tipo fator
dados_clientes$Inadimplente <- as.factor(dados_clientes$Inadimplente)
str(dados_clientes$Inadimplente)
summary(dados_clientes$Inadimplente)
View(dados_clientes$Inadimplente)

#Visualizando proporção de inadimplentes
table(dados_clientes$Inadimplente)
prop.table(table(dados_clientes$Inadimplente))
qplot(Inadimplente, data = dados_clientes, geom = 'bar')+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

#Separando dataset entre treino e test
set.seed(12345)
indice <- createDataPartition(dados_clientes$Inadimplente, p=0.75,list = FALSE)
dim(indice)
dados_treino <- dados_clientes[indice,]
table(dados_treino$Inadimplente)
View(dados_treino)

#Comparando proporções entre dataset original e de treino
compara_dados <- cbind(prop.table(table(dados_clientes$Inadimplente)),
                       prop.table(table(dados_treino$Inadimplente)))
colnames(compara_dados) <- c('Original','Treinamento')
compara_dados
melt_compara_dados <- melt(compara_dados)
melt_compara_dados
ggplot(melt_compara_dados, aes(x = Var1, y=value))+
  geom_bar(aes(fill=Var2), stat = 'Identity', position = 'dodge')+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Passa o que restou para o dataset de teste
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)
 
################## Machine Learning #############################
#Primeira versão do Modelo: Random Forest
?randomForest
modeloV1 <- randomForest(Inadimplente ~.,data = dados_treino)
modeloV1

#Avaliação do modelo:
plot(modeloV1)

previsoesV1 <- predict(modeloV1,dados_teste)
str(previsoesV1)
#Matriz de Confusão
cmV1 <- confusionMatrix(previsoesV1,dados_teste$Inadimplente,positive = '1')
cmV1


#Outras métricas (F1, recall, precisão):
y <- dados_teste$Inadimplente
y_predV1 <- previsoesV1

precision <- posPredValue(y_predV1, y)
precision

recall <- sensitivity(y_predV1,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1

#Balanceamento da classe menor por oversampling:
install.packages( "C:/Users/guilherme.soares/Desktop/PowerBi-Curso/Cap15/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
install.packages(c("zoo","xts","quantmod",'abind','ROCR'))
library(DMwR)
table(dados_treino$Inadimplente)
set.seed(9560)
dados_treino_bal <- SMOTE(Inadimplente ~., data = dados_treino)
table(dados_treino_bal$Inadimplente)

#Aplicando ao modelo o dataset oversampled:
modeloV2 <- randomForest(Inadimplente ~ .,data = dados_treino_bal)
modeloV2

#Avaliação do modelo:
plot(modeloV2)

previsoesV2 <- predict(modeloV2,dados_teste)
str(previsoesV2)
#Matriz de Confusão
cmV2 <- confusionMatrix(previsoesV2,dados_teste$Inadimplente,positive = '1')
cmV2

#Outras métricas (F1, recall, precisão):
y <- dados_teste$Inadimplente
y_predV2 <- previsoesV2

precision <- posPredValue(y_predV2, y)
precision

recall <- sensitivity(y_predV2,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1

#verificando as variáveis mais importantes:
View(dados_treino_bal)
varImpPlot(modeloV2)

#Obtendo as variáveis mais importantes:
imp_var <- importance(modeloV2)
varImportance <- data.frame(variables = row.names(imp_var),
                            Importance = round(imp_var[,'MeanDecreaseGini'],2))
varImportance

#Criando o rank de variáveis baseado na importância:
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))
rankImportance

#Colocando no gráfico com ggplot:
ggplot(rankImportance,
       aes(x=reorder(variables,Importance),
           y=Importance,
           fill = Importance))+
  geom_bar(stat = 'Identity') +
  geom_text(aes(x=variables, y=0.5, label = Rank),
            hjust=0,
            vjust=0.55,
            size=4,
            colour = 'red')+
  labs(x='Variables')+
  coord_flip()

#terceira versão do modelo, removendo colunas menos relevantes:
modeloV3 <- randomForest(Inadimplente ~ PAY_0 + PAY_2 + PAY_3 + PAY_AMT1 + PAY_5 + PAY_AMT2 + BILL_AMT1,
                         data = dados_treino_bal)
modeloV3

#Avaliação do modelo:
plot(modeloV3)

previsoesV3 <- predict(modeloV3,dados_teste)
str(previsoesV3)
#Matriz de Confusão
cmV3 <- confusionMatrix(previsoesV3,dados_teste$Inadimplente,positive = '1')
cmV3

#Outras métricas (F1, recall, precisão):
y <- dados_teste$Inadimplente
y_predV3 <- previsoesV3

precision <- posPredValue(y_predV3, y)
precision

recall <- sensitivity(y_predV3,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1

#Salvando Modelo 3:
saveRDS(modeloV3, file = 'modelo/modeloV3.rds')

modeloFinal <- readRDS('modelo/modeloV3.rds')

#Exemplo de utilização do modelo para previsões de 3 clientes:
PAY_0 <- c(0,0,0)
PAY_2 <- c(0,0,0)
PAY_3 <- c(1,0,0)
PAY_AMT1 <- c(1100,1000,1200)
PAY_5 <- c(0,0,0)
PAY_AMT2 <- c(1500,1300,1150)
BILL_AMT1 <- c(350,420,280)

novos_clientes <- data.frame(PAY_0 , PAY_2 , PAY_3 , PAY_AMT1, PAY_5, PAY_AMT2, BILL_AMT1)
View(novos_clientes)

#Convertendo colunas para categóricas:
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_5))

str(novos_clientes)
previsoes_novos_clientes <- predict(modeloFinal,novos_clientes)
View(previsoes_novos_clientes)
str(previsoes_novos_clientes)
View(cmV1)
