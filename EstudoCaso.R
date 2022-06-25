install.packages('Caret')
install.packages('Amelia')
install.packages('dplyr')
install.packages('reshape')
install.packages('randomForest')
install.packages('el1071')
install.packages('reshage')

library(Amelia)
library(ggplot2)
library(caret)
library(reshage)
library(randomForest)
library(dplyr)
library(e1071)


#carregando o dataset
dados_clientes <- read.csv("C:/Users/soufe/OneDrive/Documentos/PowerBI/Projeto3/6-dataset/dataset.csv")

#visualizando os dados e sua estrutura.
View(dados_clientes)
str(dados_clientes)
summary(dados_clientes)

#removendo a primeira coluna ID

dados_clientes$ID <-NULL
dim(dados_clientes)
View(dados_clientes)

#renomeadno a coluna de classe 

colnames(dados_clientes)
colnames(dados_clientes)[24] <- "Inadimplente"
colnames(dados_clientes)
View(dados_clientes)

#verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
?missmap
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)

#Renomeando colunac categoricas 

colnames(dados_clientes)
colnames(dados_clientes)[1] <- "Valor"
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado-Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

#Escolaridade

View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)
?cut
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                                   c(0,1,2,3),
                                   labels = c("Fundamental",
                                              "Medio",
                                              "Superior"))
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)



#convertendo faixa etária
View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)
dados_clientes$Idade <- cut(dados_clientes$Idade,
                            c(0,30,50,100),
                            labels = c("Jovem",
                                       "Adulto",
                                       "Idoso"))
View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
View(dados_clientes)

#Convertendo estado civil
dados_clientes$`Estado-Civil` <- cut(dados_clientes$`Estado-Civil`,
                                     c(0,1,2),
                                     labels = c("Solteiro",
                                                "Casado"))
View(dados_clientes)

# convertendo a variável que indica pagamentos para o tipo fator

dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#dataset após conversões 

str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)
missmap(dados_clientes, main = "Valores Missing Observados")
dim(dados_clientes)
View(dados_clientes)


#Alterando a cariável dependente para o tiipo fator
str(dados_clientes$Inadimplente)
colnames(dados_clientes)
dados_clientes$Inadimplente <- as.factor(dados_clientes$Inadimplente)
str(dados_clientes$Inadimplente)
View(dados_clientes)

#total de inadiplentes verus não-inadiplentes
table(dados_clientes$Inadimplente)

#vejamos as porcentagens entre classes
prop.table(table(dados_clientes$Inadimplente))

#plot da distribuição uando ggplot2
qplot(Inadimplente, data = dados_clientes, geom ="bar") +
    theme(axis.text = element_text(angle = 90, hjust = 1))

#set seed
set.seed(12345)

#amostragem estratificada
#seleciona as linhas de acordo com a variavel indiplente como strata
?createDataPartition
indice <- createDataPartition(dados_clientes$Inadimplente, p = 0.75, list =FALSE)
dim(indice)

#definimos os dados de treinameto como subconjunto do conjunto de dados original
#com numeros de indice de linha(conforme identificado acima e todas as colunas)
dados_treino <- dados_clientes[indice,]
table(dados_treino$inadimplente)

#veja porcentagem entre as classees
prop.table(table(dados_treino$inadimplente))

#numero de registros no dataset de treinamento
dim(dados_treino)






