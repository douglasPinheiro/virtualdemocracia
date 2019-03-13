#pre processamento
#ler base
#dividir em treino e teste
#criar o modelo (rodar o Random Forest)
#analisar os resultados

# install.packages("randomForest")
library(randomForest)
# install.packages('caTools')
library(caTools)
# install.packages('e1071')
library(e1071)
# install.packages('caret')
library(caret)

#carregar a base na variável
base = read.csv2('pesquisa-artigo.csv')

#encode (todos os atributos categóricos)
base$Timestamp <- NULL
base_processed = base
base_processed$Questao.01 = factor(base_processed$Questao.01, levels = c(1,2,3,4))
base_processed$Questao.02 = factor(base_processed$Questao.02, levels = c(1,2,3,4,5))
base_processed$Questao.03 = factor(base_processed$Questao.03, levels = c(0,1,2,3,4))
base_processed$Questao.04 = factor(base_processed$Questao.04, levels = c(0,1,2,3,4,5,6,7))
base_processed$Questao.05 = factor(base_processed$Questao.05, levels = c(0,1,2, 3))
base_processed$Questao.06 = factor(base_processed$Questao.06, levels = c(1,2,3,4,5,6))
base_processed$Questao.07 = factor(base_processed$Questao.07, levels = c(1,2,3))
base_processed$Questao.08 = factor(base_processed$Questao.08, levels = c(1,2,3,4))
base_processed$Questao.09 = factor(base_processed$Questao.09, levels = c(1,2,3))
base_processed$Questao.10 = factor(base_processed$Questao.10, levels = c(0,1,2,3))
base_processed$Questao.11 = factor(base_processed$Questao.11, levels = c(1,2,3,4,5,6,7,8,9,10,11))

set.seed(1)

divisao = sample.split(base_processed$Questao.11, SplitRatio=0.8)

base_treino = subset(base_processed, divisao == TRUE)
base_teste = subset(base_processed, divisao == FALSE)

#classificador Random Forest
classif_Random_Forest = randomForest(formula = Questao.11 ~ ., data = base_treino, importance = TRUE, ntree = 1000)
prev_Random_Forest = predict(classif_Random_Forest, newdata = base_teste[-11])

matriz_confusao = table(base_teste$Questao.11, prev_Random_Forest)
result_cm = confusionMatrix(matriz_confusao)
resumo_cm = result_cm$overall
acuracia_cm = resumo_cm['Accuracy']

print(paste("A melhor acuracia foi de", best_accuracy))

##########################################################################################
# installed.packages(tidyverse)
library("tidyverse")

# DESCOBRE A IMPORTANCIA DE CADA PERGUNTA PARA TOMADA DE DECISAO
importance <- importance(classif_Random_Forest)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip() + 
  theme_minimal()

##########################################################################################
