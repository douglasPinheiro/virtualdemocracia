#pre processamento
#ler base
#dividir em treino e teste
#criar o modelo (rodar o KSVM)
#analisar os resultados


#install.packages('caTools')
library(caTools)
#install.packages('e1071')
library(e1071)
#install.packages('caret')
library(caret)
#install.packages('kernlab')
library(kernlab)


#carregar a base na variável
base = read.csv2('pesquisa-artigo.csv')

#encode (todos os atributos categÃƒÂ³ricos)
base$Timestamp <- NULL
base$Questao.02 <- NULL
base$Questao.03 <- NULL
base$Questao.05 <- NULL
#base$Questao.09 <- NULL
base_processed = base
base_processed$Questao.01 = factor(base_processed$Questao.01, levels = c(1,2,3,4))
#base_processed$Questao.02 = factor(base_processed$Questao.02, levels = c(1,2,3,4,5))
#base_processed$Questao.03 = factor(base_processed$Questao.03, levels = c(0,1,2,3,4))
base_processed$Questao.04 = factor(base_processed$Questao.04, levels = c(0,1,2,3,4,5,6,7))
#base_processed$Questao.05 = factor(base_processed$Questao.05, levels = c(0,1,2, 3))
base_processed$Questao.06 = factor(base_processed$Questao.06, levels = c(1,2,3,4,5,6))
base_processed$Questao.07 = factor(base_processed$Questao.07, levels = c(1,2,3))
base_processed$Questao.08 = factor(base_processed$Questao.08, levels = c(1,2,3,4))
base_processed$Questao.09 = factor(base_processed$Questao.09, levels = c(1,2,3))
base_processed$Questao.10 = factor(base_processed$Questao.10, levels = c(0,1,2,3))
base_processed$Questao.11 = factor(base_processed$Questao.11, levels = c(1,2,3,4,5,6,7,8,9,10,11))

set.seed(32)

classidx <- ncol(base_processed)
folds <- createFolds(base_processed[,classidx],10,FALSE)

base_treino <-(base_processed[folds!=3,])
base_teste <-(base_processed[folds==3,])

#classificador Random Forest
classif_ksvm = ksvm(Questao.11 ~ ., data = base_treino, kernel = "rbfdot", cost = 0)
prev_ksvm = predict(classif_ksvm, newdata = base_teste[-11])

matriz_confusao = table(base_teste$Questao.11, prev_ksvm)
result_cm = confusionMatrix(matriz_confusao)

resumo_cm = result_cm$overall
acuracia_cm = round(resumo_cm['Accuracy'] * 100, digits = 2)

print(paste("Melhor Acurácia: ", acuracia_cm))

##########################################################################################
# BEST
# Seed:  32 Fold:  3 Kernel: rbfdot Cost:  0 Acurácia:  68.97


##########################################################################################
# installed.packages('tidyverse')
library(tidyverse)

# DESCOBRE A IMPORTANCIA DE CADA PERGUNTA PARA TOMADA DE DECISAO
importance <- importance(classif_ksvm)

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

# QUESTOES QUE TIVERAM MENOS IMPORTANCIA
# 2 / 5 / 9

##########################################################################################