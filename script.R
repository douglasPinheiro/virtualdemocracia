#pre processamento
#ler base
#dividir em treino e teste
#criar o modelo (rodar o ksvm)
#analisar os resultados


#install.packages('caTools')
library(caTools)
#install.packages('e1071')
library(e1071)
#install.packages('caret')
library(caret)
#install.packages("kernlab")
library(kernlab)


#carregar a base na variÃ¡vel
base = read.csv2('pesquisa-artigo.csv')

#encode (todos os atributos categÃ³ricos)
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

classidx <- ncol(base_processed)
folds <- createFolds(base_processed[,classidx],10,FALSE)

best_accuracy <- 0;
best_div <- 0

for (i in 1:10){
  base_treino <-(base_processed[folds!=i,])
  base_teste <-(base_processed[folds==i,])
  
  
  classif_ksvm = ksvm(Questao.11 ~ ., data = base_treino, kernel = "rbfdot", cost = 9)
  
  prev_ksvm = predict(classif_ksvm, newdata = base_teste[-11])
  
  matriz_confusao = table(base_teste[ ,11], prev_ksvm)
  result_cm = confusionMatrix(matriz_confusao)
  resumo_cm = result_cm$overall
  acuracia_cm = resumo_cm['Accuracy']
  
  if(acuracia_cm > best_accuracy){
    best_accuracy = acuracia_cm;
    best_div = i;
  }
}

paste("A melhor acurÃ¡cia foi de", best_accuracy, best_div)
