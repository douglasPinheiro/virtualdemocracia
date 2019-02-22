#pre processamento
#ler base
#dividir em treino e teste
#criar o modelo (rodar o svm)
#analisar os resultados


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

kernels = c("linear", "radial", "polynomial", "sigmoid")
best_accuracy = 0
best_kernel = ""
best_cost = 0

for(k in 1: 4) {
  for (i in 1:10){
    #classificador svm
    classif_svm = svm(formula = Questao.11 ~ ., data = base_treino, type = 'C-classification', kernel = kernels[k], cost = i)
    prev_svm = predict(classif_svm, newdata = base_teste[-11])

    matriz_confusao = table(base_teste[ ,11], prev_svm)
    result_cm = confusionMatrix(matriz_confusao)
    resumo_cm = result_cm$overall
    acuracia_cm = resumo_cm['Accuracy']
    
    if(acuracia_cm > best_accuracy){
      best_accuracy = acuracia_cm;
      best_kernel = kernels[k]
      best_cost = i
    }
  }
}

paste("A melhor acurácia foi de", best_accuracy, " com o kernel", best_kernel, "e com o custo", best_cost)