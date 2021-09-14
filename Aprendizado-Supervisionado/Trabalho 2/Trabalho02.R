########################################
# Aprendizado Supervisionado - Trabalho 2
# Nome(s): Daniele Monetenegro
#          Rodrigo Dantas da Silva 
#          Thiago Bruschi Martins
########################################

library(glmnet)
library(caret)

source("support_functions.R")
source("DMwR.R")

### Input dos dados ###
trainSet <- read.csv('proteins_training_set.csv')
valSet <- read.csv('proteins_validation_set.csv')
testSet <- read.csv('proteins_test_set.csv')


trainSet$target <- as.factor(trainSet$target)
valSet$target <- as.factor(valSet$target)
testSet$target <- as.factor(testSet$target)

set.seed(42)

########## Questao 1) Inspecao dos dados ##########
summary(trainSet)
print(paste('Numero de observacoes para treino:', nrow(trainSet)))
print(paste('Numero de observacoes para validacao:', nrow(valSet)))
print(paste('Numero de observacoes para teste:', nrow(testSet)))

# Verifica se ha algum dado faltante em algum data set
print(any(is.na(trainSet)))
print(any(is.na(valSet)))
print(any(is.na(testSet)))

# Verifica se existe alguma observacao repetida entre os data sets
merge(trainSet, valSet)
merge(testSet, valSet)
merge(trainSet, testSet)

########## Questao 2) Verificando o balanceamento ##########
# As classes estao desbalanceadas, tendo quase o dobro de observacoes da classe 0
table(trainSet$target)

# Como nao ha dados faltantes nem repetidos, seguimos para a normalizacao
# O balanceamento das classes sera feito depois criarmos o baseline

########## Questão 3) Normalizacao ##########
# Pelo que vimos no summary dos dados de treino, não há variaveis categóricas
# portanto aplicaremos a Z-Norma em todas as features

mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)

# Normalizacao dos dados de validacao com a media e desvio padrao dos dados de treino
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

# Normalizacao dos dados de teste com a media e desvio padrao dos dados de treino
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)


########## Questao 4) Baseline ##########
feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]
(hypothesis <- getHypothesis(feature_names, 1)) # Formula que contem todas as features iniciais, sem alteracao de expoente

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$target

# Treinando o modelo
model <-  glmnet(x_train, y_train,  family="binomial", maxit=1e+5,
                 standardize = FALSE, alpha=0, lambda = 1e-6)

# Realizando a predicao nos dados de treino
# Recebe as previsoes em forma de probabilidade, pois os resultados da regressao
# sao passados pela funcao sigmoide (logistica) e geram um numero de 0 a 1.
# Este numero e interpretado como probabilidade. A interpretação do valor da probabilidade pode variar,
# aqui vamos comecar classificando as observacoes com uma probabilidade menor que 0.5 como sendo da classe 0
trainPred <- predict(model, newx = x_train, type="response")
(trainClassPred <- trainPred)

trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0

# Funcao que gera a confusionMatrix (absoluta)
# Onde podemos verificar a taxa de Falsos Positivos e Falsos Negativos
cm_train_baseline <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$target), 
                      positive='1')

# Funcao que recebe a cofusionMatrix(abosluta) e gera a ConfusionMatrix Relativa
# Isto e, ela recebe a matriz assim:
# TN FP
# FN TP
# Ela apresenta estes elementos relativos a taxa total de positivos ou negativos:
# TNR FPR
# FNR TPR
(cm_relative_train_baseline <- calculaMatrizConfusaoRelativa(cm_baseline_train))

# A acuracia balanceada e dada pela media entre TNR (Especificidade) e TPR (Recall)
(acc_bal_train_baseline <- (cm_relative_train_baseline[1,1] + cm_relative_train_baseline[2,2])/2)

# Validacao do Baseline
x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$target
valPred <- predict(model, newx = x_val, type="response")

# Transformando a probablidade em uma resposta categorica
valClassPred <- valPred

# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm_val_baseline <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$target), 
                      positive='1')

# ConfusionMatrix balanceada da Validcao
(cm_relative_val_baseline <- calculaMatrizConfusaoRelativa(cm_val_baseline))
(acc_val_baseline <- (cm_relative_val_baseline[1,1] + cm_relative_val_baseline[2,2])/2)
(err_val_baseline <- 1 - acc_val_baseline)

### Teste Baseline
x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

# Transformando a probablidade em uma resposta categorica
testClassPred <- testPred

# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm_test_baseline <- confusionMatrix(data = as.factor(testClassPred), 
                                   reference = as.factor(testSet$target), 
                                   positive='1')

# ConfusionMatrix balanceada da Validcao
(cm_relative_test_baseline <- calculaMatrizConfusaoRelativa(cm_test_baseline))
(acc_test_baseline <- (cm_relative_test_baseline[1,1] + cm_relative_test_baseline[2,2])/2)


########## Questão 5) Melhorando o Baseline ##########

######  Balanceamento
# usando ponderacao da funcao de erro 
# Assumingo que os FP e FN tem peso igual na vida real
# Verifica o balanceamento das classes nos dados de treino

(classes_frequency = table(trainSet$target))
(relative_classes_frequency = classes_frequency/sum(classes_frequency))

(w_positive = 1 - relative_classes_frequency[2])
(w_negative = 1 - relative_classes_frequency[1])


# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(trainSet)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[trainSet$target == 1] = w_positive 

# Associando o peso dos negatives (w_negative) aos respectivos exemplos
weights[trainSet$target == 0] = w_negative 

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$target

weighting_model <- glmnet(x_train, y_train,  family="binomial",
                                weights = weights,
                                standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$target
valPred <- predict(weighting_model, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm_weights <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$target), 
                      positive='1')

(cm_relative_weights <- calculaMatrizConfusaoRelativa(cm_weights))
## Validacao Baseline com Pesos
(acc_val_weights <- (cm_relative_weights[1,1] + cm_relative_weights[2,2])/2)
(err_val_weights <- 1 - acc_val_weights)

###### Combinando Features 
library(dplyr)
cor(trainSet %>% select(-target))
write.csv(cor(trainSet %>% select(-target)), 'cor_trainSet.csv')
# Maiores correlacoes: parker com chou_fasman e kolaskar_tongaonkar

# Combinacao de features 
f01 <- formula(target ~ .)
f02 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^2)
f03 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^3)
f04 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^4)
f05 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^5)
f06 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^6)
f07 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^7)
f08 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^8)
f09 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^9)
f10 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^10)


# Polinomial: elevando as variaveis
g01 <- getHypothesis(feature_names, degree=1)
g02 <- getHypothesis(feature_names, degree=2)
g03 <- getHypothesis(feature_names, degree=3)
g04 <- getHypothesis(feature_names, degree=4)
g05 <- getHypothesis(feature_names, degree=5)
g06 <- getHypothesis(feature_names, degree=6)
g07 <- getHypothesis(feature_names, degree=7)
g08 <- getHypothesis(feature_names, degree=8)
g09 <- getHypothesis(feature_names, degree=9)
g10 <- getHypothesis(feature_names, degree=10)


modelsComb <- c(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10)
modelsPoli <- c(g01, g02, g03, g04, g05, g06, g07, g08, g09, g10)

acc_train_comb <- c(length(modelsComb))
acc_val_comb <- c(length(modelsComb))

acc_train_poli <- c(length(modelsPoli))
acc_val_poli <- c(length(modelsPoli))
# Treino e Validacao de cada um dos modelos e formulas criadas acima
# Comecando pelos modelos com combinacao de features
i <- 1
for(f in modelsComb){
  x_train <- model.matrix(f, trainSet)
  x_val <- model.matrix(f, valSet)
  model <- glmnet(x_train, y_train,  family="binomial", weights = weights,
                     standardize = FALSE, alpha=0, lambda = 1e-6)
  
  # Verificando acc balanceada do treino
  valPred <- predict(model, newx = x_train, type="response")
  valClassPred <- valPred
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm_train_comb <- confusionMatrix(data = as.factor(valClassPred), reference = as.factor(trainSet$target), positive='1')
  cm_relative_train_comb <- calculaMatrizConfusaoRelativa(cm_train_comb)
  acc_train_comb[i] <- (cm_relative_train_comb[1,1] + cm_relative_train_comb[2,2])/2
  
  # Verificando acc balanceada da validacao
  valPred <- predict(model, newx = x_val, type="response")
  valClassPred <- valPred
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm_val_comb <- confusionMatrix(data = as.factor(valClassPred), reference = as.factor(valSet$target), positive='1')
  cm_relative_val_comb <- calculaMatrizConfusaoRelativa(cm_val_comb)
  (acc_val_comb[i] <- (cm_relative_val_comb[1,1] + cm_relative_val_comb[2,2])/2)
  
  i <- i + 1
  
}
# Verificando as saidas
acc_train_comb
acc_val_comb
(err_train_comb <- (1 - acc_train_comb))
(err_val_comb <- (1 - acc_val_comb))

# Seguindo com os modelos polinomiais
i <- 1
for(g in modelsPoli){
  x_train <- model.matrix(g, trainSet)
  x_val <- model.matrix(g, valSet)
  model <- glmnet(x_train, y_train,  family="binomial", weights = weights,
                  standardize = FALSE, alpha=0, lambda = 1e-6)
  
  # Verificando acc balanceada do treino
  valPred <- predict(model, newx = x_train, type="response")
  valClassPred <- valPred
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm_train_poli <- confusionMatrix(data = as.factor(valClassPred), reference = as.factor(trainSet$target), positive='1')
  cm_relative_train_poli <- calculaMatrizConfusaoRelativa(cm_train_poli)
  acc_train_poli[i] <- (cm_relative_train_poli[1,1] + cm_relative_train_poli[2,2])/2
  
  # Verificando acc balanceada da validacao
  valPred <- predict(model, newx = x_val, type="response")
  valClassPred <- valPred
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm_val_poli <- confusionMatrix(data = as.factor(valClassPred), reference = as.factor(valSet$target), positive='1')
  cm_relative_val_poli <- calculaMatrizConfusaoRelativa(cm_val_poli)
  (acc_val_poli[i] <- (cm_relative_val_poli[1,1] + cm_relative_val_poli[2,2])/2)
  
  i <- i + 1
}
# Verificando as saidas
(err_train_poli <- (1 - acc_train_poli))
(err_val_poli <- (1 - acc_val_poli))


############### Verificando o desempenho ate aqui
#### Analise de Desempenho e diagnostico de vies e variancia
plot(err_train_comb, xlab="Complexidade", ylab="Erro (1-Acuracia Balanceada)", 
     pch="+", col="aquamarine",  xaxt="n", 
     ylim=c(min(c(err_val_baseline, err_train_comb, err_val_comb, err_train_poli, err_val_poli)),
            max(c(err_val_baseline, err_train_comb, err_val_comb, err_train_poli, err_val_poli))))


axis(1, at=1:length(modelsComb), 
     labels=seq(from = 1, to = length(modelsComb), by = 1), las=1)
points(err_val_comb, pch="*", col="aquamarine4")
points(err_train_poli, pch="+", col="coral")
points(err_val_poli, pch="*", col="coral4")
points(rep(err_val_baseline, length(modelsComb)), pch="o", col="green")

lines(err_train_comb, col="aquamarine", lty=2)
lines(err_val_comb, col="aquamarine4", lty=2)
lines(err_train_poli, col="coral", lty=2)
lines(err_val_poli, col="coral4", lty=2)
lines(rep(err_val_baseline, length(err_train_poli)), col="green", lty=2)

legend('topright', 1, 0.6234, legend=c("Baseline","T - Combinação", "V - Combinação", "T - Polinomial", "V - Polinomial"), 
       col=c("green", "aquamarine","aquamarine3", 'coral', 'coral3'), lty=2, cex=0.7, inset = 0.1)

# Finalizando com os modelos que usam polinomios e combinacao de features

# Polinomial + Combinacao: elevando as variaveis
# Vamos pegar o ponto otimo do polinomial, e vamos combinar features no maximo ate esse grau
h01 <- formula(target ~ . + (start_position+end_position)^2+(chou_fasman+emini+kolaskar_tongaonkar)^2+(parker+isoelectric_point)^2+(aromaticity+hydrophobicity)^2 + I(stability)^2)
h02 <- formula(target ~ . + I(start_position+end_position)^2+I(chou_fasman+emini+kolaskar_tongaonkar)^2+(parker+isoelectric_point)^2+I(aromaticity+hydrophobicity+stability)^2)
h03 <- formula(target ~ . + I(start_position+end_position)^3+I(chou_fasman+emini+kolaskar_tongaonkar)^3+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^3)
h04 <- formula(target ~ . + I(start_position+end_position)^4+I(chou_fasman+emini+kolaskar_tongaonkar)^3+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^3)
h05 <- formula(target ~ . + I(start_position+end_position)^5+I(chou_fasman+emini+kolaskar_tongaonkar)^3+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^3)
h06 <- formula(target ~ . + I(start_position+end_position)^6+I(chou_fasman+emini+kolaskar_tongaonkar)^3+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^3)
h07 <- formula(target ~ . + I(start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar)^3+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^3)
h08 <- formula(target ~ . + I(start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar)^4+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^4)
h09 <- formula(target ~ . + I(start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar)^5+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^5)
h10 <- formula(target ~ . + I(start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar)^6+I(parker+isoelectric_point)^3+I(aromaticity+hydrophobicity+stability)^6)

modelsPoliComb <- c(h01, h02, h03, h04, h05, h06, h07, h08, h09, h10)
acc_train_policomb <- c(length(modelsPoliComb))
acc_val_policomb <- c(length(modelsPoliComb))

i <- 1
for(h in modelsPoliComb){
  print(h)
  x_train <- model.matrix(h, trainSet)
  x_val <- model.matrix(g, valSet)
  model <- glmnet(x_train, y_train,  family="binomial", weights = weights,
                  standardize = FALSE, alpha=0, lambda = 1e-6)
  
  # Verificando acc balanceada do treino
  valPred <- predict(model, newx = x_train, type="response")
  valClassPred <- valPred
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm_train_policomb <- confusionMatrix(data = as.factor(valClassPred), reference = as.factor(trainSet$target), positive='1')
  cm_relative_train_policomb <- calculaMatrizConfusaoRelativa(cm_train_policomb)
  acc_train_policomb[i] <- (cm_relative_train_policomb[1,1] + cm_relative_train_policomb[2,2])/2
  
  # Verificando acc balanceada da validacao
  valPred <- predict(model, newx = x_val, type="response")
  valClassPred <- valPred
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm_val_policomb <- confusionMatrix(data = as.factor(valClassPred), reference = as.factor(valSet$target), positive='1')
  cm_relative_val_policomb <- calculaMatrizConfusaoRelativa(cm_val_policomb)
  (acc_val_policomb[i] <- (cm_relative_val_policomb[1,1] + cm_relative_val_policomb[2,2])/2)
  
  i <- i + 1
}
# Verificando as saidas
acc_train_policomb
acc_val_policomb



#### Analise de Desempenho e diagnostico de vies e variancia
plot(err_train_comb, xlab="Complexidade", ylab="Erro (1-Acuracia Balanceada)", 
     pch="+", col="aquamarine",  xaxt="n", 
     ylim=c(min(c(acc_val_baseline, acc_train_comb, acc_val_comb, acc_train_poli, acc_val_poli, acc_train_policomb, acc_val_policomb)),
            max(c(acc_val_baseline, acc_train_comb, acc_val_comb, acc_train_poli, acc_val_poli, acc_train_policomb, acc_val_policomb))))


axis(1, at=1:length(modelsComb), 
     labels=seq(from = 1, to = length(modelsComb), by = 1), las=1)
points(acc_val_comb, pch="*", col="aquamarine4")

points(acc_train_poli, pch="+", col="coral")
points(acc_val_poli, pch="*", col="coral3")

#points(acc_train_policomb, pch="+", col="darkorchid1")
#points(acc_val_policomb, pch="*", col="darkorchid4")

points(rep(acc_val_baseline, length(modelsComb)), pch="o", col="green")

lines(acc_train_comb, col="aquamarine", lty=2)
lines(acc_val_comb, col="aquamarine3", lty=2)
lines(acc_train_poli, col="coral", lty=2)
lines(acc_val_poli, col="coral3", lty=2)
lines(acc_train_policomb, col="darkorchid1", lty=2)
lines(acc_val_policomb, col="darkorchid4", lty=2)

legend('topright', 1, 0.6234, legend=c("T - Combinacao", "V - Combinacao", "Baseline", "T - Polinomial", "V - Polinomial", "T - Polinomial+Combinacao", "V - Polinomial+Combinacao"), 
       col=c("green", "aquamarine","aquamarine3", 'acc_val_coral', 'coral3', 'darkorchid1', 'darkorchid4'), lty=2, cex=0.7, inset = 0.1)



######## Questao 6) Regularizacao ########

# Modelo escolhido para analise de regularizacao:
hypothesis <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^6)

############ Regularization Analysis ############
loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

lambda_values <- c(1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5)

i <- 1
for(l in lambda_values){
  
  print(l)
  # Applying hypothesis and training the model
  x_train <- model.matrix(hypothesis, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial", 
                  standardize = FALSE, maxit = 1e+05, 
                  alpha=0, lambda = l)
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to class
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  #trainClassPred
  
  lossN = getLoss(trainSet$target[trainSet$target == 0], trainPred[trainSet$target == 0])
  lossP = getLoss(trainSet$target[trainSet$target == 1], trainPred[trainSet$target == 1])
  mean_loss_train <- (lossN+lossP)/2
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  
  # Validation
  x_val <- model.matrix(hypothesis, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  ##### Let's see how well we did
  #Loss 
  lossN = getLoss(valSet$target[valSet$target == 0], valPred[valSet$target == 0])
  lossP = getLoss(valSet$target[valSet$target == 1], valPred[valSet$target == 1])
  mean_loss_val <- (lossN+lossP)/2
  
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  loss_train[i] <- mean_loss_train
  loss_val[i] <- mean_loss_val
  
  acc_train[i] <- acc_bal_train 
  acc_val[i] <-acc_bal_val 
  i <- i + 1
  
}

############# Plotting Loss ############
plot(loss_train, xlab="Regularization factor (lambda)", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val)),
            max(c(loss_train, loss_val))))


axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)

points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(5, 0.5, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

(err_train_reg <- 1 - acc_train)
(err_val_reg <- 1- acc_val)
############ Ploting Acc Balanced ############
plot(err_train_reg, xlab="Regularization factor (lambda)", ylab="Acc Balanced", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(err_train_reg, err_val_reg, err_train_not_reg, err_val_not_reg)),
            max(c(err_train_reg, err_val_reg, err_train_not_reg, err_val_not_reg))))

axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)
points(acc_val, pch="*", col="blue")
points(rep(acc_bal_val_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_bal_val_baseline, length(acc_val)), col="green", lty=2)
legend(5, 0.7, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)
