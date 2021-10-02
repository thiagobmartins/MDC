####### C?digo de apoio ao Trabalho 03 da disciplina INF-0615 #######
#########################################
# Aprendizado Supervisionado - Trabalho 3
# Nome(s): Daniele Monetenegro
#          Rodrigo Dantas da Silva 
#          Thiago Bruschi Martins
########################################

# Leitura da base de treinamento+validacao
train_val_set <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

# Leitura da base de Teste. Descomentem as linhas abaixo quando o 
# conjunto de teste estiver dispon?vel.

test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T) # Descomentar

# As duas linhas abaixo s?o um trick para corrigir os "levels" na
# coluna country. Ele apenas adiciona 1 exemplo de treino na primeira
# linha do teste e depois retira-o para obter o test_set original. 
# Nao se preocupem, eh apenas para nivelamento interno do R. 
# Certifiquem-se de executar os comandos na seguinte ordem:
# linha 38, linha 47 e linha 48 quando a base de teste estiver disponivel

temporary_test <- rbind(train_val_set[1,], test_set) # Descomentar
test_set <- temporary_test[-1,] # Descomentar


####### ======= O TRABALHO COME?A A PARTIR DAQUI ======= #######
library(caret)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)

############## =========== Funcoes ============== ############
# Tivemos que colocar todas as funcoes aqui pois o moodle nao
# aceita mais que dois arquivos para fazermos upload
# Escreve a funcao de hipotese dada as features continuas e o 
# respectivo grau polinomial
getHypothesis <- function(feature_names, degree=1){
  
  hypothesis_string <- "hypothesis <- formula(label ~ "
  for(i in 1:length(feature_names)){
    if (i >= 2)
      hypothesis_string <- paste(hypothesis_string, feature_names[i], sep = "+")
    else
      hypothesis_string <- paste(hypothesis_string, feature_names[i])
  }
  
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}

# Funcao que treina uma arvore de desicao
# Gera pesos automaticamente para o balaceamento da arvore
treina_arvore_weights <- function(data, maxDepth=30){
  pesos = gera_pesos(data)
  tree <- rpart(formula=label ~ ., data=data, method="class", weights=pesos,
                control=rpart.control(minsplit=2, cp=0.0, xval = 10, maxdepth=maxDepth),
                parms= list(split="information"))
  
  return(tree)
}


# Funcao que gera os pesos para as classes de um determinado dataset
# Os pesos gerados sao inversamente proporcionais a frequencia de cada classe
gera_pesos <- function(data)
{
  (classes_frequency <- table(data$label))
  (classes_prop <- classes_frequency / nrow(data))
  
  pesos <- rep(0, times=nrow(data))
  pesos[data$label == 'dead'] <- 1-classes_prop['dead']
  pesos[data$label == 'recovered'] <- 1-classes_prop['recovered']
  pesos[data$label == 'onTreatment'] <- 1-classes_prop['onTreatment']
  
  return(pesos)
}

# Funcao que calcula a acuracia balanceada de um 
# determinado modelo em um determinado dataset
acc_bal <- function(treeModel, data)
{
  pred <- predict(treeModel, data, type="class")
  cm <- confusionMatrix(data = as.factor(pred), 
                        reference = as.factor(data$label), 
                        positive='yes')
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  
  return((cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3)
}

# Funcao que gera a matriz de confusao relativa nos conjuntos de treino
# e de validacao para um determinado modelo
matriz_confusao_relativa <- function(treeModel, data_train, data_val)
{
  # Avaliando no conjunto de treino
  train_pred <- predict(treeModel, data_train, type="class")
  cm_train <- confusionMatrix(data = as.factor(train_pred), 
                              reference = as.factor(data_train$label), 
                              positive='yes')
  
  cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
  acc_bal_train <- round((cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3, 3)
  
  print(paste('Acuracia Balanceada no Treino', acc_bal_train))
  cat('\n')
  print('Matriz de Confusao Relativa do Treino')
  print(cm_relative_train)
  cat('\n')
  
  # Avaliando no conjunto de validacao
  val_pred <- predict(treeModel, data_val, type="class")
  cm_val <- confusionMatrix(data = as.factor(val_pred), 
                            reference = as.factor(data_val$label), 
                            positive='yes')
  
  cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
  acc_bal_val <- round((cm_relative_val[1,1] + cm_relative_val[2,2] + cm_relative_val[3,3])/3, 3)
  
  
  print(paste('Acuracia Balanceada na Validacao', acc_bal_val))
  cat('\n')
  print('Matriz de Confusao Relativa da Validacao')
  cat('\n')
  print(cm_relative_val)
  
  return(acc_bal_val)
}

# Funcao que gera a matriz de confusao relativa e  acuracia 
# balanceada no conjunto de teste
testa_modelo <- function(treeModel, data_test)
{
  
  # Avaliando no conjunto de treino
  train_pred <- predict(treeModel, data_test, type="class")
  cm_train <- confusionMatrix(data = as.factor(train_pred), 
                              reference = as.factor(data_test$label), 
                              positive='yes')
  
  cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
  acc_bal_train <- round((cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3, 3)
  
  print(paste('Acuracia Balanceada no Teste', acc_bal_train))
  cat('\n')
  print('Matriz de Confusao Relativa do Teste')
  print(cm_relative_train)
  cat('\n')
  
}

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposi??o para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
  cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
  
  return(cm_relative)  
}

# Funcao que gera o grafico de vies e variancia para variacao de profundidade
vies_variancia_depth <- function(data, nome){
  g <- ggplot(data=data, aes(x=depth, y=acc_bal, colour=Dados)) +
    geom_line() +
    geom_point() +
    ggtitle(nome) +
    xlab("Profundidade") +
    scale_x_continuous(limits=c(0,30),breaks=seq(2,30,4)) +
    scale_y_continuous(limits=c(0.76,1.0), breaks=seq(0.75,1.0,0.02)) +
    theme(legend.position = c(0.8, 0.2)) +
    scale_color_manual(values=c("chartreuse1", "blue", "red")) +
    ylab("Acuracia Balanceada")
  
  return(g)
}

# Funcao que gera o grafico de vies e variancia para variacao de features
vies_variancia_features <- function(data, nome){
  g <- ggplot(data=data, aes(x=n_features, y=acc_bal, colour=Dados)) +
    geom_line() +
    geom_point() +
    ggtitle(nome) +
    xlab("Numero de Features") +
    scale_x_continuous(limits=c(2,13),breaks=seq(2,13,2)) +
    scale_y_continuous(limits=c(0.79,0.92), breaks=seq(0.79,0.92,0.02)) +
    theme(legend.position = c(0.8, 0.2)) +
    scale_color_manual(values=c("chartreuse1", "blue", "red")) +
    ylab("Acuracia Balanceada")
  return(g)
}

# Funcao que gera o grafico de vies e variancia para variacao de features
vies_variancia_ntrees <- function(data, nome){
  x_min <- min(data[1])
  x_max <- max(data[1])
  y_min <- min(data[2])
  y_max <- max(data[2])
  g <- ggplot(data=data, aes(x=n_trees, y=acc_bal, colour=Dados)) +
    geom_line() +
    geom_point() +
    ggtitle(nome) +
    xlab("Numero de Arvores") +
    scale_x_continuous(limits=c(x_min,x_max),breaks=seq(x_min,x_max,2)) +
    scale_y_continuous(limits=c(y_min,y_max), breaks=seq(y_min,y_max,0.02)) +
    theme(legend.position = c(0.8, 0.2)) +
    scale_color_manual(values=c("chartreuse1", "blue", "red")) +
    ylab("Acuracia Balanceada")
  return(g)
}

varia_depth_w <- function(data, depths, feature_names, acc_val_baseline){
  acc_depths_train <- c()
  acc_depths_val <- c()
  
  pesos <- gera_pesos(data)
  
  for (depth in depths){
    model <- rpart(formula=label ~ ., data=data, method="class", weights = pesos,
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10, maxdepth = depth),
                   parms= list(split="information"))
    
    acc_depths_train <- append(acc_depths_train, acc_bal(model, data))
    acc_depths_val <- append(acc_depths_val, acc_bal(model, dataVal))
  }
  
  
  d1 <- data.frame( depth=depths, acc_bal=acc_depths_train, Dados='Treino')
  d2 <- data.frame( depth=depths, acc_bal=acc_depths_val, Dados='Validacao')
  d3 <- data.frame( depth=depths, acc_bal=rep(acc_val_baseline, length(depth)), Dados='Baseline')
  
  return(rbind(d1,d2,d3))
}


vies_variancia_rf <- function(data, nome){
  g <- ggplot(data=data, aes(x=n_trees, y=acc_bal, colour=Dados)) +
    geom_line() +
    geom_point() +
    ggtitle(nome) +
    xlab("Numero de Arvores") +
    scale_x_continuous(limits=c(2,13),breaks=seq(2,13,2)) +
    scale_y_continuous(limits=c(0.70,1.00), breaks=seq(0.70,1.00,0.02)) +
    theme(legend.position = c(0.8, 0.2)) +
    scale_color_manual(values=c("chartreuse1", "blue", "red")) +
    ylab("Acuracia Balanceada")
  return(g)
}

############ ============= Fim das funcoes ======== #########

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

#install.packages("ggpubr")
#library(ggpubr)
library("cowplot")

# Configurem o valor da semente.
set.seed(42) # Coloquem o valor que desejarem.

########## Questao 1) Inspecao dos dados ##########
table(train_val_set$label)

# O dataset esta desbalanceado
# Vamos utilizar um mix de oversampling com weights

######## Balanceamento e divisao em treino e validacao

# Remove os elementos repetidos antes da divisao Treino/Validacao/Test
data_unique <- unique(train_val_set)

# Treino-Validacao 80% / Teste 20%
randomTrainIndexes <- sample(1:nrow(data_unique), size=0.8*nrow(data_unique))
dataTrain <- data_unique[randomTrainIndexes, ]
dataVal  <- data_unique[-randomTrainIndexes, ] 

merge(dataTrain, dataVal)

dim(dataTrain)
dim(dataVal)

######## Testando diferentes Balanceamentos ###########
# Comecaremos comum modelo sem balanceamento e depois testaremos
# 3 tipos de balanceamantos de repeticao de amostras, junto com
# pesos diferentes para cada classe

table(dataTrain$label)
onTreatment_train <- dataTrain[dataTrain$label == 'onTreatment', ]
dead_train <- dataTrain[dataTrain$label == 'dead', ]
recovered_train <- dataTrain[dataTrain$label == 'recovered', ]

sem_bal <- rpart(formula=label ~ ., data=dataTrain, method="class",
                 control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                 parms= list(split="information"))
sem_bal_acc_val <- matriz_confusao_relativa(sem_bal, dataTrain, dataVal)

(results_balanciamento <- data.frame( modelo = c('Sem Balanceamento'), acc_bal = c(sem_bal_acc_val)))

### Testando Undersampling na classe OnTreatment
# Undersampling + weights
randomOnTreatmentIdx <- sample(1:nrow(onTreatment_train), size=1.5*nrow(recovered_train))
under_treatment_train <- onTreatment_train[randomOnTreatmentIdx,]
dataTrainUnder <- rbind(dead_train, under_treatment_train, recovered_train)
table(dataTrainUnder$label)

feature_names <- colnames(dataTrainUnder[1:(ncol(dataTrainUnder)-1)])

source("support_functions.R")
model_under <- treina_arvore_weights(data = dataTrainUnder)
print(' ------- Undersampling --------')
under_acc_val <- matriz_confusao_relativa(model_under, dataTrainUnder, dataVal)
(results_balanciamento <- rbind(results_balanciamento, c('Undersampling', under_acc_val)))

### Testando Oversampling
# Oversampling + weights
randomRecoveredIdx <- sample(1:nrow(recovered_train), size=1.5*nrow(recovered_train), replace=TRUE)
over_recovered_train <- recovered_train[randomRecoveredIdx,]
dataTrainOver <- rbind(dead_train, onTreatment_train, over_recovered_train)
table(dataTrainOver$label)

# Gerando pesos e treinando modelo
model_over <- treina_arvore_weights(data = dataTrainOver)
print(' ------- Oversampling --------')
over_acc_val <- matriz_confusao_relativa(model_over, dataTrainOver, dataVal)
(results_balanciamento <- rbind(results_balanciamento, c('Oversampling', over_acc_val)))


### Mesclando os dois
# Aplicando Undersampling na classe OnTreatment, para o dataset 
# que ja estava com oversampling na classe recovered
randomRecoveredIdx <- sample(1:nrow(recovered_train), size=1.2*nrow(recovered_train), replace=TRUE)
over_recovered_train <- recovered_train[randomRecoveredIdx,]
randomTreatmentIdx <- sample(1:nrow(onTreatment_train), size=0.7*nrow(onTreatment_train), replace=TRUE)
under_treatment_train <- onTreatment_train[randomTreatmentIdx,]

dataTrainOverUnder <- rbind(dead_train, under_treatment_train, over_recovered_train)
table(dataTrainOverUnder$label)

## Usando pesos
model_overunder <- treina_arvore_weights(data = dataTrainOverUnder)
print(' ------- Undersampling + Oversampling --------')
overunder_acc_val <- matriz_confusao_relativa(model_overunder, dataTrainOverUnder, dataVal)
(results_balanciamento <- rbind(results_balanciamento, c('Over + Undersampling', overunder_acc_val)))

## Resultados
results_balanciamento [order(results_balanciamento$acc_bal, decreasing = TRUE),]
######## QUESTAO 2) Testando o baseline ################
# Pelo resultado da tabela results_balanceamento vemos que ouve um empate
# entre o modelo sem balanceamento e com Oversampling
# Vamos escolher como baseline nosso modelo sem balanceamento, apenas para fins
# de comparacao, pois os modelos futuros provavelmente terao balanceamento
table(dataVal$label)
baseline <- model_under
matriz_confusao_relativa(model_under, dataTrainUnder, dataVal)
acc_val_baseline <- acc_bal(model_under, dataVal)

### Avaliando no Conjunto de teste ####
acc_bal_test_baseline <- testa_modelo(baseline, test_set)

######## QUESTAO 3) Aumentando a profundidade ################
# Para fins de comparação vamos testar todas as profundidades possiveis
# para dois modelos diferentes: Sem balanceamento e com Oversampling + weights
# Pois foram os dois melhores resultados anteriores

depths <- 2:30
#### Varando a profundidade para o modelo com Oversampling + weights


results_depth_under_w <- varia_depth_w(dataTrainUnder, depths)


# Gerando um grafico em conjunto com os dois plots
vies_variancia_depth(results_depth_under_w, "Diagnostico de Vies e Variancia")

## Ponto otimo 
best_depth <- treina_arvore_weights(dataTrainUnder, 7)
acc_bal_test_baseline <- testa_modelo(best_depth, test_set)


#### QUESTAO 4) Selecao de features


######## Testando no melhor modelo ate agora (MaxDepth = 28 + Undersampling + Weights)
best_depth <- 7
best_tree <- rpart(formula=getHypothesis(feature_names), data=dataTrainUnder, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10, maxdepth = best_depth),
                   parms= list(split="information"))


relative_importance <- best_tree$variable.importance/sum(best_tree$variable.importance)
(df_rel_importance <- data.frame(relative_importance))


####### Variando as Features com depth = 7 ###########
acc_featsel_train_7 <- c()
acc_featsel_val_7 <- c()
features_imp <- rownames(df_rel_importance)
df_feat_selection_7 <- data.frame(n_features = c(), acc_val = c(), Dados = c())
features_interval <- 2:13
weights_under <- gera_pesos(dataTrainUnder)

for (f in features_interval){
    model <- rpart(formula=getHypothesis(features_imp[1:10]),
                  data=dataTrainUnder, method="class", weights = weights_under,
                  control=rpart.control(minsplit=2, cp=0.0, xval = 10, maxdepth=7),
                  parms= list(split="information"))
    
    acc_featsel_train_7 <- append(acc_featsel_train_7, acc_bal(model, dataTrainUnder))
    acc_featsel_val_7 <- append(acc_featsel_val_7, acc_bal(model, dataVal))
}

d1 <- data.frame( n_trees=features_interval, acc_bal=acc_featsel_train_7, Dados='Treino')
d2 <- data.frame( n_trees=features_interval, acc_bal=acc_featsel_val_7, Dados='Validacao')
d3 <- data.frame( n_trees=features_interval, acc_bal=rep(acc_val_baseline, length(features_interval)), Dados='Baseline')

df_feat_selection_7 <- rbind(d1, d2, d3)
library("support_functions.R")
vies_variancia_features(df_feat_selection_7, "Diagnostico de Vies e Variancia pelo numero de features")
testa_modelo(model, test_set)


# Pelos resultados mostrados, não foi possível melhorar o modelo fazendo selecao de features
# No entanto, descartando as duas features menos importantes conseguimos um resultado no
# conjunto de validacao semelhante ao modelo que utiliza todas as features
# E descartando as 5 features menos importantes conseguimos um resultado bem parecido 


########## Questao 5) Variando o numero de arvores ################
library(randomForest)


ntrees_interval <- seq(1,40, 2)
acc_rf_train <- c()
acc_rf_val <- c()
for (n_tree in ntrees_interval) {
  
  rfModel <- randomForest(formula=label ~ ., data= dataTrainUnder,
                          ntree=n_tree, mtry=((dim(dataTrainUnder)[2]-1) * 0.3))
  
  acc_rf_train <- append(acc_rf_train, round(acc_bal(rfModel, dataTrainUnder), 2))
  acc_rf_val <- append(acc_rf_val, round(acc_bal(rfModel, dataVal), 2))
  
}

d1 <- data.frame( n_trees=ntrees_interval, acc_bal=acc_rf_train, Dados='Treino')
d2 <- data.frame( n_trees=ntrees_interval, acc_bal=acc_rf_val, Dados='Validacao')
d3 <- data.frame( n_trees=ntrees_interval, acc_bal=rep(acc_val_baseline, length(ntrees_interval)), Dados='Baseline')
df_rf <- rbind(d1, d2, d3)
vies_variancia_ntrees(df_rf, "Diagnostico de Vies e Variancia pelo numero de Arvores")

best_rf <- randomForest(formula=label ~ ., data= dataTrainUnder,
                        ntree=7, mtry=((dim(dataTrainUnder)[2]-1) * 0.3))

testa_modelo(best_rf, test_set)
