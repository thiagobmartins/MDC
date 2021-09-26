####### C?digo de apoio ao Trabalho 03 da disciplina INF-0615 #######


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

source("support_functions.R")

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
acc_bal_test_baseline <- avalia_teste(baseline, test_set)

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
acc_bal_test_baseline <- avalia_teste(best_depth, test_set)


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

d1 <- data.frame( n_features=features_interval, acc_bal=acc_featsel_train_7, Dados='Treino')
d2 <- data.frame( n_features=features_interval, acc_bal=acc_featsel_val_7, Dados='Validacao')
d3 <- data.frame( n_features=features_interval, acc_bal=rep(acc_val_baseline, length(features_interval)), Dados='Baseline')

df_feat_selection_7 <- rbind(d1, d2, d3)
vies_variancia_features(df_feat_selection_7, "Diagnostico de Vies e Variancia pelo numero de features")
testa_modelo(model, test_set)


# Pelos resultados mostrados, não foi possível melhorar o modelo fazendo selecao de features
# No entanto, descartando as duas features menos importantes conseguimos um resultado no
# conjunto de validacao semelhante ao modelo que utiliza todas as features
# E descartando as 5 features menos importantes conseguimos um resultado bem parecido 


########## Questao 5) Variando o numero de arvores ################

rfModel <- randomForest(formula=class ~ variance + skewness 
                        + curtosis + entropy, 
                        data= dataTrainUnder, ntree=100)

