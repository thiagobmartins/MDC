########################################
# Teste 3 - Trabalho Final         
# Nome(s): Daniele Monetenegro
#          Rodrigo Dantas da Silva 
#          Thiago Bruschi Martins
########################################


# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta fun??o escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles n?o s?o elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que voc? deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exerc?cios 02 e 03 para ver o funcionamento 
# de uma funcao similar a essa.


getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao
train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)
test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

library(dplyr)

### Questao 1) Inspecao dos dados
set.seed(42)

summary(train_set)
print(paste('Numero de observacoes para treino:', nrow(train_set)))
print(paste('Numero de observacoes para validacao:', nrow(val_set)))

train_set$month <- as.factor(train_set$month)
train_set$day <- as.factor(train_set$day)
train_set$hour <- as.factor(train_set$hour)

val_set$month <- as.factor(val_set$month)
val_set$day <- as.factor(val_set$day)
val_set$hour <- as.factor(val_set$hour)

test_set$month <- as.factor(test_set$month)
test_set$day <- as.factor(test_set$day)
test_set$hour <- as.factor(test_set$hour)

###  Podemos ver que aparentemente a unica feature categorica seria wd, que e a direcao do vento.
###  No entanto, vamos considerar o mes, o dia e a hora como categoricas tambem. 
###  Nao ha valores faltantes aparenentemente. Pois nao ha nenhuma NA. 
###  Nao conseguimos ter certeza se existem outliers ainda pois nao temos conhecimentos especificos 
### da concentracao destes elementos no ar.
###  Nao ha features sem anotacoes. Todas foram passadas no enunciado.
###  Se houvesse uma feature sem anotacao, primeiro tentariamos descobrir o que ela representa
### caso nao encontrassemo, olhariamos os valores dela para descobrir se e categorica ou numerica
### depois disso o tratamento continuaria o mesmo, entre as outras features, de acordo com o tipo dela

### Questao 2) Normalizacao: nao se aplica as features categoricas
# Vamos retirar as coluna categoricas temporariamente dos conjuntos de treino e validacao
# apenas para podermos aplicarmos a normalizacao nas features numericas

X_train <- train_set %>% select(-wd, -month, -day, -hour)
X_val <- val_set %>% select(-wd, -month, -day, -hour)
X_test <- test_set %>% select(-wd, -month, -day, -hour)

# Verifica as colunas removidas
colnames(X_train)

# Aplicacao da Z-Norma
# Calculamos a media e desvio padrao dos dados de treino
# Os novos valores das colunas numericas sao dados por
# (valor_antigo - media)/ desvio_padrao

# Dados de treino
(mean_features <- apply(X_train[,1:12], 2, mean))
(sd_features <- apply(X_train[,1:12], 2, sd))

X_train[1:12] <- sweep(X_train[,1:12], 2, mean_features, "-")
X_train[1:12] <- sweep(X_train[,1:12], 2, sd_features, "/")
head(X_train)

# Dados de Validacao: normalizados com a media e desvio do conjunto de treino
X_val[1:12] <- sweep(X_val[,1:12], 2, mean_features, "-")
X_val[1:12] <- sweep(X_val[,1:12], 2, sd_features, "/")
head(X_val)

# Dados de Teste: normalizados com a media e desvio do conjunto de treino
X_test[1:12] <- sweep(X_test[,1:12], 2, mean_features, "-")
X_test[1:12] <- sweep(X_test[,1:12], 2, sd_features, "/")
head(X_test)

### Questão 3) Criacao do Baseline
# Para criação do baseline vamos precisar aplicar OneHotEncoding a features categorica (wd)
library(mltools)
library(data.table)

# Recuperamos as features categoricas que haviam sido removida anteriormente
X_train$wd <- train_set$wd
X_train$month <- train_set$month
X_train$day <- train_set$day
X_train$hour <- train_set$hour

X_val$wd <- val_set$wd
X_val$month <- val_set$month
X_val$day <- val_set$day
X_val$hour <- val_set$hour

X_test$wd <- test_set$wd
X_test$month <- test_set$month
X_test$day <- test_set$day
X_test$hour <- test_set$hour

# OneHotEncoding
# Criacao de uma coluna numerica, ou logica, para cada uma das categorias de cada variavel
# categorica, ou seja, para cada uma das direcoes possiveis do vento, uma pra cada dia do mês 
# e uma pra cada mês
X_train <- one_hot(as.data.table(X_train))
X_val <- one_hot(as.data.table(X_val))
X_test <- one_hot(as.data.table(X_test))

# Utilizando a funcao getHypothesis, criamos uma hipotese que inclui
# todas as features iniciais

(feature_names <- colnames(X_train)[1:12]) # features numericas
(categorical_feature_names <- colnames(X_train)[15:ncol(X_train)]) # categoricas
(hypothesis <- getHypothesis(feature_names, categorical_feature_names, degree=1))

## Baseline ##
baseline <- lm(formula=hypothesis, data=X_train)

trainPred <- predict(baseline, X_train)
valPred <- predict(baseline, X_val)
testPred <- predict(baseline, X_test)


MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

# Calculo dos erros do baseline
(mae_train_baseline <- MAE(trainPred, X_train$target))
(mae_val_baseline <- MAE(valPred, X_val$target))
(mae_test_baseline <- MAE(valPred, X_val$target))

report <- data.frame('Baseline', round(mae_train_baseline,2), round(mae_val_baseline,2) , round(mae_test_baseline,2))
names(report) <- c('Modelo','MAE Treino', 'MAE Validacao', 'MAE Teste')
report


# Questão 4) Combinacao de Features
# Adicionar linha no dataframe: 

##### Combining features #####
cor(X_train)

# Combinacao de todas as features menos MONTH, HOUR e PRESS
f01 <- formula(target ~ .)
f02 <- formula(target ~ . + (PM2.5+NO2)^2)
f03 <- formula(target ~ . + (PM2.5+NO2+O3)^3)
f04 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP)^4)
f05 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP+RAIN)^5)
f06 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP+RAIN+WSPM)^6)
f07 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP+RAIN+WSPM+TEMP)^7)
f08 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP+RAIN+WSPM+TEMP+SO2)^8)
f09 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP+RAIN+WSPM+TEMP+SO2+PM10)^9)
f10 <- formula(target ~ . + (PM2.5+NO2+O3+DEWP+RAIN+WSPM+TEMP+SO2+PM10+year)^10)


# Combinacao de todas as features menos MONTH, HOUR e PRESS
g01 <- formula(target ~ .)
g02 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^2)
g03 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^3)
g04 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^4)
g05 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^5)
g06 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^6)
g07 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^7)
g08 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^8)
g09 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^9)
g10 <- formula(target ~ . + (year+PM2.5+SO2+NO2+DEWP+TEMP+PM10+O3+RAIN+WSPM)^10)

modelsNoCategorical <- c(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10)
total_mae_train_noCat <- c(length(modelsNoCategorical))
total_mae_val_noCat <- c(length(modelsNoCategorical))

modelsNoCategoricalG <- c(g01, g02, g03, g04, g05, g06, g07, g08, g09, g10)
total_mae_train_noCatG <- c(length(modelsNoCategoricalG))
total_mae_val_noCatG <- c(length(modelsNoCategoricalG))

# Loop que vai criar um modelo para cada uma das combinacoes de variaveis
# que criamos acima
i <- 1
for(f in modelsNoCategorical){
    
    model <- lm(formula=f, data=X_train)
    
    valPred <- predict(model, X_val)
    trainPred <- predict(model, X_train)
    
    mae_train <- MAE(trainPred, X_train$target)
    total_mae_train_noCat[i] <- mae_train
    
    mae_val <- MAE(valPred, X_val$target)
    total_mae_val_noCat[i] <- mae_val
    i <- i + 1
    
}
total_mae_train_noCat

i <- 1
for(g in modelsNoCategoricalG){
    print(g)
    model <- lm(formula=g, data=X_train)
    
    valPred <- predict(model, X_val)
    trainPred <- predict(model, X_train)
    
    mae_train <- MAE(trainPred, X_train$target)
    total_mae_train_noCatG[i] <- mae_train
    
    mae_val <- MAE(valPred, X_val$target)
    total_mae_val_noCatG[i] <- mae_val
    i <- i + 1
    
}

 plot(total_mae_train_CatH, xlab="Complexidade", ylab="Erro (MAE)", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline,
                  total_mae_val_noCatG, total_mae_train_noCat, total_mae_val_CatH, 
                  total_mae_train_CatH, total_mae_val_CatJ, total_mae_train_CatJ)),
            max(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline, 
                  total_mae_val_noCatG, total_mae_train_noCat,  total_mae_val_CatH,
                  total_mae_train_CatH, total_mae_val_CatJ, total_mae_train_CatJ))))


axis(1, at=1:length(modelsNoCategorical), 
     labels=seq(from = 1, to = length(modelsNoCategorical), by = 1), las=1)
points(total_mae_val_noCat, pch="*", col="blue")
points(total_mae_train_noCatG, pch="+", col="orange")
points(total_mae_val_noCatG, pch="*", col="purple")

points(total_mae_train_CatH, pch="+", col="gray")
points(total_mae_val_CatH, pch="*", col="black")

points(total_mae_train_CatJ, pch="+", col="pink")
points(total_mae_val_CatJ, pch="*", col="brown")

points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_noCatG, col="orange", lty=2)
lines(total_mae_val_noCatG, col="purple", lty=2)
lines(total_mae_train_noCat, col="red", lty=2)
lines(total_mae_val_noCat, col="blue", lty=2)


lines(total_mae_train_CatH, col="gray", lty=2)
lines(total_mae_val_CatH, col="black", lty=2)

lines(total_mae_train_CatJ, col="pink", lty=2)
lines(total_mae_val_CatJ, col="brown", lty=2)

lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
total_mae_val_noCat
legend('top', 1, 0.6234, legend=c("TREINO - Modelo Crescente", "VALIDACAO - Modelo Crescente", "Baseline", "TREINO - Modelo Combinatorio", "VALIDACAO - Modelo Combinatorio"), 
       col=c("red","blue", "green", 'orange', 'purple', 'gray', 'black'), lty=2, cex=0.7, inset = 0.0)

## Modelo escolhido para teste: g05
model <- lm(formula=g05, data=X_train)

trainPred <- predict(model, X_train)
valPred <- predict(model, X_val)
testPred <- predict(model, X_test)


(mae_train <- MAE(trainPred, X_train$target))
(mae_val <- MAE(valPred, X_val$target))
(mae_test <- MAE(testPred, X_test$target))

report[nrow(report) + 1,] = c("Modelo Combinatorio 5",round(mae_train,2),round(mae_val,2),round(mae_test,2))

### Questão 5) Combinação Polinomial

# Combinando todas as features
h01 <- getHypothesis(feature_names, categorical_feature_names, degree=1)
h02 <- getHypothesis(feature_names, categorical_feature_names, degree=2)
h03 <- getHypothesis(feature_names, categorical_feature_names, degree=3)
h04 <- getHypothesis(feature_names, categorical_feature_names, degree=4)
h05 <- getHypothesis(feature_names, categorical_feature_names, degree=5)
h06 <- getHypothesis(feature_names, categorical_feature_names, degree=6)
h07 <- getHypothesis(feature_names, categorical_feature_names, degree=7)
h08 <- getHypothesis(feature_names, categorical_feature_names, degree=8)
h09 <- getHypothesis(feature_names, categorical_feature_names, degree=9)
h10 <- getHypothesis(feature_names, categorical_feature_names, degree=10)

modelsCategoricalH <- c(h01, h02, h03, h04, h05, h06, h07, h08, h09, h10)
total_mae_train_CatH <- c(length(modelsCategoricalH))
total_mae_val_CatH <- c(length(modelsCategoricalH))


i <- 1
for(h in modelsCategoricalH){
    print(h)
    model <- lm(formula=h, data=X_train)
    
    valPred <- predict(model, X_val)
    trainPred <- predict(model, X_train)
    
    mae_train <- MAE(trainPred, X_train$target)
    total_mae_train_CatH[i] <- mae_train
    
    mae_val <- MAE(valPred, X_val$target)
    total_mae_val_CatH[i] <- mae_val
    i <- i + 1
    
}


principais_features <- c('PM2.5', 'PM10', 'SO2', 'NO2', 'O3', 'TEMP', 'DEWP', 'RAIN', 'WSPM')

# Combinando de forma diferente
j01 <- getHypothesis(principais_features, degree=1)
j02 <- getHypothesis(principais_features, degree=2)
j03 <- getHypothesis(principais_features, degree=3)
j04 <- getHypothesis(principais_features, degree=4)
j05 <- getHypothesis(principais_features, degree=5)
j06 <- getHypothesis(principais_features, degree=6)
j07 <- getHypothesis(principais_features, degree=7)
j08 <- getHypothesis(principais_features, degree=8)
j09 <- getHypothesis(principais_features, degree=9)
j10 <- getHypothesis(principais_features, degree=10)

modelsCategoricalJ <- c(j01, j02, j03, j04, j05, j06, j07, j08, j09, j10)
total_mae_train_CatJ <- c(length(modelsCategoricalJ))
total_mae_val_CatJ <- c(length(modelsCategoricalJ))



i <- 1
for(j in modelsCategoricalJ){
    print(j)
    model <- lm(formula=j, data=X_train)
    
    valPred <- predict(model, X_val)
    trainPred <- predict(model, X_train)
    
    mae_train <- MAE(trainPred, X_train$target)
    total_mae_train_CatJ[i] <- mae_train
    
    mae_val <- MAE(valPred, X_val$target)
    total_mae_val_CatJ[i] <- mae_val
    i <- i + 1
    
}


plot(total_mae_train_CatH, xlab="Complexidade", ylab="Erro (MAE)", 
     pch="+", col="gray",  xaxt="n", 
     ylim=c(min(c( total_mae_val_CatH, total_mae_train_CatH, 
                  total_mae_val_CatJ, total_mae_train_CatJ, total_mae_val_noCatG, total_mae_train_noCatG)),
            max(c( total_mae_val_CatH, total_mae_train_CatH, 
                    total_mae_val_CatJ, total_mae_train_CatJ, total_mae_val_noCatG, total_mae_train_noCatG))))


axis(1, at=1:length(modelsCategoricalH), 
     labels=seq(from = 1, to = length(modelsCategoricalH), by = 1), las=1)
points(total_mae_val_CatH, pch="*", col="black")
points(total_mae_train_CatJ, pch="+", col="pink")
points(total_mae_val_CatJ, pch="*", col="brown")
points(total_mae_train_noCatG, pch="+", col="orange")
points(total_mae_val_noCatG, pch="*", col="purple")

points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_CatH, col="gray", lty=2)
lines(total_mae_val_CatH, col="black", lty=2)
lines(total_mae_train_CatJ, col="pink", lty=2)
lines(total_mae_val_CatJ, col="brown", lty=2)
lines(total_mae_train_noCatG, col="orange", lty=2)
lines(total_mae_val_noCatG, col="purple", lty=2)

lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
total_mae_val_noCat
legend('top', 1, 0.6234, legend=c("TREINO - Modelo Combinatorio", "VALIDACAO - Modelo Combinatorio", "Baseline", 
                                  "TREINO - Modelo Exponencial", "VALIDACAO - Modelo Exponencial",
                                  "TREINO - Features Selecionadas", "VALIDACAO Features selecionadas"), 
       col=c("orange","purple", "green", 'gray', 'black', 'pink', 'brown'), lty=2, cex=0.7, inset = 0.0)

#### Teste do melhor modelo polinomial
## Modelo escolhido para teste: h05
model <- lm(formula=h05, data=X_train)

trainPred <- predict(model, X_train)
valPred <- predict(model, X_val)
testPred <- predict(model, X_test)


(mae_train <- MAE(trainPred, X_train$target))
(mae_val <- MAE(valPred, X_val$target))
(mae_test <- MAE(testPred, X_test$target))

report[nrow(report) + 1,] = c("Modelo Polionomial 5",round(mae_train,2),round(mae_val,2),round(mae_test,2))
