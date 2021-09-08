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

# Descomente a linha abaixo apenas quando o conjunto de teste esiver dispon?vel
#test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

library(dplyr)

### Questao 1) Inspecao dos dados
set.seed(42)

summary(train_set)
print(paste('Numero de observacoes para treino:', nrow(train_set)))
print(paste('Numero de observacoes para validacao:', nrow(val_set)))

###  Podemos ver que existe apenas uma feature categorica : wd, que a direcao do vento
###  Nao ha valores faltantes aparenentemente. Pois nao ha nenhuma NA. 
###  Nao conseguimos ter certeza se existem outliers ainda pois nao temos conhecimentos especificos 
### da concentracao destes elementos no ar.
###  Nao ha features sem anotacoes. Todas foram passadas no enunciado.
###  Se houvesse uma feature sem anotacao, primeiro tentariamos descobrir o que ela representa
### caso nao encontrassemo, olhariamos os valores dela para descobrir se e categorica ou numerica
### depois disso o tratamento continuaria o mesmo, entre as outras features, de acordo com o tipo dela



### Questao 2) Normalizacao: nao se aplica a feature categorica (wd)
# Vamos ignorar a feature wd pois, além dela ser categorica, a direcao
# do vento nao deve fazer diferenca na analise

# Funcao que retorna a z-norma de um data-frame inteiro
z_norma <- function(data){
    mean <- apply(data[1:ncol(data)-1], 2, mean)
    sd <- apply(data[1:ncol(data)-1], 2, sd)
    
    data <- sweep(data[1:ncol(data)-1], 2, mean, '-')
    data <- sweep(data[1:ncol(data)-1], 2, sd, '/')
    return(data)
}

# Separa o target de cada dataset e remove as colunas indesejadas
#X_train <- train_set %>% select(-No, -year, -wd, ) 
#y_train <- train_set$target

#X_val <- val_set %>% select(-No, -year, -wd, -target) 
#y_val <- val_set$target

X_train <- train_set %>% select(-No, -wd)
X_val <- val_set %>% select(-No, -wd)
# Verifica as colunas removidas
colnames(X_train)

# Z-Norm normalization

# Dados de treino
mean_features <- apply(X_train[,1:14], 2, mean)
mean_features

sd_features <- apply(X_train[,1:14], 2, sd)
sd_features

X_train[1:14] <- sweep(X_train[,1:14], 2, mean_features, "-")
X_train[1:14] <- sweep(X_train[,1:14], 2, sd_features, "/")
head(X_train)

# Dados de Validacao
X_val[1:14] <- sweep(X_val[,1:14], 2, mean_features, "-")
X_val[1:14] <- sweep(X_val[,1:14], 2, sd_features, "/")
head(X_val)

# Questão 3) Criacao do Baseline
feature_names <- colnames(X_train)[1:14]
feature_names

hypothesis <- getHypothesis(feature_names, degree=1)
hypothesis

## Baseline ##
baseline <- lm(formula=hypothesis, data=X_train)

valPred <- predict(baseline, X_val)
trainPred <- predict(baseline, X_train)

MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

# Calculo dos erros do baseline
mae_train_baseline <- MAE(trainPred, X_train$target)
mae_train_baseline

mae_val_baseline <- MAE(valPred, X_val$target)
mae_val_baseline

report <- data.frame('Baseline', round(mae_train_baseline,2), round(mae_val_baseline,2) , 0)
names(report) <- c('Modelo','MAE Treino', 'MAE Validacao', 'MAE Teste')
report


# Questão 4) Combinacao de Features
# Adicionar linha no dataframe: report[nrow(report) + 1,] = c("Teste",13,12,0)

##### Combining features #####
cor(X_train)

# Combinacao de todas as features menos MONTH, YEAR e PRESS
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


# Combinacao de todas as features menos MONTH e PRESS
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
modelsNoCategoricalG <- c(g01, g02, g03, g04, g05, g06, g07, g08, g09, g10)

total_mae_train_noCat <- c(length(modelsNoCategorical))
total_mae_val_noCat <- c(length(modelsNoCategorical))

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
write.csv(cor(X_train %>% select(-month, -day)), 'teste.csv')

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
plot(total_mae_train_noCat, xlab="Complexidade", ylab="Erro (MAE)", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline, total_mae_val_noCatG, total_mae_train_noCat)),
            max(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline, total_mae_val_noCatG, total_mae_train_noCat))))


axis(1, at=1:length(modelsNoCategorical), 
     labels=seq(from = 1, to = length(modelsNoCategorical), by = 1), las=1)
points(total_mae_val_noCat, pch="*", col="blue")
points(total_mae_train_noCatG, pch="+", col="orange")
points(total_mae_val_noCatG, pch="*", col="purple")

points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_noCatG, col="orange", lty=2)
lines(total_mae_val_noCatG, col="purple", lty=2)

lines(total_mae_train_noCat, col="red", lty=2)
lines(total_mae_val_noCat, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
total_mae_val_noCat
legend('topright', 1, 0.6234, legend=c("TREINO - Modelo Crescente", "VALIDACAO - Modelo Crescente", "Baseline", "TREINO - Modelo Combinatorio", "VALIDACAO - Modelo Combinatorio"), 
       col=c("red","blue", "green", 'orange', 'purple'), lty=2, cex=0.7, inset = 0.1)

