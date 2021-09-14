################################################
# MDC010 - Aprendizado Supervisionado 01       #
# Codigo de suporte aos exerc?cios 03, 04 e 05 #
################################################

# Funcao para dividir em treino, valida??o e teste
splitTrainValTest <- function(data){
    randomTrainValIndexes <- sample(1:nrow(data), size=0.8*nrow(data))
    trainValSet <- data[randomTrainValIndexes,]
    testSet <- data[-randomTrainValIndexes,]
    
    randomTrainIndexes <- sample(1:nrow(trainValSet), size=0.8*nrow(trainValSet))
    trainSet <- trainValSet[randomTrainIndexes,]
    valSet <- trainValSet[-randomTrainIndexes,]
    
    print(dim(trainSet))
    print(dim(valSet))
    print(dim(testSet))
    
    return(list(trainSet, valSet, testSet))
}

# Calcula a loss de um modelo dado os valores preditos e os labels
getLoss <- function(y_true, y_pred){
    y_true <- as.numeric(y_true) - 1
    
    totalLoss <- 0
    eps <- 1e-9
    # Recall: length(y_true) == length(y_pred)
    # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
    # eps is used for numerical stability, it is very close to 0.
    # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
    # would be 0*log2(0) + 1*log(1). It would result in NaN
    # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
    for(i in 1:length(y_true)){
        loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
        totalLoss <- totalLoss + loss
    }
    totalLoss <- totalLoss/(length(y_true))
    return(totalLoss)
}


# Escreve a funcao de hipotese dada as features continuas e o 
# respectivo grau polinomial
getHypothesis <- function(feature_names, degree){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

# Calcula a matriz de confus?o relativa 
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi??o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
    cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
    cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
    
    return(cm_relative)  
}