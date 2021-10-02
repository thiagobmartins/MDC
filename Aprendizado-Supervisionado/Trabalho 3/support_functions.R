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