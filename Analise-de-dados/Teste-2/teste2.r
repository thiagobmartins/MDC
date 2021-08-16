########################################
# Teste 2         
# Nome(s): Daniele Monetenegro
#          Rodrigo Dantas da Silva 
#          Thiago Bruschi Martins
########################################

## 1 - Agrupamento

groupsum <- function(df, col1, col2){
  if(!is.numeric(df[,col2])){
    cat('A variavel Col2 deve ser numerica.')
  } else {
    tmp <- tapply(df[,col2], chuvas[,col1], sum)
    res <- data.frame(col1 = names(tmp), tmp, row.names = NULL)
    colnames(res) <- c(col1, col2)
    return(res)    
  }
}

##### Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva)
groupsum(chuvas, "cidade", "chuva")

## 2 - Binario para Decimal

binToDec <- function(...){
  conjuntoResposta <- c()
  
  for(numero in list(...)){ # percore a lista de parâmetros
    soma <- 0
    tamanho <- (length(numero))
    
    for(i in tamanho:1) # percore cada um dos vetores/parâmetros para fazer a conversão
    {
      soma <- soma + numero[i]*2^(tamanho-i)
    }
    conjuntoResposta <- append(conjuntoResposta, soma)  
  }
  return(conjuntoResposta)
}

##### Exemplos no PDF:
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(word, text){
  text <- gsub("[.|,|!]","",text)
  text <- tolower(text) 
  tam <- sapply(strsplit(text, " "), length) 
  text <- strsplit(text, " ")[[1]]
  result <- 0
  
  for (i in 1:tam) {
    if (text[i] == word) {
      result <- result + 1
    }
  }
  return(result)
  
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)
text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)
text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)
