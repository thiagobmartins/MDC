wordCount <- function(word, text){
  text <- tolower(gsub("[.|,|!|?]","", text))
  tokens <- strsplit(text, split=" ")
  
  contador_word <- 0
  for(token in tokens[[1]]){
    if(token == word){
      contador_word <- contador_word + 1
    }
  }
  
  return(contador_word)
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

