######################################################################
# INF-0612 Análise de Informação                                     #
#                                                                    #
# Teste 2 - Binário para Decimal                                     #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Daniele Montenegro da Silva Barros                             #
#   - Thiago Bruschi Martins                                         #
#   - Rodrigo Silva Dantas                                           #
#                                                                    #
######################################################################


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

# Teste
binToDec(c(1,0,1,0), c(1,0))
