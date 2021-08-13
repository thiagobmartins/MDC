######################################################################
# INF-0612 Análise de Informação                                     #
#                                                                    #
# Teste 2 - Função de Agrupamento                                    #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Daniele Montenegro da Silva Barros                             #
#   - Thiago Bruschi Martins                                         #
#   - Rodrigo Silva Dantas                                           #
#                                                                    #
######################################################################


dia <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)

cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')

chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)

df <- data.frame(dia, cidade, chuva)

groupsum <- function(df, colgroup, colsum){
   teste <- tapply(df[[colsum]], df[[colgroup]], sum)
   novo_df <- data.frame(rownames(teste), teste)
   names(novo_df)[1] <- colsum
   rownames(novo_df) <- NULL
  return(novo_df)
}

groupsum(df, "cidade", "chuva")
