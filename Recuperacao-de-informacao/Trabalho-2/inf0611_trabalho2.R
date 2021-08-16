#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao                             #
#                                                                #
# Trabalho Avaliativo 2                                          #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# - Daniele Monetenegro                                          #
# - Rodrigo Dantas da Silva                                      #
# - Thiago Bruschi Martins                                       #
#                                                                #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares   
#----------------------------------------------------------------#
# configure o caminho antes de executar
# setwd("") 
source("./ranking_metrics.R")
source("./trabalho2_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens                 
#----------------------------------------------------------------#
imagens <- read_images(path_plantas)

#----------------------------------------------------------------#
# Obtem classe de cada imagem             
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas)

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path_plantas, nome_classes, 'biloba')
ground_truth_europaea <- get_ground_truth(path_plantas, nome_classes, 'europaea')
ground_truth_ilex <- get_ground_truth(path_plantas, nome_classes, 'ilex')
ground_truth_monogyna <- get_ground_truth(path_plantas, nome_classes, 'monogyna')
ground_truth_regia <- get_ground_truth(path_plantas, nome_classes, 'regia')



#----------------------------------------------------------------#
# Questao 1                               
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <-function(img){
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  img <- grayscale(img)[,,1,1]
  r1 <- lbp(img,1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
}

# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  
  img <- grayscale(img)[,,1,1]
  features <-NULL
  for(i in 0:2){
    for(j in 0:2){
      #adiciona um novo momento como caracteristica no vetor de caracteristicas da imagem
      features <- cbind(features,momento(img, i,j, central=TRUE))
    }
  }
  return(features)
}

#----------------------------------------------------------------#
# obtem caracteristicas de cor, textura e forma para todas as imagens e 
# armazena em matrizes onde uma linha representa uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <- t(as.data.frame(lapply(imagens, lbp_desc)))
rownames(features_t) <- names(imagens)
features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)

#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba, 'Consulta - Biloba')
mostrarImagemColorida(consulta_europaea, 'Consulta - Europaea')
mostrarImagemColorida(consulta_ilex, 'Consulta - Ilex')
mostrarImagemColorida(consulta_monogyna, 'Consulta - Monogyna')
mostrarImagemColorida(consulta_regia, 'Consulta - Regia')

#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- get_ranking_by_distance(features_c, consulta_biloba)
ranking_c_europaea <- get_ranking_by_distance(features_c, consulta_europaea)
ranking_c_ilex <- get_ranking_by_distance(features_c, consulta_ilex)
ranking_c_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna)
ranking_c_regia <- get_ranking_by_distance(features_c, consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- get_ranking_by_distance(features_t, consulta_biloba)
ranking_t_europaea <- get_ranking_by_distance(features_t, consulta_europaea)
ranking_t_ilex <- get_ranking_by_distance(features_t, consulta_ilex)
ranking_t_monogyna <- get_ranking_by_distance(features_t, consulta_monogyna)
ranking_t_regia <- get_ranking_by_distance(features_t, consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- get_ranking_by_distance(features_s, consulta_biloba)
ranking_s_europaea <- get_ranking_by_distance(features_s, consulta_europaea)
ranking_s_ilex <- get_ranking_by_distance(features_s, consulta_ilex) 
ranking_s_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna)
ranking_s_regia <- get_ranking_by_distance(features_s, consulta_regia)

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20

analyse_rankings <- function(ranking, ground_truth, top, text) {
  
  precision <- precision(ground_truth, ranking, top)
  
  recall <- recall(ground_truth, ranking, top)
  
  f1 <- f1_score(ground_truth, ranking, top)
  
  ap <- average_precision(ground_truth, ranking, top)
  
  # Imprimindo os valores de precisão e revocação
  cat(paste("Precisão: ", precision, 
            "\nRevocação: ", recall,
            "\nTaxa F1: ", f1,
            "\nPrecisão Média: ", ap))
  
  plot_prec_e_rev(ranking, ground_truth, top, text)
  
}

# analisando rankings gerados com caracteristicas de cor
analyse_rankings(ranking_c_ilex, ground_truth_ilex, 5, '\nAnálise Ilex')
analyse_rankings(ranking_c_ilex, ground_truth_ilex, 10, '\nAnálise Ilex')
analyse_rankings(ranking_c_ilex, ground_truth_ilex, 15, '\nAnálise Ilex')
analyse_rankings(ranking_c_ilex, ground_truth_ilex, 20, '\nAnálise Ilex')

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(ranking_t_ilex, ground_truth_ilex, 5, '\nAnálise Ilex')
analyse_rankings(ranking_t_ilex, ground_truth_ilex, 10, '\nAnálise Ilex')
analyse_rankings(ranking_t_ilex, ground_truth_ilex, 15, '\nAnálise Ilex')
analyse_rankings(ranking_t_ilex, ground_truth_ilex, 20, '\nAnálise Ilex')

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(ranking_s_ilex, ground_truth_ilex, 5, '\nAnálise Ilex')
analyse_rankings(ranking_s_ilex, ground_truth_ilex, 10, '\nAnálise Ilex')
analyse_rankings(ranking_s_ilex, ground_truth_ilex, 15, '\nAnálise Ilex')
analyse_rankings(ranking_s_ilex, ground_truth_ilex, 20, '\nAnálise Ilex')


#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#    
# RESPOSTA
# 2 a) 
#As análises foram realizadas com base nos dados retornados pela consulta do ilex. 
#No descrito de cor no top 5 melhor taxa foi a precisão média com 0.91; no top 10 precisão médio 
#foi 0.83; no top 15 a precisão ficou com 0.33, e a revocação subiu para 0.6; mantendo o aumento 
#também na top 20, com 0.6.
#Em relação ao descritor de textura, utilizado muito para reconhecer os padrões da imagem, 
#no top 20 a revocação foi de 0.8, e precisão apenas 0.4, fazendo com que a taxa de erro do modelo
#fosse maior. O melhor resultado para a precisão em relação a forma foi no top 5, e a revocação no 
#top 20 0.6.
#Desta forma, o descritor de cor teve uma precisão média mais alta, a textura teve uma melhor 
#taxa de revocação apesar da precisão no top 5 e top 10 serem igual a 1, a taxa de erro foi maior,
#o que levou a precisão para 0.4 e precisão média para 0.53
#em compensação a revocação foi 0.8 no top 20. Em todos os modelos a taxa do F1 score se manteve baixa,
#sendo esta uma média harmônica entre a precisão e a revocação, sendo este um indicativo que essas 
#taxas (uma ou outra) estavam baixas, sendo sua variação entre 0.53 e 0.4 para todos os descritores. 
# 
#                                         
# (b) Considerando as 5 consultas definidas, calcule a m?dia das precis?es m?dias em top 10. 
# Avaliando por essa medida, qual descritor obteve melhores resultados? Justifique. 
# Lembre-se que para justificar sua resposta, voc? pode complementar sua an?lise usando 
# tamb?m outras medidas de avalia??o de ranking adicionais vistas na Aula 1, caso seja pertinente
#               

map_biloba <- mean_average_precision(list(list(ground_truth_biloba, ranking_c_biloba), 
                                          list(ground_truth_biloba, ranking_s_biloba), 
                                          list(ground_truth_biloba, ranking_t_biloba)), 10)
map_europaea <- mean_average_precision(list(list(ground_truth_europaea, ranking_c_europaea), 
                                            list(ground_truth_europaea, ranking_s_europaea), 
                                            list(ground_truth_europaea, ranking_t_europaea)), 10)
map_ilex <- mean_average_precision(list(list(ground_truth_ilex, ranking_c_ilex), 
                                        list(ground_truth_ilex, ranking_s_ilex), 
                                        list(ground_truth_ilex, ranking_t_ilex)), 10)
map_monogyna <- mean_average_precision(list(list(ground_truth_monogyna, ranking_c_monogyna), 
                                            list(ground_truth_monogyna, ranking_s_monogyna), 
                                            list(ground_truth_monogyna, ranking_t_monogyna)), 10)
map_regia <- mean_average_precision(list(list(ground_truth_regia, ranking_c_regia), 
                                         list(ground_truth_regia, ranking_s_regia), 
                                         list(ground_truth_regia, ranking_t_regia)), 10)


cat(paste("MAP Biloba: \t", map_biloba, 
          "\nMAP Europaea: \t", map_europaea,
          "\nMAP Ilex: \t", map_ilex,
          "\nMAP Monogyna: \t", map_monogyna,
          "\nMAP Regia: \t", map_regia))

# RESPOSTA
# MAP Biloba: 	 0.802777777777778 
# MAP Europaea: 	 0.982804232804233 
# MAP Ilex: 	 0.87218253968254 
# MAP Monogyna: 	 0.819312169312169 
# MAP Regia: 	 0.798233182161754
# O descritor que obteve o melhor resultado foi para a classe Europaea. Foi criado um map onde é calculada 
# a médias das médias onde o valor para essa classe foi de 0.982.
#                                         
#                                         
#                                         
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat = cbind(features_c, features_t, features_s)

# gerar novos rankings
ranking_concat_biloba <- get_ranking_by_distance(features_concat, consulta_biloba)
ranking_concat_europaea <- get_ranking_by_distance(features_concat, consulta_europaea)
ranking_concat_ilex <- get_ranking_by_distance(features_concat, consulta_ilex)
ranking_concat_monogyna <- get_ranking_by_distance(features_concat, consulta_monogyna)
ranking_concat_regia <- get_ranking_by_distance(features_concat, consulta_regia)

# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(ranking_concat_biloba, ground_truth_biloba, 5, '\nAnálise Biloba - Concatenado')
analyse_rankings(ranking_concat_biloba, ground_truth_biloba, 10, '\nAnálise Biloba - Concatenado')
analyse_rankings(ranking_concat_biloba, ground_truth_biloba, 15, '\nAnálise Biloba - Concatenado')
analyse_rankings(ranking_concat_biloba, ground_truth_biloba, 20, '\nAnálise Biloba - Concatenado')

analyse_rankings(ranking_concat_europaea, ground_truth_europaea, 5, '\nAnálise Europaea - Concatenado')
analyse_rankings(ranking_concat_europaea, ground_truth_europaea, 10, '\nAnálise Europaea - Concatenado')
analyse_rankings(ranking_concat_europaea, ground_truth_europaea, 15, '\nAnálise Europaea - Concatenado')
analyse_rankings(ranking_concat_europaea, ground_truth_europaea, 20, '\nAnálise Europaea - Concatenado')

analyse_rankings(ranking_concat_ilex, ground_truth_ilex, 5, '\nAnálise Ilex - Concatenado')
analyse_rankings(ranking_concat_ilex, ground_truth_ilex, 10, '\nAnálise Ilex - Concatenado')
analyse_rankings(ranking_concat_ilex, ground_truth_ilex, 15, '\nAnálise Ilex - Concatenado')
analyse_rankings(ranking_concat_ilex, ground_truth_ilex, 20, '\nAnálise Ilex - Concatenado')

analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna, 5, '\nAnálise Monogyna - Concatenado')
analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna, 10, '\nAnálise Monogyna - Concatenado')
analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna, 15, '\nAnálise Monogyna - Concatenado')
analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna, 20, '\nAnálise Monogyna - Concatenado')

analyse_rankings(ranking_concat_regia, ground_truth_regia, 5, '\nAnálise Regia - Concatenado')
analyse_rankings(ranking_concat_regia, ground_truth_regia, 10, '\nAnálise Regia - Concatenado')
analyse_rankings(ranking_concat_regia, ground_truth_regia, 15, '\nAnálise Regia - Concatenado')
analyse_rankings(ranking_concat_regia, ground_truth_regia, 20, '\nAnálise Regia - Concatenado')
#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?
# As analises foram feitas em relação a mesma classe da questão 2, a ilex. 
# O descritor de cor no top 5 que tinha uma precisão média de 0.91, passou para 0.80 com o
#ranking concatenado. No top 10 a revocação foi de 0.5, menor que o resultado do ranking 
#sem a concatenação. 
# 
# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.
# De forma geral, as features concatenadas obtiveram um resultado inferior, do que elas separadas.
# As métricas de precisão média, precisão e rovocação nos top 5, 10, 15 e 20 ou mantiveram as mesmas taxas
# ou os resultados foram inferiores.
# 
# 
# 
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# Não, os resultados mostrar que todos os descritores, analisados de forma separada obtiveram resultados 
# uniformes, não demonstrando uma grande variação entre eles. Como demonstrado o F1 score se manteve entre 
# 0.53 e 0.4, monstrando uma baixa taxa da revocação ou da precisão em todos os modelos.
# 
# 
#----------------------------------------------------------------#
