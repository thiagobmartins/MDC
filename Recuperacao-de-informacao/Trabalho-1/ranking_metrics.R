######APÓS-REVISÃO###########
# Usar lista abaixo para remover acentuações 
# áàãâéèẽêíìĩîóòõôúùũûçñ
# ÁÀÃÂÉÈẼÊÍÌĨÎÓÒÕÔÚÙŨÛÇÑ
#####################################################
# Mineração de Dados Complexos -- MDC 2021 
# Recuperação de Informação
# Implementações dos métodos de avaliação de ranking
#####################################################


#####################################################
# Notação comum:
# 
# gtruth - ground truth: vetor 1xn binário, 
#     gtruth[i] = 1 indica que o i-ésimo elemento 
#     é relevante e gtruth[i] = 0 indica que o 
#     i-ésimo elemento NÃO é relevante 
# relev: vetor com os graus relevâncias dos elementos 
# ranking: vetor com índices para os elementos 
#     ordenados, deve ser possível acessar o gtruth 
#     e o relev com esses índices
# k: tamanho do topo a ser considerado nos cálculos
# r: total de elementos relevantes no topo_k
# t: total de elementos relevantes na base
#####################################################


#####################################################
# * Avaliação para relevância binária:
#   - Precisão 
#   - Revocação
#   - Taxa F1
#   - Precisão Média
#   - Média das Precisões Médias
# 
#####################################################

######### 
# Precisão
# (P_k = r / k)
precision <- function(gtruth, ranking, k) {
    r <- sum(gtruth[ranking[1:k]])
    return(r / k)
}

######### 
# Revocação
# (R_k = r / t)
recall <- function(gtruth, ranking, k) {
    r <- sum(gtruth[ranking[1:k]])
    t <- sum(gtruth)
    return(r / t)
}

########### 
# Taxa F1 
# (f1 = 2 * P_k * R_k / (P_k + R_k))
f1_score <- function(gtruth, ranking, k) {
  P_k <- precision(gtruth, ranking, k)
  R_k <- recall(gtruth, ranking, k)
  return((2 * P_k * R_k) / (P_k + R_k))
}

########## 
# Precisão Média 
# (AP_k = 1/r * sum((r_i - r_(i-1))*P_i))
ap <- function(gtruth, ranking, k) {
    r <- sum(gtruth[ranking[1:k]])
    if (r == 0) return(0)
    
    # índices do ranking onde um elemento relevante é
    # retornado
    k_relevant <- which(gtruth[ranking[1:k]] == 1)

    # lista de precisões 
    Ps <- mapply(precision, k = k_relevant, 
                 MoreArgs = list(gtruth = gtruth, 
                                 ranking = ranking))

    return(sum(Ps) / r)
}
average_precision <- ap

########## 
# Médias das Precisões Médias
# (MAP_k = 1/n * sum( AP_k ))
# gtruths_rankings é uma lista de pares 
# (ground truth, ranking)
map <- function(gtruths_rankings, k) {
    # aplicando a função de precisção média em 
    # cada ranking da lista
    APs <- sapply(gtruths_rankings, 
                  function(x) ap(x[[1]], x[[2]], k))
    
    return(mean(APs))
}
mean_average_precision <- map


#####################################################
# * Avaliação para relevância em níveis:
#   - Ganho Cumulativo 
#   - Ganho Cumulativo Descontado
#   - Ganho Cumulativo Descontado Normalizado
# 
#####################################################

########## 
# Ganho Cumulativo
# (CG_k = sum (rel_i))
cg <- function(relev, ranking, k) {
    return(sum(relev[ranking[1:k]]))
}
cumulative_gain <- cg 

########## 
# Ganho Cumulativo Descontado
# (DCG_k = sum (rel_i / lg (i+1)))
dcg <- function(relev, ranking, k) {
    # lista de descontos 
    desc <- log(2:(k+1), 2)

    return(sum(relev[ranking[1:k]] / desc))
}
discounted_cumulative_gain <- dcg

########## 
# Ganho Cumulativo Descontado Normalizado
# (NDCG_k = DCG_k / IDCG_k)
ndcg <- function(relev, ranking, k) {
    
    dcg <- dcg(relev, ranking, k)

    # usando a função order para obter as posições 
    # de um ranking ideal, ou seja, com os maiores 
    # graus de relevância em ordem decrescente 
    ideal_ranking <- order(relev, decreasing=TRUE)
    idcg <- dcg(relev, ideal_ranking, k)

    return(dcg / idcg)
}
normalized_discounted_cumulative_gain <- ndcg

#####################################################
# * Avaliação entre rankings:
#   - Média de Ranking Recíproco
#   - Índice de Jaccard
#   - Distância de Kendall Tau
# 
###########

########## 
# Média de Ranking Recíproco
# (MRR = 1 / n * sum ( 1 / pos_i))
mrr <- function(gtruth, rankings) { 
    # retorna a posição da primeira ocorrência do 
    # valor máximo da lista, no caso do ground truth
    # o valor máximo é 1
    pos_i <- function(ranking) {
        which.max(gtruth[ranking])
    }
    # aplicando a função pos_i em cada ranking
    rr <- mapply(pos_i, rankings)
    return(mean(1/rr))
}
mean_reciprocal_ranking <- mrr

########## 
# Índice de Jaccard 
# (JAC_k = |a interseção b |/|a união b|)
jaccard_index <- function(ranking_a, ranking_b, k) {
  uni <- union(ranking_a[1:k], ranking_b[1:k])
  inter <-  intersect(ranking_a[1:k], ranking_b[1:k])
  return(length(inter) / length(uni))
}

########## 
# Coeficiente de Kendall-Tau 
# 
kendall_tau <- function(ranking_a, ranking_b ) {
    D <- length(ranking_a)
    Cd <- (D * (D - 1)) / 2
    
    # obtendo as posições dos elementos nos rankings
    a <- order(ranking_a)
    b <- order(ranking_b)

    # função testa se há uma discordância entre as 
    # posições dos elementos i e j
    disagree <- function(i, j) {
        return((a[j] < a[i] && b[j] > b[i]) || 
                 (a[j] > a[i] && b[j] < b[i]))
    }

    # função testa as discordâncias entre o elemento 
    # j e os elementos i, com i < j
    count_disagree <- function(j) {
        mapply(disagree, i = 1:(j-1), 
               MoreArgs = list(j = j))
    }
    disagree <- unlist(mapply(count_disagree, D:2))
    
    return(sum(disagree / Cd))
}

#####################################################
# * Funções úteis para gerar plots:
#   - Tamanho do topo do ranking (eixo x) x
#     precisão e revocação (eixo y)
#   - Precisão (eixo x) x  Revocação (eixo y)
# 
###########
# Lista de cores para os plots
clrs <- c("#FC4E07", "#E7B800", "#141B41", "#00AFBB",
          "#74B45E", "#14591D", "#5B2A86")

plot_measure_x_topk <- function(df, k, legends, 
                                title) {
    library(ggplot2)
    library(reshape2)
    df$x <- c(1:k)
    df.long<-melt(df, id.vars="x")
    ggplot(df.long, aes(x, value, color=variable))+
      geom_line() + theme_light() +
      labs(colour = element_blank(), title = title) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_x_continuous(name = "Top k", 
                         limits = c(1, k), 
                         breaks = 5 * 1:(k/5), 
                         minor_breaks = NULL) + 
      scale_y_continuous(name = "", limits = c(0, 1), 
                         breaks = 0.1 * 0:10, 
                         minor_breaks = NULL)+
      scale_color_manual(labels = legends, 
                         values = clrs)
}

plot_precision_x_recall <- function(df,  title) {
    library(ggplot2)
    library(reshape2)
    
    ggplot(df, aes(x = rec)) + 
      geom_point(aes(y = prec)) + 
      geom_line(aes(y = prec)) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_x_continuous(name = "Revocação", 
                         limits = c(0.1, 0.6), 
                         breaks = 0.1 * 0:6, 
                         minor_breaks = NULL) + 
      scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                         breaks = 0.1 * 0:10, 
                         minor_breaks = NULL)
}

#####################################################
# Histórico
# 
# 01.02.2020 - (C.M.R.) Versão inicial
# 29.12.2020 - (A.P.S.D.) Refatoração e comentários
# dd.mm.aaaa - (W.H.O.) Desc.
#####################################################