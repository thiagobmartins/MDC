######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Daniele Montenegro da Silva Barros                             #
#   - Thiago Bruschi Martins                                         #
#   - Rodrigo Silva Dantas                                           #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(corpus)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("ranking_metrics.R", encoding = "UTF-8")
source("trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
# setwd("~/Documents/Unicamp/Recuperação de Informação/Atividade 01/")


######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
head(queries)
# Exemplo de acesso aos tokens de uma consulta
q1 <- queries[queries$doc_id == "Query_01",]
q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]

# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs, term='word')

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, 
                                                                 k = 1.2, 
                                                                 b = 0.75))
# Visualizando as estatísticas da coleção (apenas para debuging)
head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################

# Incluindo implementacao das metricas
source("ranking_metrics.R")

# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  ranking <- get_ranking_by_stats(stat_name = stat_name,
                                  docs_stats = stats,
                                  tokens_query = text_tokens(query[[1]]))
  # Visualizando o ranking (apenas para debuging)
  # head(ranking, n = 5)
  
  # Calculando a precisão
  p <- precision(ground_truth,
                 ranking$doc_id,
                 top)

  # Calculando a revocação
  r <- recall(ground_truth,
              ranking$doc_id,
              top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text) 
}

# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id == "Query_014", 2]
n_consulta1 <- 14

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ],
#                    docs_stats, "nome da statistica",
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ],
                   docs_stats, "tf_idf",
                   top = 20, "Ranking TF-IDF")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ],
                   docs_stats, "bm25",
                   top = 20, "Ranking BM25")


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_020", 2]
n_consulta2 <- 20

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2, ],
                   docs_stats, "tf_idf",
                   top = 20, "Ranking TF-IDF")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2, ],
                   docs_stats, "bm25",
                   top = 20, "Ranking BM25")


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
# Na Query 20, quando k assume o valor de 7 o método bm25 recuperou todos os objetos relevantes, 
# assim, a revocação assume o valor 1, mantendo seu valor até k igual a 20. Já a tf-idf a revocação só sai do valor
# de 0 quando k igual a 13, e quando k igual a 15 assume seu maior valor, sendo a precisão inferior utilizando tf-id.
# Assim, no TF-IDF da query20 não chega a recuperar todos os elementos relevantes nem com k igual a 20.

# Enquanto na Query 14, quando k assume o valor de 5, tanto no método tf-id e na BM-25, a revocação é igual 1, 
# assim, em ambos os métodos todos os objetos relevantes foram encontrados quando k igual a 5. 
# Já a precisão quando k igual a 5 a precisão é igual a 0.8. No entanto, 
#  tem uma pequena diferença o TF-IDF começa acertando o primeiro elemento, assumindo um melhor desempenho no 
# ínicio do processamento.
######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
# head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
# head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc, term='word')

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, 
                                                                      k = 1.2, 
                                                                      b = 0.75))


# Definindo a consulta 1 
consulta1_proc <- queries[queries$doc_id == "Query_014", 2]
n_consulta1_proc <- 14
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ],
                   docs_stats, "tf_idf",
                   top = 20, "Ranking TF-IDF (No StopWords)")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ],
                   docs_stats, "bm25",
                   top = 20, "Ranking BM25 (No StopWords)")


# Definindo a consulta 2 
consulta2_proc <- queries[queries$doc_id == "Query_020", 2]
n_consulta2_proc <- 20

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ],
                   docs_stats, "tf_idf",
                   top = 20, "Ranking TF-IDF (No StopWords)")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ],
                   docs_stats, "bm25",
                   top = 20, "Ranking BM25 (No StopWords)")

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# 
# Na query 20 tanto a revocação quanto a precisão melhoram significativamente com a remoção das stopwords.
# Podemos verificar isso nos gráficos pois os valores de revocação e precisão atingem valores mais altos para
# K menores em realação ao mesmo modelo com as stopwords. No caso da revocação ela também atinge seu valor máximo
# (100%) antes. No caso da precisão, para K menores que 6 ela fica mais tempo em valores mais altos, embora no final o 
# valor seja o mesmo visto que há poucos elementos relevantes nesta pesquisa.
# 
# Já na query 14 os gráficos gerados foram os mesmos, ou seja, não houve diferença entre remover ou não as stopwords.
# Provavelmente por ser uma query com menos palavras, e por tanto, menos stopwords.
# 
#
######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
#                              full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
#                               full.names = TRUE)
# file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################
































