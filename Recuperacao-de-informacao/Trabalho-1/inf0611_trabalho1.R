######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Thiago Bruschi Martins                                         #
#   - Dani Ribeiro                                                   #
#   - Rodrigo                                                        #
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

source <- function(f) {
  l <- readLines(f)
  eval(parse(text=l),envir=.GlobalEnv)
}

source("./ranking_metrics.R")
source("./trabalho1_base.R")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
# setw("")


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
# head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)

# Visualizando as consultas (apenas para debuging)
 head(queries)
# Exemplo de acesso aos tokens de uma consulta
 q1 <- queries[queries$doc_id == "Query_01",]; q1

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
term_freq <- document_term_frequencies(docs, term = "word", document="doc_id")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq))
# Visualizando as estatísticas da coleção (apenas para debuging)
 head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


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
  ranking <- get_ranking_by_stats(stat_name, stats, query[[2]])
  
  head(ranking)
  # Visualizando o ranking (apenas para debuging)
  # head(ranking, n = 5)
  
  # Calculando a precisão
  p <- precision(ground_truth, ranking[, 1], top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text) 
}

# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id=="Query_01", ]
n_consulta1 <- 1

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ], docs_stats, "tf_idf", 10, "TF-IDF")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ], docs_stats, "bm25", 10, "BM25")


# Definindo a consulta 2 
consulta2 <- ...
n_consulta2 <- ...

# Resultados para a consulta 2 e tf_idf
computa_resultados(...)

# Resultados para a consulta 2 e bm25
computa_resultados(...)


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
#
#
#
#

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
term_freq_proc <- ...

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- ...


# Definindo a consulta 1 
consulta1_proc <- ...
n_consulta1_proc <- ...
# Resultados para a consulta 1 e tf_idf
computa_resultados(...)

# Resultados para a consulta 1 e bm25
computa_resultados(...)


# Definindo a consulta 2 
consulta2_proc <- ...
n_consulta2_proc <- ...

# Resultados para a consulta 2 e tf_idf
computa_resultados(...)

# Resultados para a consulta 2 e bm25
computa_resultados(...)

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# 
# 
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
































