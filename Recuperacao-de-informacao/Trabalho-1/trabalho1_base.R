
process_data <- function(path,regex_sep, name, convertcase = FALSE, remove_stopwords = FALSE) {
    # Carregando o texto dos documentos
    lines <- readLines(path, encoding = "UTF-8")
    # Obtendo o nome dos documento
    doc <- grep(regex_sep, lines)
    start <- c(1, doc+1) # + 1 para pular o titulo
    end <- c(doc - 1, length(lines))
    text <- mapply(function(s, e) paste(lines[s:e], 
                                        collapse = "\n"), start, end)

    # Removendo espacos brancos em sequencia
    text <- trimws(text)
    # Obtendo os titulos dos documento e 
    doc_id <- sub(regex_sep, name, lines[doc])
    # Removendo espacos brancos em sequencia
    doc_id <- trimws(doc_id)
    
    # Descartando a linha inicial
    text <- text[-1]
    # Vinculando titulo do documento com texto do documento
    data <- tibble(doc_id, text)

    # Obtendo tokens por documento
    doc_tokens <- unnest_tokens(data, word, text, to_lower=convertcase)

    # Removendo stop_words
    if (remove_stopwords) {
        doc_tokens <- doc_tokens %>% filter(!word %in% stop_words$word)
    }
    doc_tokens$doc_id <- as.factor(doc_tokens$doc_id) 
    doc_tokens$doc_id <- ordered(doc_tokens$doc_id, levels=unique(doc_tokens$doc_id))
    return(doc_tokens)
}

scores_termo_consulta <- function(docs_stats, 
                                  query_term, stat) {
  # Seleciona as linhas que contém o termo da consulta
    term_stat <- docs_stats[docs_stats$term == query_term,]
    # Seleciona as colunas com o id do documento e a estatística 
    term_stat <- term_stat[,c("doc_id", stat)]
    # Renomeando a colunas
    stat_col_name <- paste(stat, query_term, sep="-")
    colnames(term_stat) <- c("doc_id", stat_col_name)
    return(term_stat)
}

# - stat_name deve ser um desses valores:
#     "tf", "idf", "tf_idf", "tf_bm25" OU "bm25"
# - docs_stats deve ser um data.frame obtido através da 
#     conversão dos resultados da função 
#     document_term_frequencies_statistics
# - tokens_query deve ser uma lista simples de tokens
get_ranking_by_stats <- function(stat_name, 
                                 docs_stats, 
                                 tokens_query) {
  tokens_query <- unique(tokens_query)
  # Para cada elemento x de tokens_query, executamos 
  # uma chamada da função scores_termo_consulta e 
  # obtemos uma lista com os data.frame's resultantes
  # da execução da função scores_termo_consulta
  stats <- lapply(tokens_query, 
                  function(x) scores_termo_consulta(
                        docs_stats, x, stat_name)
                  )
  # Aplicamos a função reduce que faz um full_join das
  stats <- stats %>% reduce(full_join, by = "doc_id")
  # Substituindo valores NA (faltantes) por 0
    stats[is.na(stats)] <- 0
    # Somando os valores da estatística para cada documento 
    # Multiplica os tf_idfs dos termos 'good' e 'witch'
    aux <- stats[,-1] %>% 
      mutate(SUM = Reduce(`+`, .))
    stats$SUM <- aux$SUM
    # Ordenando os documentos pelo valor da soma das 
    # estatísticas de cada termo da consulta
    ranking <- stats[order(stats$SUM, decreasing = TRUE),]
    return(ranking)
}




plot_prec_x_rev <- function(ranking, groundtruth, k) {
  # Calculando a precisão com a função precision para 
  # cada valor de 1 ate k e armazenando no vetor p
  p <- mapply(precision, 1:k, MoreArgs = list(
    gtruth = groundtruth, ranking = ranking))
  # Calculando a revocação com a função recall para 
  # cada valor de 1 ate k e armazenando no vetor r
  r <- mapply(recall, 1:k, MoreArgs = list(
    gtruth = groundtruth, ranking = ranking))

  # Criando um data.frame com k linhas e duas colunas 
  # que armazenam os valores de precisão (prec) e 
  # revocação (rev)
  pr <- data.frame(prec = p, rec = r)
  # Criando plot a partir data.frame pr
  ggplot(pr, aes(x = rec)) + 
    geom_point(aes(y = prec)) + 
    geom_line(aes(y = prec)) +
    theme_light() +
    labs(colour = element_blank(), 
         title = "Precisão x Revocação") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(name = "Precisão", 
                       limits = c(0, 1), 
                       breaks = 0.1 * 0:10,
                       minor_breaks = NULL) +
    scale_x_continuous(name = "Revocação", 
                       limits = c(0, 1), 
                       breaks = 0.1 * 0:10,
                       minor_breaks = NULL)
}


plot_prec_e_rev <- function(ranking, groundtruth, k, text) {
  # Calculando a precisão com a função precision para 
  # cada valor de 1 ate k e armazenando no vetor p
  p <- mapply(precision, 1:k, MoreArgs = list(
    gtruth = groundtruth, ranking = ranking))
  # Calculando a revocação com a função recall para 
  # cada valor de 1 ate k e armazenando no vetor r
  r <- mapply(recall, 1:k, MoreArgs = list(
    gtruth = groundtruth, ranking = ranking))

  # Criando um data.frame com k linhas e duas colunas 
  # que armazenam os valores de precisão (prec) e 
  # revocação (rev)
  pr <- data.frame(prec = p, rec = r)
  # Criando plot a partir data.frame pr
  ggplot(pr, aes(x = 1:k)) + 
    # Adicionado linha e pontos referentes a precisão 
    geom_point(aes(y = prec, colour = "Precisão")) + 
    geom_line(aes(y = prec, colour = "Precisão")) +
    # Adicionado linha e pontos referentes a revocação 
    geom_point(aes(y = rec, colour = "Revocação")) + 
    geom_line(aes(y = rec, colour = "Revocação")) +
    # Definindo um tema com configurações básicas de 
    # visualização do gráfico a ser gerado
    theme_light() +
    # Definindo conteúdo do título do gráfico
    labs(colour = element_blank(), 
         title = paste("Precisão e Revocação X Top k", text)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    # Configurando a legenda
    theme(legend.position = c(0.5, 0.15)) +
    theme(legend.box.background = 
            element_rect(colour = "black")) +
    # Configurando o eixo x com legenda e número 
    # de marcações
    scale_x_continuous(name = "Top k", 
                       limits = c(1, k), 
                       breaks = 5 * 1:(k/5), 
                       minor_breaks = NULL) +
    # Configurando o eixo y com legenda e número 
    # de marcações
    scale_y_continuous(name = "", limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL)
}