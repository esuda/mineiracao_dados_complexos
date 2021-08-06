######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   -                                                                #
#   -                                                                #
#   -                                                                #
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


# Configure aqui o diretório onde se encontram os arquivos do trabalho
# setw("")
setwd('C:\\Users\\Eric\\Documents\\GitHub\\mineiracao_dados_complexos\\Recuperacao de Informacao\\Atividade 1')

source("C:\\Users\\Eric\\Documents\\GitHub\\mineiracao_dados_complexos\\Recuperacao de Informacao\\Aula 1\\ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

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
term_freq <- document_term_frequencies(docs, term="word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, k = 1.2, b = 0.75))
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
  ranking <- get_ranking_by_stats(stat_name, stats, query$word)
  # Visualizando o ranking (apenas para debuging)
  head(ranking, n = 5)
  
  # Calculando a precisão
  p <- precision(ground_truth, ranking$doc_id, top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top) 
  
  # Avaliacao entre os rankings
  f1 <- f1_score(ground_truth, ranking$doc_id, top)
  avgp <- ap(ground_truth, ranking$doc_id,top)
  # mavg<- map(gtruths_rankings, k)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], 
            "\nPrecisão: ", p, 
            "\tRevocação: ", r,
            "\nF1: ", f1,
            "\nPrecisao media: ", avgp))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text)
  
  return(ranking)
}

# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id == "Query_01",]
n_consulta1 <- 1

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
rank_tf1 <- computa_resultados(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=100, text="- Consulta 1 TF-IDF")

# Resultados para a consulta 1 e bm25
rank_bm1 <- computa_resultados(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=20, text="- Consulta 1 BM25")


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_020",]
n_consulta2 <- 20

# Resultados para a consulta 2 e tf_idf
rank_tf2 <- computa_resultados(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=20, text="- Consulta 20 TF-IDF")
plot_prec_e_rev(rank_tf2$doc_id, ground_truths[n_consulta2, ], 20, "- Consulta 20 TF-IDF")

# Resultados para a consulta 2 e bm25
rank_bm2 <- computa_resultados(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=20, text="- Consulta 20 BM25")
plot_prec_e_rev(rank_bm2$doc_id, ground_truths[n_consulta2, ], 20, "- Consulta 20 BM25")

# Resultado das Media das Precisoes medias
map(list(list(ground_truths[n_consulta1, ] , rank_tf1$doc_id), list(ground_truths[n_consulta2, ] , rank_tf2$doc_id)), 20)
map(list(list(ground_truths[n_consulta1, ] , rank_bm1$doc_id), list(ground_truths[n_consulta2, ] , rank_bm2$doc_id)), 20)

#mrr(ground_truths[n_consulta1, ] , list(rank_tf1$doc_id, rank_tf2$doc_id))
#mrr(ground_truths[n_consulta1, ] , list(rank_bm1$doc_id, rank_bm2$doc_id))


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
# Para a Consulta 1, pela precisao media conseguimos ver que o metodo bm25 
# apresenta um valor maior (~24p.p) quando comparado ao tf-idf.

# Para a consulta 20, apenas a observacao visual da precisao e revocacao
# podemos observar que o metodo bm25 retorna um resultado melhor que o tf-idf
# (~39p.p de diferenca de precisao media)

# De forma geral, atraves do MAP, vemos que o metodo bm25 retorna um resultado
# melhor que o do tf-idf (~31p.p)
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
term_freq_proc <- document_term_frequencies(docs_proc, term="word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, k = 1.2, b = 0.75)) 


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_01",]
n_consulta1_proc <- 1
# Resultados para a consulta 1 e tf_idf
rank_tf1 <- computa_resultados(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                   stats=docs_stats_proc, stat_name="tf_idf", 
                   top=20, text="- Consulta 1 TF-IDF")

plot_prec_e_rev(rank_tf1$doc_id, ground_truths[n_consulta1_proc, ], 100, "- Consulta 1 TF-IDF")

# Resultados para a consulta 1 e bm25
rank_bm1 <- computa_resultados(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                   stats=docs_stats_proc, stat_name="bm25", 
                   top=20, text="- Consulta 1 BM25")

plot_prec_e_rev(rank_bm1$doc_id, ground_truths[n_consulta1_proc, ], 20, "- Consulta 1 BM25")

# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_020",]
n_consulta2_proc <- 20

# Resultados para a consulta 2 e tf_idf
rank_tf2 <- computa_resultados(query=consulta2_proc, ground_truth=ground_truths[n_consulta2_proc, ], 
                   stats=docs_stats_proc, stat_name="tf_idf", 
                   top=20, text="- Consulta 20 TF-IDF")

# Resultados para a consulta 2 e bm25
rank_bm2 <- computa_resultados(query=consulta2_proc, ground_truth=ground_truths[n_consulta2_proc, ], 
                   stats=docs_stats_proc, stat_name="bm25", 
                   top=20, text="- Consulta 20 BM25")
plot_prec_e_rev(rank_bm2$doc_id, ground_truths[n_consulta2_proc, ], 20, "- Consulta 2 BM25")

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# Alterar a retirada de stop words nao altera a precisao e a revocacao para k=20
# primeiras palavras, pois conseguimos recuperar todas as palavras alvo antes de k=20,
# porem ao analisar a precisao media, podemos ver que retirando 
# os stopwords a precisao media foi maior pois indica que conseguimos recuperar antes os resultados. 
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
































