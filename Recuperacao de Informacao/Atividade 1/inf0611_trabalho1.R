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

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], 
            "\nPrecisão: ", p, 
            "\tRevocação: ", r,
            "\nF1: ", f1,
            "\nPrecisao media: ", avgp))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text)
}

# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id == "Query_010",]
n_consulta1 <- 10

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=15, text="- Consulta 10 TF-IDF")

# Resultados para a consulta 1 e bm25
computa_resultados(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=15, text="- Consulta 10 BM25")


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_020",]
n_consulta2 <- 20

# Resultados para a consulta 2 e tf_idf
computa_resultados(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=15, text="- Consulta 20 TF-IDF")


# Resultados para a consulta 2 e bm25
computa_resultados(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=15, text="- Consulta 20 BM25")


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
# Para a Consulta 10 do arquivo txt ambos os metodos bm25 e tf-idf apresentam 
# Precisao, revocacao e F1 iguais, porem utilizando o bm25 obtemos uma precisao
# media com aprox 1 p.p. acima, logo bm25 seria o modelo indicado.
#
# Para a consulta 20 do arquivo txt, visualmente observando os graficos podemos 
# afirmar que o modelo bm25 seria o mais indicado, bem como avaliando o score f1
# e a precisao media.
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
head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc, term="word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, k = 1.2, b = 0.75)) 


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_010",]
n_consulta1_proc <- 10
# Resultados para a consulta 1 e tf_idf
computa_resultados(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=15, text="- Consulta 10 TF-IDF")

# Resultados para a consulta 1 e bm25
computa_resultados(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=15, text="- Consulta 10 BM25")


# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_020",]
n_consulta2_proc <- 20

# Resultados para a consulta 2 e tf_idf
computa_resultados(query=consulta2_proc, ground_truth=ground_truths[n_consulta2_proc, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=15, text="- Consulta 20 TF-IDF")

# Resultados para a consulta 2 e bm25
computa_resultados(query=consulta2_proc, ground_truth=ground_truths[n_consulta2_proc, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=15, text="- Consulta 20 BM25")


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
































