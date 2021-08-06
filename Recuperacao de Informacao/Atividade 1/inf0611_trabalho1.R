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
# head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
# head(queries)
# Exemplo de acesso aos tokens de uma consulta
q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
# head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
#ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
# names(ground_truths)[ground_truths[1,]==1]


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

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], 
            "\nPrecisão: ", p, 
            "\tRevocação: ", r))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text)
}

metricas <- function(query, ground_truth, stats, stat_name, top) {
  # Criando ranking (função do arquivo base)
  ranking <- get_ranking_by_stats(stat_name, stats, query$word)
  
  # Calculando a precisão
  p <- precision(ground_truth, ranking$doc_id, top)
  
  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top) 
  
  # Avaliacao entre os rankings
  f1 <- f1_score(ground_truth, ranking$doc_id, top)
  avgp <- ap(ground_truth, ranking$doc_id,top)
  
  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1],
            "\nMetodologia: ", stat_name,
            "\nPrecisão: ", p, 
            "\nRevocação: ", r,
            "\nF1: ", f1,
            "\nPrecisao media: ", avgp))
  
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
computa_resultados(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=20, text="- Consulta 1 TF-IDF")

# Resultados para a consulta 1 e bm25
computa_resultados(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=20, text="- Consulta 1 BM25")


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_020",]
n_consulta2 <- 20

# Resultados para a consulta 2 e tf_idf
computa_resultados(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                   stats=docs_stats, stat_name="tf_idf", 
                   top=20, text="- Consulta 20 TF-IDF")


# Resultados para a consulta 2 e bm25
computa_resultados(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                   stats=docs_stats, stat_name="bm25", 
                   top=20, text="- Consulta 20 BM25")


# Resultado das Media das Precisoes medias
q1_tfidf <- metricas(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                    stats=docs_stats, stat_name="tf_idf", top=20)
q1_bm25 <- metricas(query=consulta1, ground_truth=ground_truths[n_consulta1, ], 
                    stats=docs_stats, stat_name="bm25", top=20)


q2_tfidf <- metricas(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                     stats=docs_stats, stat_name="tf_idf", top=20)
q2_bm25 <- metricas(query=consulta2, ground_truth=ground_truths[n_consulta2, ], 
                    stats=docs_stats, stat_name="bm25", top=20)

# Avaliacao entre as metodologias de consulta tfidf vs bm25
avg_precision_tfid <- map(list(list(ground_truths[n_consulta1, ] , q1_tfidf$doc_id), list(ground_truths[n_consulta2, ] , q2_tfidf$doc_id)), 20)
avg_precision_bm25 <- map(list(list(ground_truths[n_consulta1, ] , q1_bm25$doc_id), list(ground_truths[n_consulta2, ] , q2_bm25$doc_id)), 20)
print(cat(paste("Precisao media tfidf: ",avg_precision_tfid,
                "\nPrecisao media bm25: ", avg_precision_bm25)))

######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################

# Consulta 1 - Query 01:
# Como os metodos TF-IDF e BM25 tem precisao e a revocacao iguais (p=0.35, r=1)
#devemos observar a metrica de precisao media. Neste caso o metodo BM25 possui
#approx. +24p.p (ap=0.620) quando comparado com TF-IDF (ap=0.856). Logo para esta
#consulta o metodo BM25 seria o modelo ideal.

# Consulta 2 - Query 20:
# A observacao dos graficos entre as metodologias claramente que o metodo BM25
#retorna um resultado melhor que o do TF-IDF pois recupera todos os arquivos de
#em um k muito mais baixo. Alem do mais o score F1 (+9p.p) e a precisao media (+39p.p)

# Observacao entre as metodologias:
# Utilizando o MAP para as duas metodologias, TF-IDF e BM25, poemos observar que 
#o metodo bm25 retorna um resultado melhor que o do tf-idf (+31p.p).

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
computa_resultados(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                   stats=docs_stats_proc, stat_name="tf_idf", 
                   top=20, text="- Consulta 1 TF-IDF")

# Resultados para a consulta 1 e bm25
computa_resultados(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                   stats=docs_stats_proc, stat_name="bm25", 
                   top=20, text="- Consulta 1 BM25")

# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_020",]
n_consulta2_proc <- 20

# Resultados para a consulta 2 e tf_idf
computa_resultados(query=consulta2_proc, ground_truth=ground_truths[n_consulta2_proc, ], 
                   stats=docs_stats_proc, stat_name="tf_idf", 
                   top=20, text="- Consulta 20 TF-IDF")

# Resultados para a consulta 2 e bm25
computa_resultados(query=consulta2_proc, ground_truth=ground_truths[n_consulta2_proc, ], 
                   stats=docs_stats_proc, stat_name="bm25", 
                   top=20, text="- Consulta 20 BM25")

# Resultado das Media das Precisoes medias
q1_tfidf_proc <- metricas(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                     stats=docs_stats_proc, stat_name="tf_idf", top=20)
q1_bm25_proc <- metricas(query=consulta1_proc, ground_truth=ground_truths[n_consulta1_proc, ], 
                    stats=docs_stats_proc, stat_name="bm25", top=20)


q2_tfidf_proc <- metricas(query=consulta2, ground_truth=ground_truths[n_consulta2_proc, ], 
                     stats=docs_stats_proc, stat_name="tf_idf", top=20)
q2_bm25_proc <- metricas(query=consulta2, ground_truth=ground_truths[n_consulta2_proc, ], 
                    stats=docs_stats_proc, stat_name="bm25", top=20)

# Avaliacao entre as metodologias de consulta tfidf vs bm25
avg_precision_tfid_proc <- map(list(list(ground_truths[n_consulta1_proc, ] , q1_tfidf_proc$doc_id), list(ground_truths[n_consulta2_proc, ] , q2_tfidf_proc$doc_id)), 20)
avg_precision_bm25_proc <- map(list(list(ground_truths[n_consulta1_proc, ] , q1_bm25_proc$doc_id), list(ground_truths[n_consulta2_proc, ] , q2_bm25_proc$doc_id)), 20)
print(cat(paste("Precisao media tfidf: ",avg_precision_tfid_proc,
                "\nPrecisao media bm25: ", avg_precision_bm25_proc)))

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
 
# Consulta 1 - Query 01:
# Alterar a retirada de stop words para esta consulta tanto utilizando o modelo
#TF-IDF ou o modelo de BM25 nao ha alteracao da precisao e da revocacao 
#quando estamos recuperando k=20 primeiros documentos pois conseguimos uma revocacao
#igual a 1 antes da recuperacao dos 20 primeiros elementos. Logo precisamos analisar
#a precisao media e podemos observar que para o TF-IDF ha uma perda minima de precisao 
#media (-2p.p), porem para o BM25 ha um aumento marginal de precisao media (+5p.p).

# Consulta 2 - Query 10:
# Semelhante ao caso da Query 01, retirar os stop words nao tem um impacto na precisao
#e revocacao em k=20, porem, diferente da Query 1, nao chegamos em um valor de revocacao
#igual a 1, logo ha espaco para as metodologias recuperarem mais registros relevantes
#antes de atingir k=20. Notamos isso de forma mais pronunciada no BM25 com precisao media
#de +22p.p e +4p.p no metodo TF_IDF.

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
































