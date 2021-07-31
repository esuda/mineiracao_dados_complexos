# rm(list=ls())
setwd("C:\\Users\\Eric\\Desktop\\Mineiracao de Dados\\Recuperacao da Informacao\\Aula 1")
source("ranking_metrics.R")

data(iris); head(iris)

iris_dist = iris[,1:4]

dist_matrix = dist(x=iris_dist, method = "euclidian",diag = TRUE)

dist_matrix <- as.matrix(dist_matrix)

samples = c(50, 100, 150)

query <- t(dist_matrix[samples,])

colnames(query) <- c('query_50', 'query_100', 'query_150')

index_50 <- order(query[,1])
index_100 <- order(query[,2])
index_150 <- order(query[,3])

total_truth <- ifelse(iris[,5]=='versicolor', 1, 0)

precision_50 <- precision(total_truth, index_50, 75)
recall_50 <- recall(total_truth, index_50, 75)
avg_precision_50 <- ap(total_truth, index_50, 75)

map(list(list(total_truth, index_50), list(total_truth, index_100), list(total_truth, index_150)), 75)

jaccard_index(index_50,index_100, 75)
jaccard_index(index_50,index_150, 75)
jaccard_index(index_100,index_150, 75)
