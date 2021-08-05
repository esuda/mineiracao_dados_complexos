##################################################################
# Mineracao de Dados Complexos -- MDC 2021 
# Recuperacao de Informacao
# Codigos da Aula 2 - Recuperacao de Texto
##################################################################


##################################################################
# Preparando ambiente - workspace
# # Linux
# setwd("~/user/diretorio/INF-0611/aula2/")
# # Windows
# setwd("C:\\Users\\Eric\\Documents\\GitHub\\mineiracao_dados_complexos\\Recuperacao de Informacao\\Aula 2\\")

# # Instalando os pacotes
#install.packages("tm")
#install.packages("dplyr")
#install.packages("corpus")
#install.packages("janeaustenr")
#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("udpipe")

##################################################################
# Processamento de Texto
##################################################################

setwd("C:\\Users\\Eric\\Documents\\GitHub\\mineiracao_dados_complexos\\Recuperacao de Informacao\\Aula 2\\")
install.packages("tm")
install.packages("dplyr")
install.packages("corpus")
install.packages("janeaustenr")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("udpipe")
##################################################################
# Analise Lexica - Exemplo
# carregando o pacote 
library(corpus)

# criando um exemplo simples, cada palavra sera um documento
text <- "...Mas a saudade e isto mesmo; e o passar e repassar das 
memorias antigas..."

# criando os tokens 
tokens <- text_tokens(text); tokens

##################################################################
# Remocao de stopwords - Exemplo
# carregando o pacote
library(tm)

# Relembrando: corpus e um termo usado para designar uma colecao 
# de documentos de texto. A funcao tm_map (do pacote tm) aplica 
# diversas operacoes no corpus

# listando as operacoes disponiveis para uso com a funcao tm_map
getTransformations()

# visualizando a lista de stopwords do portugues
head(stopwords("portuguese"))

# criando um objeto do tipo corpus a partir da lista de tokens 
corpus <- Corpus(VectorSource(tokens))

# visualizando documentos
inspect(corpus)

# removendo stopwords do portugues
corpus_sw <- tm_map(corpus, removeWords, stopwords("portuguese"))
# Warning message:
# In tm_map.SimpleCorpus(corpus, removeWords, stopwords("portuguese")) :
#   transformation drops documents
# !!! Apenas um aviso causado pelo fato de os documentos nao 
# possuirem nomes 

# visualizando documentos sem stopwords
inspect(corpus_sw)


# Exemplo 2 -- Stopwords do ingles
# criando um corpus onde cada frase e um documento
text_en <- c("Because I could not stop for Death -", 
             "He kindly stopped for me -", 
             "The  Carriage held but just Ourselves -", 
             "and Immortality")

# removendo stopwords do ingles
corpus_en <- Corpus(VectorSource(text_en))
corpus_en_sw <- tm_map(corpus_en, removeWords, stopwords())

# visualizando os documentos sem as stopwords
inspect(corpus_en_sw)

##################################################################
# Stemming (Portugues) - Exemplo
# carregando a biblioteca
library(corpus)

# aplicando tokenizacao seguida de stemming
stemming_c <- text_tokens(text, stemmer = "pt") 

# visualizando os tokens apos o stemming
stemming_c

##################################################################
# Stemming (Ingles) - Exemplo
# aplicando stemming
stems_en <- text_tokens(text_en, stemmer = "en") 

# visualizando documentos apos o stemming
stems_en

##################################################################
# Normalizacao - Exemplo
# carregando a biblioteca
library(tm)

# criando um documento simples
text <- "...Mas a saudade          e isto    mesmo; e o passar e 
repassar das memorias  antigas..."

# criando um corpus com apenas um documento
corpus <- Corpus(VectorSource(text))

# removendo espacos em branco
corpus_norm <- tm_map(corpus, stripWhitespace)
corpus_norm[[1]]$content

# removendo pontuacao
corpus_norm <- tm_map(corpus_norm, removePunctuation)
corpus_norm[[1]]$content

# convertendo para letras minusculas
corpus_norm <- tm_map(corpus_norm, content_transformer(tolower))
corpus_norm[[1]]$content


##################################################################
# Representacao e Armazenamento de Documentos
##################################################################


##################################################################
# Matriz Termo-Documento - Exemplo
# carregando a biblioteca
library(tm)

# preparando os documentos e corpus
chico_txt <- c("Nao sei", "so sei que foi assim")
chico_crps <- Corpus(VectorSource(chico_txt))

# criando matriz termo-documento
chico_tdm <- TermDocumentMatrix(chico_crps)

# visualizando a matriz termo-documento
inspect(chico_tdm)

# carregando a base de dados
data(acq)

# computando a matriz termo-documento e aplicando o processamento 
# nos dados: remocao de pontuacao e stopwords
tdm <- TermDocumentMatrix(acq, 
            control = list(removePunctuation = TRUE, 
                           stopwords = TRUE, 
                           weighting = weightBin))

# visualizando a matriz termo-documento
inspect(tdm)

##################################################################
# indice Invertido - Exemplo 
# importando os pacotes
library(tm)
library(corpus)

# criando os documentos
documents <- c("To be or not to be. I am what I am.", 
               "To do is to be. To be is to do.", 
               "I think therefore I am. Let it be.")

# criando o corpus
corpus <- Corpus(VectorSource(documents))

# convertendo para letras minusculas
text <- tm_map(corpus, content_transformer(tolower))

# removendo pontuacao
text <- tm_map(text, removePunctuation)

# transformando documentos em lista de tokens
docs <- text_tokens(text); docs

# extraindo vocabulario
vocab <- sort(unique(unlist(docs))); vocab

# criando lista de documentos que contem cada termo
index <- sapply(vocab, function(x) which(sapply(docs,
                              function(y) is.element(x, y))))

# organizando em um data.frame
index = data.frame(docs = I(index))
index

# # Exemplo mais simples do indice invertido (extra)
# # criando um data frame vazio para guardar o indice
# index <- data.frame(term = character(), docs = numeric())
# # para cada termo do vocabulario 
# for (t in c(1:length(vocab))) {
#   line <- c()
#   # verificamos a quais documentos o termo pertence 
#   for (d in c(1:length(docs))) {
#     if (is.element(vocab[t], docs[[d]])) {
#       line<-append(line, d)
#     }
#   }
#   # adicionando uma nova linha ao data.frame
#   line <- data.frame(term = vocab[t], docs = I(list(line)))
#   index <- rbind(index, line)
# }

# # visualizando o indice invertido criado
# index

##################################################################
# Modelos de Recuperacao de Texto
##################################################################


##################################################################
# Modelo de Espaco Vetorial - Exemplo 
# carregando os pacotes
library(tm)
library(proxy)

# vetores dos termos Jealous e Gossip (consultas)
jealous <- c(0, 1)
gossip <- c(1, 0)  

# definindo vetores dos livros de acordo com os termos 
# Jealous e Gossip
SaS <- c(2, 10)
PaP <- c(0, 7)
WH <- c(6, 11)

# criando uma matriz com os vetores anteriores
matriz_vetores <- rbind(jealous, gossip, SaS, PaP, WH)
matriz_vetores

# calculando a similaridade entre todos os documentos e consultas
similaridades <- simil(matriz_vetores, "cosine")
similaridades

##################################################################
# tf - Exemplo
# carregando os pacotes
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)

# carregando livros Jane Austin
book_words <- austen_books() 

# obtendo tokens por livro
book_words <- unnest_tokens(book_words, word, text) 

# contando a frequencia dos tokens em cada livro
book_words <- count(book_words, book, word, sort = TRUE)

# agrupando a frequencia dos tokens em cada categoria 
# (livro correspondente)
total_words <- group_by(book_words, book) 
total_words

# obtem numero total de palavras de cada uma das categorias
total_words <- summarize(total_words, total = sum(n))
total_words

# junta o numero de palavras com a tabela de frequencia de termos
book_words <- left_join(book_words, total_words)
book_words

# visualizando histogramas com os valores de tf no eixo x e o
# numero de termos com esse valor de tf no eixo y  
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
  labs(x = "tf") + labs(y = "numero de termos") +
  facet_wrap(~book, ncol = 2, scales = "free_y")



##################################################################
# tf-idf - Exemplo
# calculando e vincula as colunas tf, idf e tf_idf  no dataframe
book_words <- bind_tf_idf(book_words, word, book, n)

# ordenando os termos por maior tf_idf
book_words <- book_words[order(book_words$tf_idf, 
                               decreasing = TRUE),]
book_words

# visualizando os 5 termos com maior tf_idf por livro, no eixo x 
# temos os valores de tf_idf e no eixo y temos os termos
book_words %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% top_n(5) %>% ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") + coord_flip()


##################################################################
# BM25 - Exemplo
# carregando os pacotes
library(corpus)
library(udpipe)

# instanciando os  documentos
docs <- c("white audi 2.5 car", "black shoes from office",
          "new mobile iphone 7", "audi tyres audi a3",
          "nice audi bmw toyota corolla")

# vinculando titulo do documento com texto do documento
data <- tibble(doc_id = c(1:5), text = docs)

# obtendo tokens por documento
tokens <- unnest_tokens(data, word, text) 

# criando a matriz de termo-documento com frequencias
term_freq <- document_term_frequencies(tokens, term = "word")
# visualizando a matriz de termo-documento
head(term_freq)

# computando as estatisticas baseadas em frequencias: 
#   tf, idf, tf-dif, tf_bm25 e bm25
docs_stats <- document_term_frequencies_statistics(
                     term_freq, k = 1.2, b = 0.75)

# transformando em data.frame
docs_stats <- as.data.frame(docs_stats)

# visualizando a estatisticas bm25
head(docs_stats[,c("doc_id", "term", "bm25")])











