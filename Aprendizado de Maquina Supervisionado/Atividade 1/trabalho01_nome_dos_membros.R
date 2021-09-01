#----------------------------------------------------------------#
# INF-0615 Aprendizado de Maquina Supervisionado I       
#                       
# Trabalho Avaliativo 1 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Daniel Noriaki Kurosawa                                        
# - Eric Uyemura Suda                                       
# - Fernando Shigeru Wakabayashi                                        
# 
#----------------------------------------------------------------#


# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta fun??o escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles n?o s?o elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que voc? deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exerc?cios 02 e 03 para ver o funcionamento 
# de uma funcao similar a essa.


getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao
rm(list=ls())
graphics.off()

train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

# install.packages(ggplot2)
# install.packages(dplyr)
# install.packages(reshape2)

library(ggplot2)
library(dplyr)
library(reshape2)

#### Tarefas

# 1.
########
# Validacao se os conjuntos sao disjuntos

merge(train_set, val_set)
# merge(train_set, test_set)
# merge(test_set, val_set)

# Validacao de volumetria das bases
summary(train_set)
dim(train_set)
colnames(train_set)

summary(val_set)
dim(val_set)
colnames(val_set)

#########
# Verificacao da categoria marcada como (OTHER) no summary
tapply(train_set$No, train_set$wd, length)

#########
# Visualizacao da distribuição das variaveis
melt_train_set <- train_set[, -which(names(train_set) %in% c('No', 'wd', 'year', 'month', 'day'))]
melt_train_set <- melt(melt_train_set)

ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free')

#########
# Problema com a variavel RAIN, quase todos os registros tem valor 0
ggplot(train_set, aes(x=RAIN)) + 
    geom_histogram(color='White', bins=10) +
    stat_bin(aes(label=..count..))

tapply(train_set$No, train_set$RAIN, length)

#########
# Retiraremos a priori as variaveis No, RAIN e wd para gerar nosso caso BASELINE
train_set_clean <- train_set[, -which(names(train_set) %in% c('No', 'wd', 'RAIN'))]
val_set_clean <- val_set[, -which(names(train_set) %in% c('No', 'wd', 'RAIN'))]

#########
# Devido a distribuicao nao normal dos dados iremos aplicar uma normalizacao min-max
# exceto para as variaveis de data e a variavel target
min_features <- apply(train_set_clean[,6:ncol(train_set_clean)-1], 2, min); min_features

max_features <- apply(train_set_clean[,6:ncol(train_set_clean)-1], 2, max); max_features

diff <- max_features - min_features; diff

train_set_clean[,6:ncol(train_set_clean)-1] <- sweep(train_set_clean[,6:ncol(train_set_clean)-1], 2, min_features, "-")
train_set_clean[,6:ncol(train_set_clean)-1] <- sweep(train_set_clean[,6:ncol(train_set_clean)-1], 2, diff, "/")
summary(train_set_clean)

val_set_clean[,6:ncol(val_set_clean)] <- sweep(val_set_clean[,6:ncol(val_set_clean)], 2, min_features, "-")
val_set_clean[,6:ncol(val_set_clean)] <- sweep(val_set_clean[,6:ncol(val_set_clean)], 2, diff, "/")
summary(val_set_clean)

# testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, min_features, "-")
# testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, diff, "/")
# summary(testSet)

########
# Visualizacao dos dados apos normalizacao min max (Visualmente nao alterou muito a distribuicao)
melt_train_set <- train_set_clean[, -which(names(train_set_clean) %in% c('No', 'wd', 'year', 'month', 'day'))]
melt_train_set <- melt(melt_train_set)

ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free')



# Descomente a linha abaixo apenas quando o conjunto de teste esiver dispon?vel
#test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)






