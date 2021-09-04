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
setwd("/Users/nkuros/Documents/mineiracao_dados_complexos/Aprendizado de Maquina Supervisionado/Atividade 1")

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

train_set <- read.csv("./training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("./validation_set_air_quality.csv", stringsAsFactors=TRUE)
#test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)

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

# summary(test_set)
# dim(test_set)
# colnames(test_set)

#########
# Verificacao da categoria marcada como (OTHER) na coluna wd
tapply(train_set$No, train_set$wd, length)

#########
# Visualizacao da distribuição das variaveis
melt_train_set <- train_set[, -which(names(train_set) %in% c('No', 'wd', 'year', 'month', 'day'))]
melt_train_set <- melt(melt_train_set)

p <- ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free'); p


########
# Visualizacao dos boxplots para retirada de outliers
p <- ggplot(data = melt_train_set[melt_train_set$variable!='target', ], aes(x=variable, y=value)) + 
    geom_boxplot()
p + facet_wrap( ~ variable, scales="free_y"); p


#########
# Problema com a variavel RAIN, quase todos os registros tem valor 0
ggplot(train_set, aes(x=RAIN)) + 
    geom_histogram(color='White', bins=10) +
    stat_bin(aes(label=..count..))

tapply(train_set$No, train_set$RAIN, length)

########
# Funcao para criar Onehot encoding para a variavel de direcao do vento

onehot_features <- function(dataset, feature){
    cats <- unique(dataset[, feature])
    for (cat in cats){
        dataset[cat] <- as.numeric(dataset[,feature]==cat)
    }
    return(dataset)
}
train_set <- onehot_features(train_set, 'wd')
val_set <- onehot_features(val_set, 'wd')

#########
# Retiraremos a priori as variaveis No e RAIN para gerar nosso caso BASELINE
train_set_clean <- train_set[, -which(names(train_set) %in% c('No', 'wd', 'RAIN'))]
val_set_clean <- val_set[, -which(names(train_set) %in% c('No', 'wd', 'RAIN'))]

# summary(train_set_clean)

#########
# Devido a distribuicao nao normal dos dados iremos aplicar uma normalizacao min-max
# exceto para as variaveis de data e a variavel target
min_features <- apply(train_set_clean[,5:13], 2, min); min_features

max_features <- apply(train_set_clean[,5:13], 2, max); max_features

diff <- max_features - min_features; diff

train_set_clean[,5:13] <- sweep(train_set_clean[,5:13], 2, min_features, "-")
train_set_clean[,5:13] <- sweep(train_set_clean[,5:13], 2, diff, "/")
summary(train_set_clean)

val_set_clean[,5:13] <- sweep(val_set_clean[,5:13], 2, min_features, "-")
val_set_clean[,5:13] <- sweep(val_set_clean[,5:13], 2, diff, "/")

# testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, min_features, "-")
# testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, diff, "/")
# summary(testSet)

########
# Visualizacao dos dados apos normalizacao min max (Visualmente nao alterou muito a distribuicao)
melt_train_set <- train_set_clean[, -which(names(train_set_clean) %in% c('No', 'wd', 'year', 'month', 'day', 'hour'))]
melt_train_set <- melt(melt_train_set)

ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free')


########
# Criacao do modelo Baseline

########
# Para o onehot encoding, utilizar n-1 features de flags
wd_columns <- unique(train_set$wd);wd_columns

wd_columns <- as.character(wd_columns)[1:(length(wd_columns)-1)];wd_columns

not_include <- c(wd_columns, 'target');not_include

feature_names <- colnames(train_set_clean[, -which(names(train_set_clean) %in% not_include)]);feature_names


hypothesis <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=1);hypothesis

## Baseline ##
baseline <- lm(formula=hypothesis, data=train_set_clean)

valPred <- predict(baseline, val_set_clean)
trainPred <- predict(baseline, train_set_clean)

# testPred <- predict(baseline, testSet)


MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

mae_train_baseline <- MAE(trainPred, train_set_clean$target)
mae_train_baseline

mae_val_baseline <- MAE(valPred, val_set_clean$target)
mae_val_baseline

########
# Criacao de modelos atraves da combinacao de features
h02 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE + N +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                       I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^2
               )

h03 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE + N +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^3
)

h04 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE + N +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^4
)

h05 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE + N +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^5
)

h06 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=2)
h07 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=3)
h08 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=4)
h09 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=5)
h10 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=10)

modelsCategorical <- c(h02, h03, h04, h05,h06, h07, h08, h09, h10)
total_mae_train_noCat <- c(length(modelsCategorical))
total_mae_val_noCat <- c(length(modelsCategorical))

i <- 1
for(f in modelsCategorical){
    
    dataTrain <- train_set_clean
    dataVal <- val_set_clean
    
    model <- lm(formula=f, data=dataTrain)
    
    valPred <- predict(model, dataVal)
    trainPred <- predict(model, dataTrain)
    
    mae_train <- MAE(trainPred, dataTrain$target)
    total_mae_train_noCat[i] <- mae_train
    
    mae_val <- MAE(valPred, dataVal$target)
    total_mae_val_noCat[i] <- mae_val
    i <- i + 1
    
}

summary(model)

plot(total_mae_val_noCat[5:9], xlab="Complexity", ylab="Error", 
     ylim=c(200, 500), pch="+", col="blue",  xaxt="n")

points(total_mae_val_noCat[1:4], pch="+", col="blue")

points(total_mae_train_noCat[5:9], pch="*", col="red")
points(total_mae_train_noCat[1:4], pch="*", col="red")

points(rep(mae_val_baseline, 5), pch="o", col="green")

axis(1, at=1:length(modelsCategorical), labels=seq(from = 1, to = 9, by = 1), las=1)

lines(total_mae_train_noCat[1:4], col="orange", lty=2)
lines(total_mae_val_noCat[1:4], col="black", lty=2)

lines(total_mae_train_noCat[5:9], col="red", lty=2)
lines(total_mae_val_noCat[5:9], col="blue", lty=2)

lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
legend(500, y=NULL, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.8)

########
# Criacao de modelos atraves da combinacao de polinomios



# Descomente a linha abaixo apenas quando o conjunto de teste esiver dispon?vel
#test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)






