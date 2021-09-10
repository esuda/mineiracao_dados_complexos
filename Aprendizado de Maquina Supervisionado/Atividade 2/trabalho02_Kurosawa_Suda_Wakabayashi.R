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
setwd("/Users/nkuros/Documents/mineiracao_dados_complexos/Aprendizado de Maquina Supervisionado/Atividade 2/")


rm(list=ls())
graphics.off()

calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi��o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
    cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
    cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
    
    return(cm_relative)  
}

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

getLoss <- function(y_true, y_pred){
    y_true <- as.numeric(y_true) - 1
    
    totalLoss <- 0
    eps <- 1e-9
    # Recall: length(y_true) == length(y_pred)
    # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
    # eps is used for numerical stability, it is very close to 0.
    # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
    # would be 0*log2(0) + 1*log(1). It would result in NaN
    # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
    for(i in 1:length(y_true)){
        loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
        totalLoss <- totalLoss + loss
    }
    totalLoss <- totalLoss/(length(y_true))
    return(totalLoss)
}

# Comandos que leem os conjuntos de treino e de validacao

train_set <- read.csv("./proteins_training_set.csv", stringsAsFactors=TRUE)
val_set <- read.csv("./proteins_validation_set.csv", stringsAsFactors=TRUE)
# test_set <- read.csv("./test_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

# install.packages(ggplot2)
# install.packages(dplyr)
# install.packages(reshape2)

library(ggplot2)
library(dplyr)
library(reshape2)
library(glmnet)
library(caret)
library(pROC)
#### Tarefas

# 1.
########
# Validacao se os conjuntos sao disjuntos
## Ok, todos os conjuntos sao disjuntos
merge(train_set, val_set)
#merge(train_set, test_set)
#merge(test_set, val_set)

# Validacao de volumetria das bases
summary()
count(train_set[train_set$target == 0,])
count(train_set[train_set$target == 1,])
colnames(train_set)
colnames(train_set)
colSums()
#percebemos que essas variáveis denotam em conjunto, a posição da proteina
summary(train_set$end_position - train_set$start_position)
ggplot(train_set$emini,
summary(val_set)
dim(val_set)
colnames(val_set)
count(val_set[val_set$target == 0,])
count(val_set[val_set$target == 1,])

summary(test_set)
dim(test_set)
colnames(test_set)
count(test_set[test_set$target == 0,])
count(test_set[test_set$target == 1,])

#########
# Visualizacao da distribuição das variaveis
melt_train_set <- train_set[,]
melt_train_set <- melt(melt_train_set)

p <- ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free'); p


#########
# Devido a distribuicao nao normal dos dados iremos aplicar uma normalizacao min-max
# exceto para as variaveis de data e a variavel target
min_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, min); min_features
max_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, max); max_features

diff <- max_features - min_features; diff

train_set[,1:10] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, min_features, "-")
train_set[,1:10] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, diff, "/")
summary(train_set)

val_set[,1:10] <- sweep(val_set[,1:1(ncol(val_set)-1)], 2, min_features, "-")
val_set[,1:10] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, diff, "/")

#test_set[,] <- sweep(test_set[,1:(ncol(test_set)-1)], 2, min_features, "-")
#test_set[,] <- sweep(test_set[,1:(ncol(test_set)-1)], 2, diff, "/")

train_set$target <- as.factor(train_set$target)
val_set$target <- as.factor(val_set$target)
test_set$target <- as.factor(test_set$target)
head(train_set)
########
# Visualizacao dos dados apos normalizacao min max (Visualmente nao alterou muito a distribuicao)
melt_train_set <- val_set[,]
melt_train_set <- melt(melt_train_set)

ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free')



## Baseline ##
# Treinando o Modelo - Classificação #

feature_names <- colnames(train_set)[1:(ncol(train_set)-1)]
feature_names

#gerando uma função com features a primeira potência para regressão logística
hypothesis <- getHypothesis(feature_names, 1)
hypothesis

#help(glmnet) <- doc da regressão logística

x_train <- model.matrix(hypothesis, train_set)
y_train <- train_set$target


model <- glmnet(x_train,
                y_train, 
                family="binomial", #descrição para regressão logística
                standardize = FALSE,
                maxit = 1e+05,
                alpha=0, #tipo de regularização L2, 1 = l1
                lambda = 1e-2
                )

### Verificando os thetas aprendidos ###
model$beta
model$a0 # valor do theta0 (intercept)

#Previsões

trainPred <- predict(model, newx = x_train, type="response")
head(trainPred)


#Convertendo para classes
trainClassPred <- trainPred

#Modificando para Limiar 0.5 , retorno de 0 ou 1
threshold <- 0.5
trainClassPred[trainPred >= threshold] <- 1
trainClassPred[trainPred < threshold] <- 0

trainClassPred

#Valor de loss
lossN = getLoss(train_set$target[train_set$target == 0], trainPred[train_set$target == 0])
lossP = getLoss(train_set$target[train_set$target == 1], trainPred[train_set$target == 1])
lossN
lossP
(lossN+lossP)/2


#Matriz de confusão 

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(train_set$target), 
                      positive='1')
cm

cm$table


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

### Predicao no conjunto de validacao ###
x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(model, newx = x_val, type="response")

#valPred

#converting to class
valClassPred <- valPred


#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0
#matriz transposta relativa
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative


acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline

# ROC Curve for baseline
ROC <- roc(val_set$target, val_set[,1], direction="<")
ROC



####################
#### TO-DO
#### 1- Combinação de variáveis
#### 2- Balanceamento
#### 3- Regressão logística Regularização


baseline <- lm(formula=hypothesis, data=train_set)

valPred <- predict(baseline, val_set)
trainPred <- predict(baseline, train_set)

MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

R2 <- function(pred, true){
    rss <- sum((pred - true) ^ 2)
    tss <- sum((true - mean(true)) ^ 2)
    r2 <- 1 - rss/tss
    return(r2)
}

mae_train_baseline <- MAE(trainPred, train_set$target); mae_train_baseline
mse_train_baseline <- MSE(trainPred, train_set$target); mse_train_baseline
r2_train_baseline <- R2(trainPred, train_set$target);r2_train_baseline


mae_val_baseline <- MAE(valPred, val_set$target);mae_val_baseline
mse_val_baseline <- MSE(valPred, val_set$target);mse_val_baseline
r2_val_baseline <- R2(valPred, val_set$target);r2_val_baseline


# Retirando RAIN

train_set <- train_set[, -which(names(train_set) %in% c('RAIN'))];colnames(train_set)
val_set <- val_set[, -which(names(val_set) %in% c('RAIN'))]
test_set <- test_set[, -which(names(test_set) %in% c('RAIN'))]
feature_names <- colnames(train_set[, -which(names(train_set) %in% not_include)]);feature_names

########
# Criacao de modelos atraves da combinacao de features
h02 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                       I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^2
               )


h03 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^3
)

h04 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^5
)

h05 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^10
)

h06 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=2)
h07 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=3)
h08 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=5)
h09 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=10)


modelsCategorical <- c(h02, h03, h04, h05,h06, h07, h08, h09)
total_mae_train_noCat <- c(length(modelsCategorical))
total_mae_val_noCat <- c(length(modelsCategorical))

total_mse_train_noCat <- c(length(modelsCategorical))
total_mse_val_noCat <- c(length(modelsCategorical))

total_r2_train_noCat <- c(length(modelsCategorical))
total_r2_val_noCat <- c(length(modelsCategorical))


i <- 1
dataTrain <- train_set
dataVal <- val_set


for(f in modelsCategorical){
    
    model <- lm(formula=f, data=dataTrain)
    
    valPred <- predict(model, dataVal)
    trainPred <- predict(model, dataTrain)
    
    mae_train <- MAE(trainPred, dataTrain$target)
    total_mae_train_noCat[i] <- mae_train
    
    mae_val <- MAE(valPred, dataVal$target)
    total_mae_val_noCat[i] <- mae_val

    mse_train <- MSE(trainPred, dataTrain$target)
    total_mse_train_noCat[i] <- mse_train
    
    mse_val <- MSE(valPred, dataVal$target)
    total_mse_val_noCat[i] <- mse_val
    
    r2_train <- R2(trainPred, dataTrain$target)
    total_r2_train_noCat[i] <- r2_train
    
    r2_val <- R2(valPred, dataVal$target)
    total_r2_val_noCat[i] <- r2_val

    
    i <- i + 1
    
}





mae_train_comb <- total_mae_train_noCat[1:4];mae_train_comb
mae_val_comb <- total_mae_val_noCat[1:4];mae_val_comb

mse_train_comb <- total_mse_train_noCat[1:4];mse_train_comb
mse_val_comb <- total_mse_val_noCat[1:4];mse_val_comb

r2_train_comb <- total_r2_train_noCat[1:4];r2_train_comb
r2_val_comb <- total_r2_val_noCat[1:4];r2_val_comb

#
mae_train_poly <- total_mae_train_noCat[5:8];mae_train_poly
mae_val_poly <- total_mae_val_noCat[5:8];mae_val_poly

mse_train_poly <- total_mse_train_noCat[5:8];mse_train_poly
mse_val_poly <- total_mse_val_noCat[5:8];mse_val_poly

r2_train_poly <- total_r2_train_noCat[5:8];r2_train_poly
r2_val_poly <- total_r2_val_noCat[5:8];r2_val_poly


models_comb <- c('h02', 'h03', 'h04', 'h05');models_comb
models_poly <- c('h06', 'h07', 'h08', 'h09');models_poly


########
# Plotando curvas de erro x complexidade MAE
#jpeg("mae.jpeg", quality = 75)
plot(mae_train_comb, xlab="Degree", ylab="Error", 
     ylim=c(280, 400), pch="+", col="orange",  xaxt="n")
points(mae_val_comb, pch="+", col="black")
lines(mae_train_comb, col="orange", lty=1)
lines(mae_val_comb, col="black", lty=1)


points(mae_train_poly, pch="*", col="blue")
points(mae_val_poly, pch="*", col="red")
lines(mae_train_poly, col="blue", lty=2)
lines(mae_val_poly, col="red", lty=2)

points(rep(mae_val_baseline, 4), pch="o", col="green")
lines(rep(mae_val_baseline, 4), col="green", lty=1)

axis(1, at=1:4, labels=c(2, 3, 5, 10), las=1)

legend(400, y=NULL,
       legend=c("Feature Combination Train", "Feature Combination Valid","Polynomials Train", "Polynomials Test","Baseline"), 
       col=c("orange","black","red","blue", "green"), lty=1, cex=0.8)
#dev.off()

####

# Plotando curvas de erro x complexidade MSE
#jpeg("mse.jpeg", quality = 75)
plot(mse_train_comb, xlab="Degree", ylab="Error", 
     ylim=c(200000, 400000),pch="+", col="orange",  xaxt="n")
points(mse_val_comb, pch="+", col="black")
lines(mse_train_comb, col="orange", lty=1)
lines(mse_val_comb, col="black", lty=1)
mse_val_comb

points(mse_train_poly, pch="*", col="blue")
points(mse_val_poly, pch="*", col="red")
lines(mse_train_poly, col="blue", lty=2)
lines(mse_val_poly, col="red", lty=2)

points(rep(mse_val_baseline, 4), pch="o", col="green")
lines(rep(mse_val_baseline, 4), col="green", lty=1)

axis(1, at=1:4, labels=c(2, 3, 5, 10), las=1)

legend(220000, y=NULL,
       legend=c("Feature Combination Train", "Feature Combination Valid","Polynomials Train", "Polynomials Test","Baseline"), 
       col=c("orange","black","red","blue", "green"), lty=1, cex=0.8)

#dev.off()

####

# Plotando curvas de erro x complexidade R2
#jpeg("r2.jpeg", quality = 75)
plot(r2_train_comb, xlab="Degree", ylab="R2", 
     ylim=c(.6, 1), pch="+", col="orange",  xaxt="n")
points(r2_val_comb, pch="+", col="black")
lines(r2_train_comb, col="orange", lty=1)
lines(r2_val_comb, col="black", lty=1)


points(r2_train_poly, pch="*", col="blue")
points(r2_val_poly, pch="*", col="red")
lines(r2_train_poly, col="blue", lty=2)
lines(r2_val_poly, col="red", lty=2)

points(rep(r2_val_baseline, 4), pch="o", col="green")
lines(rep(r2_val_baseline, 4), col="green", lty=1)

axis(1, at=1:4, labels=c(2, 3, 5, 10), las=1)

legend(0.7, y=NULL,
       legend=c("Feature Combination Train", "Feature Combination Valid","Polynomials Train", "Polynomials Test","Baseline"), 
       col=c("orange","black","red","blue", "green"), lty=1, cex=0.8)
#dev.off()


######## A PARTIR DAQUI CALCULOS DE ERRO DO TEST SET ##########
model <- lm(formula=h04, data=dataTest)

testPred <- predict(model, test_set)
mae_test <- MAE(testPred, test_set$target);mae_test

mse_test <- MSE(testPred, test_set$target);mse_test

r2_test <- R2(testPred, test_set$target);r2_test







###############################################################################################################################


mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features


sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)



valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)


testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)