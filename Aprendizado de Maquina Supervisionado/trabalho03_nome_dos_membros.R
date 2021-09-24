#----------------------------------------------------------------#
# INF-0615 Aprendizado de Maquina Supervisionado I       
#                       
####### Código de apoio ao Trabalho 03 da disciplina INF-0615 #######
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Daniel Noriaki Kurosawa                                        
# - Eric Uyemura Suda                                       
# - Fernando Shigeru Wakabayashi                                        
# 
#----------------------------------------------------------------#

#------------------------------------------------------------------
#0 - Opções iniciais
#------------------------------------------------------------------

#setwd("/Users/nkuros/Documents/mineiracao_dados_complexos/Aprendizado de Maquina Supervisionado/Atividade 3/")
#rm(list = ls())
set.seed(42)

#------------------------------------------------------------------
#1 - Importa Bibliotecas 
#------------------------------------------------------------------
#install.packages('rpart') 
#install.packages('rpart.plot') 
#install.packages('caret') 
#install.packages('ramify') 
#install.packages('ggplot2') 
#install.packages('dplyr') 
#install.packages('hrbrthemes') 
#install.packages('rattle')
#install.packages('reshape2') 
#install.packages('ggplot2') 
#install.packages('randomForest') 

library(rpart)
library(rpart.plot)
library(caret)
library(ramify)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(rattle)
library(reshape2)
library(ggplot2)
library(randomForest)

#------------------------------------------------------------------
#2 - Funções úteis
#------------------------------------------------------------------

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposição para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    
    return(cm_relative)  
}

calcula_acc_balanceada <- function(cm_relative){
    acc_balanceada <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
    return(acc_balanceada)  
}

evaluate_model_ret_acc_relative <- function (model,df_val_or_test){
    
    model_preds <- predict(model, df_val_or_test, type = "class")
    
    cm_model_preds <- confusionMatrix(data = as.factor(model_preds), 
                                      reference = as.factor(df_val_or_test$label), 
                                      positive='forgery')
    cm_model_preds_t = t(cm_model_preds$table)
    print("MATRIZ DE CONFUSAO")
    print(cm_model_preds_t)
    print('___________________________________')
    
    
    cm_model_relative <- calculaMatrizConfusaoRelativa(cm_model_preds)
    print("MATRIZ DE CONFUSAO RELATIVA")
    print(cm_model_relative)
    
    
    acc_relativa_models <- calcula_acc_balanceada(cm_model_relative)
    
    print(acc_relativa_models)
    
    return(acc_relativa_models)
    
}

ret_feature_importances <- function(tree_model){
    importance_per_features <- tree_model$variable.importance
    relative_importance <- tree_model$variable.importance/sum(importance_per_features)
    return(relative_importance)
}








#------------------------------------------------------------------
# 3- Leitura da base de treinamento+validacao
#------------------------------------------------------------------
data <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

#3.1 - verificando os dados 
head(data)

colnames(data)

dim(data)

summary(data)


#3.2 - Removendo duplicatas
data <- unique(data)

summary(data)

dim(data)

#3.3 - Resetando índice das colunas
row.names(data) <- NULL

#3.4 - verificando desbalanceamento
table(data$label)

#------------------------------------------------------------------
# 4- Separação Treino / Validação
#------------------------------------------------------------------

# 4.1 - Separação randômica 
randomTrainValIndexes <- sample(1:nrow(data), size=0.8*nrow(data))

# 4.2 - Criação dos dataframes de Treino e validação
dataTrain <- data[randomTrainValIndexes, ]
dim(dataTrain)
dataVal  <- data[-randomTrainValIndexes, ] 
dim(dataVal)
# 4.3 - Verificando proporção dos labels
#train
table(dataTrain$label)/length(dataTrain$label)
#validation
table(dataVal$label)/length(dataVal$label)


#------------------------------------------------------------------
# 5- Treinando Baseline - Árvore de decisão
#------------------------------------------------------------------

#5.1 - criando modelo
treeModel_baseline <- rpart(formula=label ~ ., 
                            data=dataTrain,
                            method="class",
                            control=rpart.control(minsplit=2,
                                                  cp=0.0,
                                                  xval = 0),
                            parms= list(split="information"))
#5.2- verificando status
#printcp(treeModel)
#summary(treeModel)
#prp(treeModel)

#5.3 - importancia das variáveis
importance_per_features <- treeModel_baseline$variable.importance
#importance_per_features
relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance

#5.4 - EDA (opcional) das variáveis mais apontadas como importantes
table(dataTrain$label)

#5.4.1- date_death_or_discharge
p_date_death_or_discharge <- data %>%
    ggplot( aes(x=data$date_death_or_discharge, fill=label)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080", "503060")) +
    theme_ipsum() +
    labs(fill="")
p_date_death_or_discharge

#5.4.2- longitude
p_longitude <- data %>%
    ggplot( aes(x=data$longitude, fill=label)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080", "503060")) +
    theme_ipsum() +
    labs(fill="")
p_longitude

#5.4.3 - date_admission_hospital
p_date_admission_hospital <- data %>%
    ggplot( aes(x=data$date_admission_hospital, fill=label)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080", "503060")) +
    theme_ipsum() +
    labs(fill="")
p_date_admission_hospital

#5.4.4 - country
table(data$country, data$label)

#5.5 - Avaliação do Baseline

#5.5.1 - Criação das previsões do modelo
val_pred_baseline <- predict(treeModel_baseline, dataVal, type = "class")
#5.5.1 - Matriz de confusão 
cm_baseline <- confusionMatrix(data = as.factor(val_pred_baseline), 
                               reference = as.factor(dataVal$label), 
                               positive='forgery')
cm_baseline

#5.5.2 - Matriz de confusão relativa
cm_baseline_relative <- calculaMatrizConfusaoRelativa(cm_baseline)
cm_baseline_relative

#5.5.3 - Acurácia Balanceada
acc_baseline_balanceada <-calcula_acc_balanceada(cm_baseline_relative)
acc_baseline_balanceada



#------------------------------------------------------------------
# 6- Balanceamento
#------------------------------------------------------------------

#6.1 - Balanceamento por pesos 
###### OBSERVACAO ULTRA IMPORTANTE
###### VERIFICAR SE ESTA CORRETO O BALANCEAMENTO 

#6.1.1 - Criação dos pesos

label_frequency = table(dataTrain$label)
label_frequency 
relative_label_frequency =label_frequency/sum(label_frequency)
relative_label_frequency
#6.1.2 - Criação do vetor dos pesos
weights <- rep(0.0, dim(dataTrain)[1])

weight_dead = 1-relative_label_frequency[2]-relative_label_frequency[3]
weight_onTreatment = 1-relative_label_frequency[1]-relative_label_frequency[3]
weight_recovered = 1-relative_label_frequency[1]-relative_label_frequency[2]

weights[dataTrain$label == 'dead'] = weight_dead
weights[dataTrain$label == 'onTreatment'] = weight_onTreatment
weights[dataTrain$label == 'recovered'] = weight_recovered

#6.1.2 - modelo "baseline" com pesos

treeModel_w_baseline <- rpart(formula=label ~ ., 
                              data=dataTrain,
                              method="class",
                              weights = weights,
                              control=rpart.control(minsplit=2,
                                                    cp=0.0,
                                                    xval = 0),
                              parms= list(split="information"))

evaluate_model_ret_acc_relative(treeModel_w_baseline, dataVal)


#6.2 - Balanceamento por Undersampling
###### OBSERVACAO ULTRA IMPORTANTE
###### VERIFICAR SE ESTA CORRETO O BALANCEAMENTO 

#6.2.1  - Verificando a menor quantidade dentre as labels
table(dataTrain$label)

dataTrain_dead <- dataTrain[dataTrain$label == 'dead',]
dataTrain_onTreatment <- dataTrain[dataTrain$label == 'onTreatment',]
dataTrain_recovered <-dataTrain[dataTrain$label == 'recovered',]

lowest_samples <- min (nrow(dataTrain_dead), nrow(dataTrain_onTreatment), nrow(dataTrain_recovered) )
lowest_samples
#por praticidade de ajuste
nsamples <- lowest_samples

#6.2.2 - Selecinoando os indices
Idx_dead <- sample(1:nrow(dataTrain_dead), nsamples, replace = FALSE)
Idx_onTreatment <- sample(1:nrow(dataTrain_onTreatment), nsamples, replace = FALSE)
Idx_recovered <- sample(1:nrow(dataTrain_recovered), nsamples, replace = FALSE)

#6.2.3 - Criando o SubDataset
subsetDataTrain <- rbind (dataTrain_dead[Idx_dead,],
                          dataTrain_onTreatment[Idx_onTreatment,],
                          dataTrain_recovered[Idx_recovered,])
table(subsetDataTrain$label)

#6.1.4 - modelo "baseline" pelo subset

treeModel_undersampling_baseline <- rpart(formula=label ~ ., 
                                          data=subsetDataTrain,
                                          method="class",
                                          control=rpart.control(minsplit=2,
                                                                cp=0.0,
                                                                xval = 0),
                                          parms= list(split="information"))

evaluate_model_ret_acc_relative(treeModel_undersampling_baseline, dataVal)



#------------------------------------------------------------------
# 7- Variação do tamanho das árvores
#------------------------------------------------------------------

#Criaçao do Data.Frame para salvar os valores de acuracia
accPerDepth <- data.frame(depth=numeric(15), accTrain=numeric(15), accVal=numeric(15))
summary(accPerDepth)

#for para treinar árvores com tamanhos variádos (só depth nesse caso)
#utilizando base c/ undersampling
for (maxDepth in 1:25){
    treeModel <- rpart(formula=label ~ .,
                       data=subsetDataTrain,
                       method="class",
                       control=rpart.control(minsplit=2,
                                             cp=0.0, 
                                             maxdepth=maxDepth,
                                             xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(treeModel, subsetDataTrain, type="class")
    
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(subsetDataTrain$label), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    
    acc_bal_train <- calcula_acc_balanceada(cm_train_relative)
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, dataVal, type="class")
    
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    
    acc_bal_val <- calcula_acc_balanceada(cm_val_relative)
    
    accPerDepth[maxDepth,] = c(maxDepth, 
                               acc_bal_train, 
                               acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

#nesse caso esta entre 6 e 7

#------------------------------------------------------------------
# 8- Exploração do Subconjunto de features
#------------------------------------------------------------------

#### ALGUEM FAZ UM FOR PRA FAZER UMA MISTURA DE VARIAVEIS *

relative_importance
#8.1 - Subset 1
subset_1 = subsetDataTrain[, c('date_death_or_discharge',
                               'age',
                               'sex',
                               'travel_history_dates',
                               'country',
                               'date_admission_hospital',
                               'label')]
treeModel_subset_1 <- rpart(formula=label ~ .,
                            data=subset_1,
                            method="class",
                            control=rpart.control(minsplit=2,
                                                  cp=0.0, 
                                                  maxdepth=7,
                                                  xval = 0),
                            parms= list(split="information"))


evaluate_model_ret_acc_relative(treeModel_subset_1, dataVal)
ret_feature_importances(treeModel_subset_1)

#8.2 - Subset 2
subset_2 = subsetDataTrain[, c('date_death_or_discharge',
                               'age',
                               'longitude',
                               'latitude',
                               'date_admission_hospital'
                               ,'label')]


treeModel_subset_2 <- rpart(formula=label ~ .,
                            data=subset_2,
                            method="class",
                            control=rpart.control(minsplit=2,
                                                  cp=0.0, 
                                                  maxdepth=7,
                                                  xval = 0),
                            parms= list(split="information"))

evaluate_model_ret_acc_relative(treeModel_subset_2, dataVal)

ret_feature_importances(treeModel_subset_2)

#8.2 - Subset 3

subset_3 = subsetDataTrain[, c('age',
                               'travel_history_dates',
                               'label')] 

treeModel_subset_3 <- rpart(formula=label ~ .,
                     data=subset_3,
                     method="class",
                     control=rpart.control(minsplit=2,
                                           cp=0.0, 
                                           maxdepth=7,
                                           xval = 0),
                     parms= list(split="information"))

evaluate_model_ret_acc_relative(treeModel_subset_3, dataVal)

ret_feature_importances(treeModel_subset_3)
# Leitura da base de Teste. Descomentem as linhas abaixo quando o 
# conjunto de teste estiver disponível.

#test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T) # Descomentar

# As duas linhas abaixo são um trick para corrigir os "levels" na
# coluna country. Ele apenas adiciona 1 exemplo de treino na primeira
# linha do teste e depois retira-o para obter o test_set original. 
# Nao se preocupem, eh apenas para nivelamento interno do R. 
# Certifiquem-se de executar os comandos na seguinte ordem:
# linha 38, linha 47 e linha 48 quando a base de teste estiver disponivel

#temporary_test <- rbind(train_val_set[1,], test_set) # Descomentar
#test_set <- temporary_test[-1,] # Descomentar
