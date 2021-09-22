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

library(rpart)
library(rpart.plot)
library(caret)
library(ramify)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
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
