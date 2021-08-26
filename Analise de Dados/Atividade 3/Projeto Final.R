#
# Codigo - Projeto Final analise de dados
#

# Limpando espaco de trabalho
rm(list=ls())
graphics.off()

# Importanto bibliotecas
library(ggplot2)
library(dbplyr)

# --------------------
# 1) Importando tabela
# --------------------

csv_url <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

col_names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.table(csv_url,header=FALSE, fill=TRUE, sep=';', col.names=col_names)

cepagri_aux[as.Date(cepagri$horario)=="2015-01-21",]
# Arquivo csv nao tem cabecalho, input manual do nome das colunas
# e varias colunas marcadas com [ERRO] (header=FALASE e fill=TRUE)

# -----------------
# 2) Sumario basico
# -----------------
# Primeiro sumario apenas para ver como estao as variaveis em questao de classe e preenchimento
summary(cepagri)

# --------------------------------
# 3) Ajustando Formato das Colunas
# --------------------------------

# Transformando a coluna de string para horas
cepagri$horario <- as.POSIXct(cepagri$horario,
                              format = '%d/%m/%Y-%H:%M')

# Substituindo coluna de temperatura com '[ERRO]' para NA
# e transformando para numerico
cepagri[grep('[A-Z]+',cepagri$temp),]$temp <- NA

# Antes da retirada de NA 
# n = 387.846
length(cepagri$horario)

# Avaliacao de quantos registros nulos existem no dataset
datas_na <- cepagri[is.na(cepagri$temp),]
table(as.Date(datas_na$horario))
# Quantidade de registros marcados com ERRO desde 2014
# n = 31.305
nrow(datas_na)

cepagri$temp <- as.numeric(cepagri$temp)

# --------
# 4) Delimitando janela temporal do estudo
# -----

inicio <- as.Date('2015-01-02')
fim <- as.Date('2020-12-31')
janela <- (as.Date(cepagri$horario) >= inicio & as.Date(cepagri$horario) <= fim)

cepagri_janela <- cepagri[janela,]

nrow(cepagri)
# Quantidade de linhas apos o filtro por data
# n = 311.917
nrow(cepagri_janela)

# -------------------------
# 5) Tratamento de Outliers
# -------------------------

# Validacao da temperatura
# Minimos e maximos estao OK 5-28 graus
summary(cepagri_janela$temp)

# Validacao do vento
# Valores Ok (existem noticias que indicam ventos de ate 143k)
summary(cepagri_janela$vento)

# Validacao do vento
# Valores Ok vai ate 100%
summary(cepagri_janela$umid)

# Validacao sensacao termica
# Valores estranhos de 99 graus de sensacao termica
stats <- summary(cepagri_janela$sensa)

head(cepagri_janela[(cepagri_janela$sensa < 0) & !(is.na(cepagri_janela$sensa)),])
tail(cepagri_janela[(cepagri_janela$sensa > 90) & !(is.na(cepagri_janela$sensa)),])

# Pelo plot podemos observar que ha valores marcados no final do plot que parecem erros
ggplot(cepagri_janela, aes(x=sensa, y= ..density..)) + geom_histogram(color='White', bins=100) + geom_density()

# iqr <- IQR(cepagri_janela$sensa, na.rm=TRUE)
# sensa_max <- stats[2]

# Aplicando NA para os valores de sensacao termica = 99.9 por ser um possivel erro sistemico
cepagri_janela[cepagri_janela$sensa == 99.9 & is.na(cepagri_janela$sensa) == FALSE, ]$sensa <- NA
summary(cepagri_janela$sensa)

# ----------------------------------
# 5) Remocao de Valores NA
# ----------------------------------
cepagri_janela$data_lt <- as.POSIXlt(cepagri_janela$horario)
cepagri_janela$ano <- unclass(cepagri_janela$data_lt)$year + 1900
cepagri_janela$mes <- unclass(cepagri_janela$data_lt)$mon + 1

# Verificacao visual dos NAs
table <- cepagri_janela %>% group_by(ano, mes) %>% summarise(count_na=sum(is.na(temp))) 
ggplot(table, aes(x=mes, y=ano, fill=count_na)) + 
  geom_tile() +
  scale_fill_gradient(low="navy", high="red")

cepagri_aux <- cepagri_janela[(is.na(cepagri_janela$temp)==FALSE) & (is.na(cepagri_janela$sensa)==FALSE), ]
cepagri_aux2 <- cepagri_janela[(is.na(cepagri_janela$temp)==FALSE) , ]
# summary(cepagri_aux2)

# Verificacao visual apos a remocao dos NAs
table2 <- cepagri_aux %>% group_by(ano, mes) %>% summarise(count=length(temp)) 
ggplot(table2, aes(x=mes, y=ano, fill=count_na)) + 
  geom_tile() +
  scale_fill_gradient(low="red", high="navy")

# n=22.085 registros NA retirados da base
# Equivalente a 7% da base
na_s <- nrow(cepagri_janela[(is.na(cepagri_janela$temp)==TRUE) | (is.na(cepagri_janela$sensa)==TRUE), ])
tot <-nrow(cepagri_janela)
na_s/tot

# ----------------------------------
# 6) Tratamento de Valores Repetidos
# ----------------------------------
consecutive <- function(vector, k=1){
  n <- length(vector)
  result <- logical(n)
  
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  return(result)
}
# Remocao de valores que se repetem por mais de 24 horas

any(consecutive(cepagri_aux$temp, 144))
filtro <- consecutive(cepagri_aux$temp, 144)

# cepagri_aux[as.Date(cepagri_aux$horario)==as.Date("2015-11-16"), ]

cepagri_aux[filtro, ]

filtrado <- cepagri_aux[filtro, ]

as.Date(filtrado$horario)
