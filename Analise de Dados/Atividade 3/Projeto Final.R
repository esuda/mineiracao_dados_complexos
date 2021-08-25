#
# Codigo - Projeto Final analise de dados
#

# Limpando espaco de trabalho
rm(list=ls())

# Importanto bibliotecas
library(ggplot2)
library(dbplyr)

# --------------------
# 1) Importando tabela
# --------------------

csv_url <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

col_names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.table(csv_url,header=FALSE, fill=TRUE, sep=';', col.names=col_names)

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
                              format = '%d/%m/%Y-%H:%M', 
                              tz = "America/Sao_Paulo")

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

inicio <- as.Date('2015-01-01')
fim <- as.Date('2020-12-31')
janela <- (as.Date(cepagri$horario) >= inicio & as.Date(cepagri$horario) <= fim)

cepagri_janela <- cepagri[janela,]

nrow(cepagri)
# Quantidade de linhas apos o filtro por data
# n = 311.917
nrow(cepagri_janela)

# --------
# 5) Tratamento de Outliers
# -----

# Validacao da temperatura
# Minimos e maximos estao OK 5-28 graus
summary(cepagri_janela$temp)

# Validacao sensacao termica
# Valores estranhos de -8 ate 99 graus
summary(cepagri_janela$sensa)

head(cepagri_janela[(cepagri_janela$sensa < 0) & !(is.na(cepagri_janela$sensa)),])
head(cepagri_janela[(cepagri_janela$sensa > 99) & !(is.na(cepagri_janela$sensa)),])

sensacao_termica <- function(v, t){
  s <- 33 + (((10*sqrt(v)) + 10.45 - v)*((t-33)/22))
  return(s)
}

sensacao_termica(78, 15)
