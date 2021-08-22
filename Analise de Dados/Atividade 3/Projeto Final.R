#
# Codigo - Projeto Final analise de dados
#

# Importanto bibliotecas
library(ggplot2)

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

summary(cepagiri)

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
cepagri <- cepagri[!is.na(cepagri$temp),]
cepagri$temp <- as.numeric(cepagri$temp)
summary(cepagri)
# --------
# 4) Delimitando janela temporal
# -----

inicio <- as.POSIXct('2015-01-01',format='%Y-%m-%d')
fim <- as.POSIXct('2020-12-31',format='%Y-%m-%d')
janela <- (as.Date(cepagri$horario) >= inicio & as.Date(cepagri$horario) <= fim)

cepagri_janela <- cepagri[janela,]
summary(cepagri_janela)
count(cepagri)
count(cepagri_janela)
