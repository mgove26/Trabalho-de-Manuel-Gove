#Projecto_limpeza_de_dados
#Manuel Gove

#vamos instalar os pacotes
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")


#Carregar os pacotes instalados(bibliotecas)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Importacao de Daos
dados <- read.csv("C:/Users/Manuel Gove/Downloads/dados_sujos_limpeza.csv")
View(dados)

#Idetificar valores ausentes
is.na(dados)
colSums(is.na(dados))

#Eliminar dados duplicados
sum(duplicated(dados))
dados <- dados[!duplicated(dados), ]
sum(duplicated(dados))

# Corrigir espacos
dados$nome_cliente <- trimws(dados$nome_cliente)
dados$cidade <- trimws(dados$cidade)
dados$produto <- trimws(dados$produto)
dados$nome_cliente <- tools::toTitleCase(tolower(dados$nome_cliente))
dados$cidade <- tools::toTitleCase(tolower(dados$cidade))
dados$produto <- tools::toTitleCase(tolower(dados$produto))


#Identificar outliers
summary(dados$idade)
boxplot(dados$idade)

# Corrigir tipos de dados

str(dados)

# Validar emails

dados$email <- trimws(dados$email)
validar_email <- grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",dados$email)

dados$email[!validar_email & !is.na(dados$email)]

# Guardar os dados limpados

head(dados)
View(dados)
getwd()
write.csv(dados,"dados_limpos.csv",row.names = FALSE,na = "", fileEncoding = "UTF-8")

#Confirmar se o ficheiro foi criado
file.exists("dados_limpos.csv")
