
# ==========================================
# PROJECTO: LIMPEZA E TRATAMENTO DE DADOS
# Autor: Manuel Gove
# Linguagem: R
# ==========================================

# 1. IMPORTAR OS DADOS
dados <- read.csv("dados_sujos_limpeza.csv",
                  stringsAsFactors = FALSE,
                  na.strings = c("", "NA"))

# Visualizar os primeiros registos
head(dados)

# 2. IDENTIFICAR VALORES AUSENTES
colSums(is.na(dados))

# 3. ELIMINAR REGISTOS DUPLICADOS
dados <- dados[!duplicated(dados), ]

# 4. CORRIGIR ESPAÇOS E LETRAS
dados$nome_cliente <- trimws(dados$nome_cliente)
dados$cidade <- trimws(dados$cidade)
dados$produto <- trimws(dados$produto)

dados$nome_cliente <- tools::toTitleCase(
  tolower(dados$nome_cliente)
)

dados$cidade <- tools::toTitleCase(
  tolower(dados$cidade)
)

dados$produto <- tools::toTitleCase(
  tolower(dados$produto)
)

# 5. CORRIGIR TIPOS DE DADOS
dados$idade <- as.numeric(dados$idade)
dados$quantidade <- as.numeric(dados$quantidade)
dados$preco_unitario_MZN <- as.numeric(
  gsub(",", ".", dados$preco_unitario_MZN)
)
dados$satisfacao <- as.numeric(dados$satisfacao)

# Corrigir valores inválidos
dados$idade[dados$idade < 0 | dados$idade > 100] <- NA
dados$quantidade[dados$quantidade <= 0] <- NA

# Preencher valores ausentes pela mediana
dados$idade[is.na(dados$idade)] <-
  median(dados$idade, na.rm = TRUE)

dados$quantidade[is.na(dados$quantidade)] <-
  median(dados$quantidade, na.rm = TRUE)

# 6. IDENTIFICAR E TRATAR OUTLIERS
Q1 <- quantile(dados$preco_unitario_MZN, 0.25, na.rm = TRUE)
Q3 <- quantile(dados$preco_unitario_MZN, 0.75, na.rm = TRUE)

IQR_valor <- Q3 - Q1
limite <- Q3 + 1.5 * IQR_valor

dados$preco_unitario_MZN[
  dados$preco_unitario_MZN > limite
] <- NA

dados$preco_unitario_MZN[
  is.na(dados$preco_unitario_MZN)
] <- median(dados$preco_unitario_MZN, na.rm = TRUE)

# 7. VALIDAR ENDEREÇOS DE EMAIL
dados$email <- tolower(trimws(dados$email))

padrao <- "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}$"

valido <- grepl(padrao, dados$email)

dados$email[is.na(valido) | !valido] <- NA

# 8. EXPORTAR OS DADOS LIMPOS
write.csv(dados, "dados_limpos_R.csv",
          row.names = FALSE)

# VERIFICACAO FINAL
cat("Total de registos:", nrow(dados), "\n")
print(colSums(is.na(dados)))
