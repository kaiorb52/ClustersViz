
library(dplyr)
library(stringr)
library(tm)
library(readr)
library(openxlsx)
library(data.table)

base <- fread("C:/Users/kaior/Downloads/reforma_tributaria.csv")

# Selecionar as colunas necessárias
coluna_texto <- "texto"
coluna_id <- "veiculo"

df <- base %>% select(all_of(coluna_texto), all_of(coluna_id))

# Leitura da planilha que contém o lexique para lemmatização personalizada
df_lexique <- read.xlsx("dfs/Controle _ Geração de Rede de Territórios - Anbima.xlsx", sheet = 1)
custom_lemmatization <- setNames(df_lexique$palavra_lemmatizada, df_lexique$palavra)

# Leitura da planilha de stopwords
df_stopwords <- read.xlsx("dfs/Controle _ Geração de Rede de Territórios - Anbima.xlsx", sheet = 2)
stopwords <- unique(df_stopwords$stopwords)

# Função para remover caracteres especiais e padronizar o texto
limpar_texto <- function(texto) {
  texto <- str_trim(texto)
  texto <- str_replace_all(texto, "[^\\w\\s]", "")
  texto <- str_replace_all(texto, "\\b\\d{1,2}(:\\d{1,2})?(h|min|m|s|hr|hrs|hora|horas|minuto|minutos)?\\b", "")
  texto <- tolower(texto)
  texto <- iconv(texto, to = "ASCII//TRANSLIT")  # Remover acentuação
  return(texto)
}

df[[coluna_texto]] <- sapply(df[[coluna_texto]], limpar_texto)

# Função para lemmatização com base no dicionário customizado
# lemmatize_with_custom <- function(text, custom_lemmatization) {
#   words <- unlist(str_split(text, "\\s+"))
#   lemmatized_words <- sapply(words, function(word) {
#     # Usa 'get' para buscar a palavra no dicionário; se não encontrar, retorna a palavra original
#     lemmatized_word <- custom_lemmatization[word]
#     if (!is.na(lemmatized_word)) {
#       print("Processando...")
#       lemmatized_word
#     } else {
#       word
#     }
#   })
#   return(paste(lemmatized_words, collapse = " "))
# }
#
# # Aplicar a função à coluna de texto
# df[[coluna_texto]] <- sapply(df[[coluna_texto]], lemmatize_with_custom, custom_lemmatization = custom_lemmatization)

# Função para remover stopwords
filtrar_palavras_significativas <- function(texto, stopwords) {
  palavras <- unlist(str_split(texto, "\\s+"))
  palavras_filtradas <- palavras[!palavras %in% stopwords]
  return(paste(palavras_filtradas, collapse = " "))
}

df[[coluna_texto]] <- sapply(df[[coluna_texto]], filtrar_palavras_significativas, stopwords = stopwords)

View(df)
