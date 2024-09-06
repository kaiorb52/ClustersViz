
# Lemmatização.R

lemmatização <- function(base, coluna_texto, coluna_id)
{
  # EXEMPLO:
  #
  # reforma_tributaria_f <- reforma_tributaria |>
  #   lemmatização(coluna_texto = "texto", coluna_id = "V1")
  #

  df <- base %>% select(all_of(coluna_texto), all_of(coluna_id))

  df_lexique <- read.xlsx("dfs/Controle _ Geração de Rede de Territórios - Anbima.xlsx", sheet = 1)
  custom_lemmatization <- setNames(df_lexique$palavra_lemmatizada, df_lexique$palavra)
    # Leitura da planilha que contém o lexique para lemmatização personalizada

  df_stopwords <- read.xlsx("dfs/Controle _ Geração de Rede de Territórios - Anbima.xlsx", sheet = 2)
  stopwords <- unique(df_stopwords$stopwords)
    # Leitura da planilha de stopwords

  limpar_texto <- function(texto) {
    texto <- str_trim(texto)
    texto <- str_replace_all(texto, "[^\\w\\s]", "")
    texto <- str_replace_all(texto, "\\b\\d{1,2}(:\\d{1,2})?(h|min|m|s|hr|hrs|hora|horas|minuto|minutos)?\\b", "")
    texto <- tolower(texto)
    texto <- iconv(texto, to = "ASCII//TRANSLIT") # Remover acentuação
    return(texto)
  }

  df[[coluna_texto]] <- sapply(df[[coluna_texto]], limpar_texto)

  filtrar_palavras_significativas <- function(texto, stopwords) {
    palavras <- unlist(str_split(texto, "\\s+"))
    palavras_filtradas <- palavras[!palavras %in% stopwords]
    return(paste(palavras_filtradas, collapse = " "))
    # removedor de stopwords
  }

  df[[coluna_texto]] <- sapply(df[[coluna_texto]], filtrar_palavras_significativas, stopwords = stopwords)

  df <- rename(df, texto_lemmatizado = texto)
  df_f <- left_join(base, df, by = coluna_id)

  return(df_f)
}
