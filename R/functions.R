
# functions.R

r_environment <- function(){ls(envir = .GlobalEnv)}


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


corpus_slipt <- function(df, texto, segment_size)
{

  # df <- df[!is.na(df[texto]), ]
  # df <- df %>% dplyr::filter(texto != "nan")

  corpus <- corpus(df, text_field = texto)
  corpus_split <- split_segments(corpus, segment_size = segment_size)

  # lista <- list(
  #   "corpus" = corpus,
  #   "corpus_split" = corpus_split
  # )

  return(corpus_split)

}


clusterização <- function(corpus_split, k)
{

  dtm <- dfm(tokens(corpus_split))
  dtm <- dfm_trim(dtm, min_docfreq = 50)
  res1 <- rainette(dtm, k = k, min_segment_size = 50, min_split_members = 100)

  lista <- list(
    "res1" = res1,
    "dtm" = dtm,
    "corpus_split" = corpus_split,
    "k_number" = k
    )

  return(lista)
}
