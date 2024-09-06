
!pip install nltk
!pip install transformers
!pip install unidecode
!pip install pandas==1.5.3
!pip install textnets
!pip install textnets --upgrade

from unidecode import unidecode
import pandas as pd
import nltk
import re
from nltk.stem import WordNetLemmatizer
from collections import Counter
import gspread

base = pd.read_csv('/content/drive/MyDrive/FinFluence VI/ANBIMA _ FinFluence VI _ Base Geral_v2 (19.01.24).csv')

##Coluna que contém a transcrição
coluna_texto='conteudo'
##Coluna que contém o id do vídeo
coluna_id='nome_influenciador_global'

df=base[[coluna_texto,coluna_id,'servico']]
df

##Leitura da aba "lexique_iramuteq" na planilha "https://docs.google.com/spreadsheets/d/1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88"
##Essa aba possui os dados utilizados para efetuar a lemmatização do conteúdo de forma similar ao iramuteq

chave_planilha1 = '1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88'
aba_planilha1 = 'lexique_iramuteq'
wks1 = gc.open_by_key(chave_planilha1).worksheet(aba_planilha1)
df_lexique = pd.DataFrame(wks1.get_all_records())
df_lexique

##Leitura da aba "stopwords - não apagar nenhum" na planilha "https://docs.google.com/spreadsheets/d/1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88"
##Essa aba possui os dados utilizados para efetuar a remoção de palavras indesejas na coluna de texto

chave_planilha1 = '1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88'
aba_planilha1 = 'stopwords - não apagar nenhum'
wks1 = gc.open_by_key(chave_planilha1).worksheet(aba_planilha1)
df_stopwords = pd.DataFrame(wks1.get_all_records())
lista_df_stopwords=list(df_stopwords['stopwords'])
df_stopwords

df[coluna_texto] = df[coluna_texto].astype(str)


##Remove caracteres especiais e strings que representam horas da coluna de texto
##Além disso, deixa o conteúdo minusculo e sem acentuação

def limpar_texto(text):
    text=text.strip()
    text = re.sub(r'[^\w\s]', '', text)
    text = re.sub(r'\b(?:\d{1,2}(?::\d{1,2})?(?::\d{1,2})?(?:h|min|m|s|hr|hrs|hora|horas|minuto|minutos)?)\b', '', text)
    text = re.sub(r'\b((\d{2})(h|hr|hrs|hs)(\d{2})(m|min)?)\b', '', text)
    text = re.sub(r'\b((\d{2})(h|hr|hrs)(\d{2})(m|min)(\d{2})(s|seg)?)\b', '', text)
    text = text.lower()
    text = unidecode(text)
    return text
df[coluna_texto] = df[coluna_texto].apply(limpar_texto)
df

##Lemmatização da coluna conteudo_completo

custom_lemmatization = dict(zip(df_lexique["palavra"], df_lexique["palavra_lemmatizada"]))

def lemmatize_with_custom_lemmatization(text, custom_lemmatization):
    lemmatizer = WordNetLemmatizer()
    words = nltk.word_tokenize(text)
    lemmatized_words = [custom_lemmatization.get(word, lemmatizer.lemmatize(word)) for word in words]
    return ' '.join(lemmatized_words)
df[coluna_texto] = df[coluna_texto].apply(lambda x: lemmatize_with_custom_lemmatization(x, custom_lemmatization))
df


##Exportação de tokens significativos (palavras que "sobram" após a remoção de stopwords)

lista_df_stopwords=list(df_stopwords['stopwords'].unique())

#nltk.download('punkt')
def filtrar_palavras_significativas(texto):
    palavras = nltk.word_tokenize(texto)
    palavras_filtradas = []
    for palavra in palavras:
        if palavra not in lista_df_stopwords:
            palavras_filtradas.append(palavra)
    return palavras_filtradas

lista_de_palavras = list(df[coluna_texto].apply(filtrar_palavras_significativas))
palavras_unicas = [palavra for sublist in lista_de_palavras for palavra in sublist]
palavras_final = list(set(palavras_unicas))
palavras_final = sorted(palavras_final)

#Conta frequência de tokens significativos e ordena do maior para o menor

contador_palavras = Counter()


for texto in df[coluna_texto]:
    palavras_no_texto = texto.split()
    contador_palavras.update(palavras_no_texto)

frequencia_desejada = {palavra: contador_palavras[palavra] for palavra in palavras_final}

df_frequencia_tokens = pd.DataFrame({'tokens_significativos': frequencia_desejada.keys(),
                              'frequencia': frequencia_desejada.values()})

df_frequencia_tokens = df_frequencia_tokens.sort_values(by='frequencia', ascending=False)

# **Lemmatização da coluna de texto - PARTE II**

##Download de Tokens Significativos na planilha "https://docs.google.com/spreadsheets/d/1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88"

chave_planilha1 = '1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88'
aba_planilha1 = 'Tokens Significativos'
wks1 = gc.open_by_key(chave_planilha1).worksheet(aba_planilha1)
df_tokens_significativos = pd.DataFrame(wks1.get_all_records())
lista_df_tokens_significativos=list(df_tokens_significativos['tokens_significativos'].unique())


set_tokens_significativos = set(lista_df_tokens_significativos)

def processar_texto(texto):
    palavras = texto.split()  # Dividir o texto em palavras individuais
    palavras_interessantes = [palavra for palavra in palavras if palavra in set_tokens_significativos]
    return ' '.join(palavras_interessantes)


# Aplicar a função à coluna de texto
df[coluna_texto] = df[coluna_texto].apply(processar_texto)
df.to_csv('base_geral_lemmatizada.csv', index=False)

chave_planilha1 = '1QfLyLAHietEB_GgFh2oEYmPzIehhcg5hR1yyQaynF88'
aba_planilha1 = 'Tokens Significativos'
wks1 = gc.open_by_key(chave_planilha1).worksheet(aba_planilha1)
df_tokens_significativos = pd.DataFrame(wks1.get_all_records())
lista_df_tokens_significativos=list(df_tokens_significativos['tokens_significativos'].unique())

set_tokens_significativos = set(lista_df_tokens_significativos)

def processar_texto(texto):
    palavras = texto.split()  # Dividir o texto em palavras individuais
    palavras_interessantes = [palavra for palavra in palavras if palavra in set_tokens_significativos]
    return ' '.join(palavras_interessantes)


# Aplicar a função à coluna de texto
df[coluna_texto] = df[coluna_texto].apply(processar_texto)
df
