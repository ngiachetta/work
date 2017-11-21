# Comparando constituições

# Pacotes
library(rvest)
library(tm)
library(stringr)
library(purrr)
library(SnowballC)
library(wordcloud)

# Scraping

## 1824
const.1824 <- "http://www.planalto.gov.br/ccivil_03/constituicao/constituicao24.htm"

const.1824 <- const.1824 %>% read_html() %>% html_nodes(xpath = '//p') %>% html_text(trim = T)

IndentifNChar.O <- function(vetor){
  nchar.0 <- c()
  for (i in 1:length(vetor)){
    if(nchar(vetor[i])==0){
      nchar.0 <- append(nchar.0, i)
    }
  }
  return(nchar.0)
}
nchar.0 <- IndentifNChar.O(const.1824)

const.1824 <- const.1824[-c(nchar.0, 2, 4, 5)]
const.1824 <- paste(const.1824, collapse = ' ')
const.1824 <- str_replace_all(const.1824, "[\r\n\t]" , "")

# Stem: Tentativa para poder comparar as constituicoes, a escrita pode ser diferente
const.1824 <- unlist(strsplit(const.1824, split = ' '))
const.1824 <- stemDocument(const.1824, language = "portuguese")
const.1824 <- paste(const.1824, collapse = ' ')

## 1891
const.1891 <- "http://www2.camara.leg.br/legin/fed/consti/1824-1899/constituicao-35081-24-fevereiro-1891-532699-publicacaooriginal-15017-pl.html"

const.1891 <- const.1891 %>% read_html()

positions <- 2:396
const.1891.v <- c()
for (i in 1:length(positions)){
  way <- '//div[2]/div/p[position]'
  XPATH <- gsub("position", positions[i], way)
  const.1891.v[i] <- const.1891 %>% html_node(xpath = XPATH) %>% html_text(trim = T)
}
const.1891 <- const.1891.v
rm(const.1891.v)

nchar.0 <- IndentifNChar.O(const.1891)

const.1891 <- const.1891[-c(nchar.0, 394, 395)]
const.1891 <- paste(const.1891, collapse = ' ')
const.1891 <- str_replace_all(const.1891, "[\r\n\t]" , "")

# Stem: Tentativa para poder comparar as constituicoes, a escrita pode ser diferente
const.1891 <- unlist(strsplit(const.1891, split = ' '))
const.1891 <- stemDocument(const.1891, language = "portuguese")
const.1891 <- paste(const.1891, collapse = ' ')

## 1934

const.1934 <- "http://www2.camara.leg.br/legin/fed/consti/1930-1939/constituicao-1934-16-julho-1934-365196-publicacaooriginal-1-pl.html"

const.1934 <- const.1934 %>% read_html()

positions <- 2:1169
const.1934.v <- c()
for (i in 1:length(positions)){
  way <- '//div[2]/div/p[position]'
  XPATH <- gsub("position", positions[i], way)
  const.1934.v[i] <- const.1934 %>% html_node(xpath = XPATH) %>% html_text(trim = T)
}
const.1934 <- const.1934.v
rm(const.1934.v)

nchar.0 <- IndentifNChar.O(const.1934)

const.1934 <- const.1934[-c(nchar.0)]
const.1934 <- paste(const.1934, collapse = ' ')
const.1934 <- str_replace_all(const.1934, "[\r\n\t]" , "")

# Stem: Tentativa para poder comparar as constituicoes, a escrita pode ser diferente
const.1934 <- unlist(strsplit(const.1934, split = ' '))
const.1934 <- stemDocument(const.1934, language = "portuguese")
const.1934 <- paste(const.1934, collapse = ' ')

## 1946

const.1946 <- "http://www2.camara.leg.br/legin/fed/consti/1940-1949/constituicao-1946-18-julho-1946-365199-publicacaooriginal-1-pl.html"

const.1946 <- const.1946 %>% read_html() %>% html_node(xpath = '//*[@class="texto"]') %>% html_text(trim = T)

const.1946 <- str_replace_all(const.1946, "[\r\n\t]" , "")
split.46 <- unlist(strsplit(const.1946, split = ' '))
# Eliminar paragrafos desnecessarios
split.46 <- split.46[-c(16926:17843)]
split.46 <- split.46[-c(1:107)]
const.1946 <- paste(split.46, collapse = ' ')

# Stem: Tentativa para poder comparar as constituicoes, a escrita pode ser diferente
const.1946 <- unlist(strsplit(const.1946, split = ' '))
const.1946 <- stemDocument(const.1946, language = "portuguese")
const.1946 <- paste(const.1946, collapse = ' ')

# Join
constituicoes <- c(const.1824, const.1891, const.1934, const.1946)

# Corpus
const.corpus <- VectorSource(constituicoes)
const.corpus <- VCorpus(const.corpus)

# Limpeza e preprocessamento de texto
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("portuguese"), "Art.", "Vide lei", "art.", "art"))
  corpus <- tm_map(corpus, removePunctuation)
  return(corpus)
}

clcorpus <- clean_corpus(const.corpus)

#TDM
tdm_const <- TermDocumentMatrix(clcorpus)

# Analises
matrix_const <- as.matrix(tdm_const)

# Palavras similares nas constituicoes
commonality.cloud(matrix_const, max.words = 200, colors = "red")

# Palavras não similares nas constituicoes
colnames(tdm_const) <- c("Constituicao 1824", "Constituicao 1891", "Constituicao 1934", "Constituicao 1946")
matrix_const <- as.matrix(tdm_const)

comparison.cloud(matrix_const, colors = c("orange", "blue", "red", "green"), max.words = 100, title.size = 1)
