library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
library(pdftools)
library(quanteda)
library(tm)

# Obter os links dos discursos
url <- "http://www.biblioteca.presidencia.gov.br/presidencia/ex-presidentes/jk/discursos/1956"

titulo <- url %>%
  read_html %>%
  html_nodes(xpath = '//h2[@class ="tileHeadline"]/a[@class="summary url"]') %>% 
  html_text()

link <- url %>%
  read_html %>%
  html_nodes(xpath = '//h2[@class ="tileHeadline"]/a[@class="summary url"]') %>% 
  html_attr("href")

link_discursos <- tibble(titulo, link)

link_discursos %<>% mutate(data = titulo %>% str_extract("(.*?)-") %>% 
  str_replace("-","") %>% str_trim(side = "both"))

# Obter o link dos PDFs dos discursos

link_pdf <- map(link_discursos$link, ~.x %>% read_html %>% 
      html_nodes(xpath = '//div[@id="content-core"]/p/a') %>% 
      html_attr("href"))

link_pdf %<>% unlist() %>% tibble

link_discursos <- bind_cols(link_discursos, link_pdf)

link_discursos$link_pdf <- link_discursos$.

link_discursos$. <- NULL

# dowload dos PDFs

salvar_em <- "C:\\Users\\work_\\PDF\\"

for (i in 1:nrow(link_discursos)){
  nome <- link_discursos$link_pdf[i] %>% str_sub(end = -1, start = -6)
  download.file(url = link_discursos$link_pdf[i], destfile= paste0(salvar_em,nome),mode = "wb")
}


# Abrindo os PDF para extrair os discursos
sequencia <- c("01","02","03","04","05","06","07","08","09", as.character(10:74))
pdf <- map("PDF/NUMERO.pdf", ~str_replace(.x, "NUMERO", sequencia)) %>% unlist()

paginas <- map(pdf, ~pdf_text(pdf=.x))

# Função para transformar separacao de silabas de volta

voltasilaba <- function(texto){
  for (i in 1:length(texto)){
    if (str_sub(texto[i], end= -1, start = -1) == "-"){
      texto[i] <- str_replace(texto[i], "\\-$","")
      texto[i] <- str_c(texto[i], texto[i + 1])
      
      texto[i+1] <- ""
    }
  }

  limparTexto <- which(str_length(texto)==0)

  texto <- texto[-limparTexto]
  
  return(texto)
}

arrumaTexto <- function(paginas){
  texto <- NULL
  for (i in 2:length(paginas)){
    frag_pagina <- str_trunc(paginas[i], str_length(paginas[i]))  %>% 
      str_split("\n") %>% 
      .[[1]] %>% 
      str_replace_all("\\r","") %>% 
    str_trim(side = "both")
    texto <- c(texto, frag_pagina)
  }
  
  return(texto)
}

texto <- arrumaTexto(paginas) %>%
  tm::removeNumbers() %>%
  str_trim() %>%
  voltasilaba() %>%
  paste0(collapse = " ")

discursos <- NULL
for (i in 1:length(paginas)){
  texto <- arrumaTexto(paginas[[i]]) %>%
  tm::removeNumbers() %>%
  str_trim() %>%
  voltasilaba() %>%
  paste0(collapse = " ")
  discursos <- c(discursos, texto)
}

discursos_JK <- tibble(discursos) %>% bind_cols(link_discursos)
