library(rvest)
library(magrittr)
library(tidyverse)
library(stringr)

codigo <- c(Sociologia = "FSL", Antropologia = "FLA", CienciaPolitica = "FLP")

url <- "https://uspdigital.usp.br/jupiterweb/jupDisciplinaLista?codcg=8&pfxdisval=CODIGO&tipo=D"

link <- map(url, ~str_replace(.x,"CODIGO",codigo)) %>% unlist

tabelaDisciplinas <- tibble()
for (i in seq_along(link)){
  Disciplina <- read_html(link[i]) %>% 
    html_nodes(xpath = '//table[@align="center"]') %>%
    html_table(header = T) %>% .[[1]]
  
  DisciplinaNome <- rep(names(codigo[i]), nrow(Disciplina)) %>% tibble()
  
  link_Disciplina <- read_html(link[i]) %>% 
    html_nodes(xpath = '//font/span[@class="txt_arial_8pt_gray"]/a[@class="link_gray"]') %>% 
    html_attr("href") %>% 
    map_chr(.,~str_c("https://uspdigital.usp.br/jupiterweb/", .x)) %>% tibble()
  
  tabela <- bind_cols(Disciplina, DisciplinaNome) %>% bind_cols(link_Disciplina)
  
  tabelaDisciplinas <- bind_rows(tabelaDisciplinas, tabela)
  
}

tabelaDisciplinas %<>% rename(Materia = ".",
                              Links=".1") 

ObtemDetalhes <- function(link){
  
  creditoAula <- read_html(link) %>% 
    html_nodes(xpath = '//tr[@valign="TOP"]/td/font[@size="1"]/span[@class="txt_arial_8pt_gray"]') %>% 
    html_text() %>%
    .[1] %>% str_replace_all(.,"\r|\n|\t", "") %>% 
    str_trim(., side = "both") %>% as.numeric()

  creditoTrabalho <- read_html(link) %>% 
    html_nodes(xpath = '//tr[@valign="TOP"]/td/font[@size="1"]/span[@class="txt_arial_8pt_gray"]') %>% html_text() %>%
    .[2] %>% str_replace_all(.,"\r|\n|\t", "") %>% 
    str_trim(., side = "both") %>% as.numeric()
  
  cargaHorariaHoras <- read_html(link) %>% 
    html_nodes(xpath = '//tr[@valign="TOP"]/td/font[@size="1"]/span[@class="txt_arial_8pt_gray"]') %>% html_text() %>%
    .[3] %>% str_replace_all(.,"\r|\n|\t|h", "") %>% 
    str_trim(., side = "both")
  
  return(tibble(creditoAula,creditoTrabalho, cargaHorariaHoras))
  
}

tabela_Det <- tibble() 
for (i in 1:nrow(tabelaDisciplinas)) {
  tab <- ObtemDetalhes(tabelaDisciplinas$Links[i])
  if (nrow(tab) == 0){
    tab[1,] <- NA
  }
  tabela_Det <- bind_rows(tabela_Det, tab)
}

tabelaDisciplinas <- bind_cols(tabelaDisciplinas, tabela_Det)

tabelaDisciplinas %<>% mutate(CreditoTotal = creditoAula + creditoTrabalho) 

tabelaDisciplinas %>% arrange(desc(CreditoTotal)) %>% select(Nome,CreditoTotal)

tabelaDisciplinas %>% filter(CreditoTotal == 5) %>% group_by(Materia) %>% summarise(n = n())
