# Analise de dados para materia: FLP

# Pacotes
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)
library(readr)
library(ggthemes)
library(RColorBrewer)

## Eleicoes gerais: 1945, 1947, 1950, 1954, 1958 e 1962

labels.partido <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE",
                    "CODIGO_CARGO", "DESCRICAO_CARGO", "TIPO_LEGENDA", "NOME_COLIGACAO", "COMPOSICAO_LEGENDA", "SIGLA_PARTIDO", "NUMERO_PARTIDO",
                    "NOME_PARTIDO", "QTDE_VOTOS_NOMINAIS", "QTDE_VOTOS_LEGENDA")
labels.detalhe <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO",
                    "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_CARGO",
                    "DESCRICAO_CARGO","QTD_APTOS","QTD_COMPARECIMENTO",
                    "QTD_ABSTENCOES","QTD_VOTOS_NOMINAIS","QTD_VOTOS_BRANCOS",
                    "QTD_VOTOS_NULOS","QTD_VOTOS_LEGENDA","QTD_VOTOS_ANULADOS_APU_SEP",
                    "QTD_SECOES_TOT","QTD_SECOES_ANULADAS","QTD_SECOES_SEM_FUNCION",
                    "QTD_ZONAS_ELEITORAIS","QTD_JUNTAS_APURADORAS")
labels.candidato <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE",
                      "CODIGO_CARGO","NUMERO CAND", "SQ_CANDIDATO", "NOME_CANDIDATO",
                      "NOME_URNA_CANDIDATO","DESCRICAO_CARGO","COD_SIT_CAND_SUPERIOR",
                      "DESC_SIT_CAND_SUPERIOR", "CODIGO_SIT_CANDIDATO", 
                      "DESC_SIT_CANDIDATO", "CODIGO_SIT_CAND_TOT", "DESC_SIT_CAND_TOT",
                      "NUMERO_PARTIDO","SIGLA_PARTIDO", "NOME_PARTIDO",
                      "SEQUENCIAL_LEGENDA", "NOME_COLIGACAO", "COMPOSICAO_LEGENDA",
                      "TOTAL_VOTOS")
arquivo.partido <- "VOTACAO_PARTIDO_UF_ANO_UNIDF.txt"
arquivo.detalhe <- "DETALHE_VOTACAO_UF_ANO_UNIDF.txt"
arquivo.cand <- "VOTACAO_CANDIDATO_UF_ANO_UNIDF.txt"

### 1945
setwd()
uf.partido <- c("AC", "AL", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
                "RS", "SC", "SE", "SP")
uf.detalhe <- uf.partido
uf.cand <- c(uf.partido, "Fernando de Noronha", "Iguaçu", "Ponta Porã")

arq.ufs.partido <- purrr::map(arquivo.partido, str_replace, "UNIDF", uf.partido)
arq.ufs.partido <- map(arq.ufs.partido, str_replace, "ANO", as.character(1945))

arq.ufs.detalhe <- purrr::map(arquivo.detalhe, str_replace, "UNIDF", uf.detalhe)
arq.ufs.detalhe <- map(arq.ufs.detalhe, str_replace, "ANO", as.character(1945))

arq.ufs.cand <- purrr::map(arquivo.cand, str_replace, "UNIDF", uf.detalhe)
arq.ufs.cand <- map(arq.ufs.cand, str_replace, "ANO", as.character(1945))

votPart45 <- purrr::map_df(unlist(arq.ufs.partido), read_csv2, col_names = labels.partido, locale=locale(encoding = "latin1"))
votDeta45 <- purrr::map_df(unlist(arq.ufs.detalhe), read_csv2, col_names = labels.detalhe, locale=locale(encoding = "latin1"))
votCand45 <- purrr::map_df(unlist(arq.ufs.cand), read_csv2, col_names = labels.candidato, locale=locale(encoding = "latin1"))

### 1947
setwd()
uf.partido <- c("AL", "AM","AP" ,"BA", "CE", "DF", "ES", "GO","GP", "MA", "MG", "MT", "PA", "PB", "PE", "PI", "PR","RB", "RJ", "RN",
                "RS", "SC", "SE", "SP")
uf.detalhe <- uf.partido
uf.cand <- uf.partido

arq.ufs.partido <- purrr::map(arquivo.partido, str_replace, "UNIDF", uf.partido)
arq.ufs.partido <- map(arq.ufs.partido, str_replace, "ANO", as.character(1947))

arq.ufs.detalhe <- purrr::map(arquivo.detalhe, str_replace, "UNIDF", uf.detalhe)
arq.ufs.detalhe <- map(arq.ufs.detalhe, str_replace, "ANO", as.character(1947))

arq.ufs.cand <- purrr::map(arquivo.cand, str_replace, "UNIDF", uf.detalhe)
arq.ufs.cand <- map(arq.ufs.cand, str_replace, "ANO", as.character(1947))

votPart47 <- purrr::map_df(unlist(arq.ufs.partido), read_csv2, col_names = labels.partido, locale=locale(encoding = "latin1"))
votDeta47 <- purrr::map_df(unlist(arq.ufs.detalhe), read_csv2, col_names = labels.detalhe, locale=locale(encoding = "latin1"))
votCand47 <- purrr::map_df(unlist(arq.ufs.cand), read_csv2, col_names = labels.candidato, locale=locale(encoding = "latin1"))

### 195O
setwd()
uf.partido <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO","GP", "MA", "MG", "MT", "PA", "PB", "PE", "PI", "PR","PB" ,"RJ", "RN",
                "RS", "SC", "SE", "SP")
uf.detalhe <- uf.partido
uf.cand <- uf.partido

arq.ufs.partido <- purrr::map(arquivo.partido, str_replace, "UNIDF", uf.partido)
arq.ufs.partido <- map(arq.ufs.partido, str_replace, "ANO", as.character(1950))

arq.ufs.detalhe <- purrr::map(arquivo.detalhe, str_replace, "UNIDF", uf.detalhe)
arq.ufs.detalhe <- map(arq.ufs.detalhe, str_replace, "ANO", as.character(1950))

arq.ufs.cand <- purrr::map(arquivo.cand, str_replace, "UNIDF", uf.detalhe)
arq.ufs.cand <- map(arq.ufs.cand, str_replace, "ANO", as.character(1950))

votPart50 <- purrr::map_df(unlist(arq.ufs.partido), read_csv2, col_names = labels.partido, locale=locale(encoding = "latin1"))
votDeta50 <- purrr::map_df(unlist(arq.ufs.detalhe), read_csv2, col_names = labels.detalhe, locale=locale(encoding = "latin1"))
votCand50 <- purrr::map_df(unlist(arq.ufs.cand), read_csv2, col_names = labels.candidato, locale=locale(encoding = "latin1"))

### 1954
setwd()
uf.partido <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO","GP", "MA", "MG", "MT", "PA", "PB", "PE", "PI", "PR","PB" ,"RJ", "RN",
                "RS", "SC", "SE", "SP")
uf.detalhe <- uf.partido
uf.cand <- uf.partido

arq.ufs.partido <- purrr::map(arquivo.partido, str_replace, "UNIDF", uf.partido)
arq.ufs.partido <- map(arq.ufs.partido, str_replace, "ANO", as.character(1954))

arq.ufs.detalhe <- purrr::map(arquivo.detalhe, str_replace, "UNIDF", uf.detalhe)
arq.ufs.detalhe <- map(arq.ufs.detalhe, str_replace, "ANO", as.character(1954))

arq.ufs.cand <- purrr::map(arquivo.cand, str_replace, "UNIDF", uf.detalhe)
arq.ufs.cand <- map(arq.ufs.cand, str_replace, "ANO", as.character(1954))

votPart54 <- purrr::map_df(unlist(arq.ufs.partido), read_csv2, col_names = labels.partido, locale=locale(encoding = "latin1"))
votDeta54 <- purrr::map_df(unlist(arq.ufs.detalhe), read_csv2, col_names = labels.detalhe, locale=locale(encoding = "latin1"))
votCand54 <- purrr::map_df(unlist(arq.ufs.cand), read_csv2, col_names = labels.candidato, locale=locale(encoding = "latin1"))

### 1958
setwd()
uf.partido <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MT", "PA", "PB", "PE", "PI", "PR","PB","RB" ,"RJ", "RN","RO",
                "RS", "SC", "SE", "SP")
uf.detalhe <- uf.partido
uf.cand <- uf.partido

arq.ufs.partido <- purrr::map(arquivo.partido, str_replace, "UNIDF", uf.partido)
arq.ufs.partido <- map(arq.ufs.partido, str_replace, "ANO", as.character(1958))

arq.ufs.detalhe <- purrr::map(arquivo.detalhe, str_replace, "UNIDF", uf.detalhe)
arq.ufs.detalhe <- map(arq.ufs.detalhe, str_replace, "ANO", as.character(1958))

arq.ufs.cand <- purrr::map(arquivo.cand, str_replace, "UNIDF", uf.detalhe)
arq.ufs.cand <- map(arq.ufs.cand, str_replace, "ANO", as.character(1958))

votPart58 <- purrr::map_df(unlist(arq.ufs.partido), read_csv2, col_names = labels.partido, locale=locale(encoding = "latin1"))
votDeta58 <- purrr::map_df(unlist(arq.ufs.detalhe), read_csv2, col_names = labels.detalhe, locale=locale(encoding = "latin1"))
votCand58 <- purrr::map_df(unlist(arq.ufs.cand), read_csv2, col_names = labels.candidato, locale=locale(encoding = "latin1"))

### 1962
setwd()
uf.partido <- c("AC", "AL", "AM", "AP", "BA", "CE", "ES", "GB","GO", "MA", "MG", "MT", "PA", "PB", "PE", "PI", "PR","PB" ,"RJ", "RN","RO",
                "RS", "SC", "SE", "SP")
uf.detalhe <- uf.partido
uf.cand <- uf.partido

arq.ufs.partido <- purrr::map(arquivo.partido, str_replace, "UNIDF", uf.partido)
arq.ufs.partido <- map(arq.ufs.partido, str_replace, "ANO", as.character(1962))

arq.ufs.detalhe <- purrr::map(arquivo.detalhe, str_replace, "UNIDF", uf.detalhe)
arq.ufs.detalhe <- map(arq.ufs.detalhe, str_replace, "ANO", as.character(1962))

arq.ufs.cand <- purrr::map(arquivo.cand, str_replace, "UNIDF", uf.detalhe)
arq.ufs.cand <- map(arq.ufs.cand, str_replace, "ANO", as.character(1962))

votPart62 <- purrr::map_df(unlist(arq.ufs.partido), read_csv2, col_names = labels.partido, locale=locale(encoding = "latin1"))
votDeta62 <- purrr::map_df(unlist(arq.ufs.detalhe), read_csv2, col_names = labels.detalhe, locale=locale(encoding = "latin1"))
votCand62 <- purrr::map_df(unlist(arq.ufs.cand), read_csv2, col_names = labels.candidato, locale=locale(encoding = "latin1"))

rm(uf.partido, uf.detalhe, uf.cand, arq.ufs.partido, arq.ufs.detalhe, arq.ufs.cand, labels.partido, labels.detalhe, labels.candidato, arquivo.cand, arquivo.partido, arquivo.detalhe)

### Juntando todas as bases

### votPartANO

votPart <- bind_rows(votPart45, votPart47)
votPart <- bind_rows(votPart, votPart50)
votPart <- bind_rows(votPart, votPart54)
votPart <- bind_rows(votPart, votPart58)
votPart <- bind_rows(votPart, votPart62)

rm(votPart45, votPart47, votPart50, votPart54, votPart58, votPart62)

### votCandANO

votCand <- bind_rows(votCand45, votCand47)
votCand <- bind_rows(votCand, votCand50)
votCand <- bind_rows(votCand, votCand54)
votCand <- bind_rows(votCand, votCand58)
votCand <- bind_rows(votCand, votCand62)

rm(votCand45, votCand47, votCand50, votCand54, votCand58, votCand62)

### votDetaANO

votDeta <- bind_rows(votDeta45, votDeta47)
votDeta <- bind_rows(votDeta, votDeta50)
votDeta <- bind_rows(votDeta, votDeta54)
votDeta <- bind_rows(votDeta, votDeta58)
votDeta <- bind_rows(votDeta, votDeta62)

rm(votDeta45, votDeta47, votDeta50, votDeta54, votDeta58, votDeta62)

### Analises

#### Ampliação da participação

# Aptos para votar
votDeta %>% filter(DESCRICAO_CARGO =="DEPUTADO FEDERAL")%>%  group_by(ANO_ELEICAO) %>% summarise(APTOS = sum(QTD_APTOS), COMP = sum(QTD_COMPARECIMENTO)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = APTOS)) + geom_line(color = 'orange', size = 1) + theme_wsj() + scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Quantidade de eleitores aptos para votar", subtitle = "Eleições para Deputados Federais")

# Aptos para votar por UE
votDeta %>% filter(DESCRICAO_CARGO =="DEPUTADO FEDERAL")%>%  group_by(ANO_ELEICAO, SIGLA_UE) %>% summarise(APTOS = sum(QTD_APTOS), COMP = sum(QTD_COMPARECIMENTO)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = APTOS)) + geom_line(aes(color = SIGLA_UE), size = 1) + theme_wsj() + scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Quantidade de eleitores aptos para votar por UE", subtitle = "Eleições para Deputados Federais")

# Comparecimento/Aptos para votar
votDeta %>% filter(DESCRICAO_CARGO =="DEPUTADO FEDERAL")%>%  group_by(ANO_ELEICAO) %>% summarise(CompAptos = sum(QTD_COMPARECIMENTO)/sum(QTD_APTOS)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = CompAptos)) + geom_line(color = 'orange', size = 1) + theme_wsj() + scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Proporção Comparecimento / Aptos", subtitle = "Eleições para Deputados Federais")

# Comparecimento/Aptos para votar por UE
votDeta %>% filter(DESCRICAO_CARGO =="DEPUTADO FEDERAL")%>%  group_by(ANO_ELEICAO, SIGLA_UE) %>% summarise(CompAptos = sum(QTD_COMPARECIMENTO)/sum(QTD_APTOS)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = CompAptos)) + geom_line(aes(color = SIGLA_UE), size = 1) + theme_wsj() + scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Proporção Comparecimento / Aptos por UE", subtitle = "Eleições para Deputados Federais")

# Abstenções
votDeta %>% filter(DESCRICAO_CARGO =="DEPUTADO FEDERAL")%>%  group_by(ANO_ELEICAO) %>% summarise(ABST = sum(QTD_ABSTENCOES), COMP = sum(QTD_COMPARECIMENTO)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = ABST)) + geom_line(color = 'orange', size = 1) + theme_wsj() + scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Quantidade de abstenções", subtitle = "Eleições para Deputados Federais")

# Abstenções por UE
votDeta %>% filter(DESCRICAO_CARGO =="DEPUTADO FEDERAL")%>%  group_by(ANO_ELEICAO) %>% summarise(ABST = sum(QTD_ABSTENCOES), COMP = sum(QTD_COMPARECIMENTO)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = ABST)) + geom_line(aes(color = SIGLA_UE), size = 1) + theme_wsj() + scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Quantidade de abstenções", subtitle = "Eleições para Deputados Federais")

# Quantidade de votos recebidos por partido (UDN, PSD, PTB)
votPart %>% filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL", SIGLA_PARTIDO %in% c("UDN", "PSD", "PTB")) %>% group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>% summarise(TOTAL_RECEBIDOS = sum(QTDE_VOTOS_NOMINAIS) + sum(QTDE_VOTOS_LEGENDA)) %>% 
  ggplot(., aes(x = ANO_ELEICAO, y = TOTAL_RECEBIDOS))+ geom_line(aes(color = SIGLA_PARTIDO)) +theme_wsj() +scale_x_continuous(breaks = c(1945, 1947, seq(1950,1962,4)))+
  ggtitle(label = "Quantidade de votos recebidos por partido", subtitle = "Eleições para Deputados Federais")

# Quem foi eleito pelo PSD em 1945?
votCand %>% filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL", SIGLA_PARTIDO == "PSD",
                   ANO_ELEICAO == 1945, DESC_SIT_CAND_TOT == "ELEITO", TOTAL_VOTOS != 0) %>%
  arrange(desc(TOTAL_VOTOS)) %>% .[1:5,] %>%  
  ggplot(., aes(x = NOME_CANDIDATO, TOTAL_VOTOS)) +geom_bar(stat = 'identity') +theme_wsj()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle(label = "Quantidade de votos PSD")

# Quem foram os eleito pelo PSD e UDN em 1945? (Não deu certo!)
votCand %>% filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL", SIGLA_PARTIDO %in% c("PSD", "UDN"),
                   ANO_ELEICAO == 1945, DESC_SIT_CAND_TOT == "ELEITO", TOTAL_VOTOS != 0) %>%
  arrange(desc(TOTAL_VOTOS)) %>% .[1:20,] %>%  
  ggplot(., aes(x = NOME_CANDIDATO, TOTAL_VOTOS)) +geom_bar(stat = 'identity') +theme_wsj()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle(label = "Quantidade de votos PSD") +
  facet_wrap(~SIGLA_PARTIDO)

# Qual foi o desempenho do PCB em comparação com os outros partidos (VOTOS TOTAIS)
votCand %>% filter(DESCRICAO_CARGO == "PRESIDENTE", ANO_ELEICAO == 1945, SIGLA_PARTIDO %in% c("PCB", "PSD", "UDN")) %>% 
  ggplot(aes(x = SIGLA_UF, y = TOTAL_VOTOS)) + geom_bar(stat = "identity", aes(fill = SIGLA_PARTIDO), position = "dodge")+
  theme_wsj()+scale_fill_brewer(palette="Dark2")+ggtitle(label = "Quantidade de voto recebidos na eleição presidencial", subtitle = "Eleição de 1945")+
  theme(legend.position = "bottom")

# Qual foi o desempenho do PCB em comparação com os outros partidos (Porcentagem por Estados)
votCand %>%
  filter(DESCRICAO_CARGO == "PRESIDENTE",
         ANO_ELEICAO == 1945,
         SIGLA_PARTIDO %in% c("PCB", "PSD", "UDN")) %>%
  group_by(SIGLA_UF, SIGLA_PARTIDO) %>% 
  summarise(TOTAL_VOTOS = sum(TOTAL_VOTOS)) %>% 
  ungroup() %>% 
  group_by(SIGLA_UF) %>% 
  mutate(TOTAL_REC = sum(TOTAL_VOTOS),
         PORC = TOTAL_VOTOS/TOTAL_REC) %>% 
  ggplot(aes(x = SIGLA_UF, y = PORC)) + geom_bar(stat = "identity", aes(fill = SIGLA_PARTIDO), position = "dodge",width = 0.85)+
  theme_wsj()+scale_fill_brewer(palette="Dark2")+ggtitle(label = "Porcentagem de votos recebidos\nna eleição presidencial", subtitle = "Eleição de 1945")+
  theme(legend.position = "bottom")+scale_y_continuous(breaks = seq(.1, 1, .1), limits = c(0, 1))
  
