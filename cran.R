# Quantos pacotes estao disponiveis no CRAN?
# Data da coleta: 7 de Julho de 2017

library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)

url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"

cran <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

cran$Date <- lubridate::as_date(cran$Date)
cran$year <- lubridate::year(cran$Date)

ggplot(data = cran) + geom_bar(aes(year)) + labs(title = "Quantidade de pacotes disponiveis no CRAN por ano",subtitle = "n = 11001", x = "Ano", y = "Contagem") +
  scale_x_continuous(breaks = seq(2004, 2017, 1)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
