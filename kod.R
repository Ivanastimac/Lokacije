rm(list = ls())

#za scrapanje htmla
library(rvest)
library(stringr)
#za funkciju xml_remove
library(xml2)

link <- 'https://meteo.hr/podaci.php?section=podaci_vrijeme&param=hrvatska1_n'
website <- read_html(link)

met_zone <- website %>% html_nodes(".fd-c-table td.fd-u-text-align--left") 

#neke postaje imaju eksponent A pa to mičemo
removed_nodes <- met_zone %>% html_nodes("sup")
xml_remove(removed_nodes)

met_zone <- met_zone %>% html_text() 

met_zone <- str_replace_all(gsub("[\r\n]", "", met_zone), fixed(" "), "")

pressure <- website %>% html_nodes("td:nth-child(6)") %>% html_text()

#neki tlakovi imaju * pa i to mičemo
pressure <- str_replace_all(gsub("[*]", "", pressure), fixed(" "), "")

#za koristit relativnu putanju
setwd(getwd())
file <- read.csv(file='./postaje.csv', header=TRUE, encoding="UTF-8")