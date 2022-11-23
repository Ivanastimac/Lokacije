rm(list = ls())

library(rvest)
library(stringr)

link = 'https://meteo.hr/podaci.php?section=podaci_vrijeme&param=hrvatska1_n'
website = read_html(link)

#date_time = website %>% html_nodes(".hours-browser-v2__title span") %>% html_text()

met_zone = website %>% html_nodes("#table-aktualni-podaci td.fd-u-text-align--left") %>% html_text() 

met_zone =  str_replace_all(gsub("[\r\n]", "", met_zone), fixed(" "), "")

#popravit putanju!
file <- read.csv(file='C:/Users/istimac/Desktop/lokacije/postaje.csv', header=TRUE, encoding="UTF-8")