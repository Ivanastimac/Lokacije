rm(list = ls())

#za scrapanje htmla
library(rvest)
library(stringr)
#za funkciju xml_remove
library(xml2)
#za crtanje na karti
library(leaflet)
# Moran.I
library(ape)

link <- 'https://meteo.hr/podaci.php?section=podaci_vrijeme&param=hrvatska1_n'
website <- read_html(link)

met_zone <- website %>% html_nodes(".fd-c-table td.fd-u-text-align--left") 

#neke postaje imaju eksponent A pa to mičemo
removed_nodes <- met_zone %>% html_nodes("sup")
xml_remove(removed_nodes)

met_zone <- met_zone %>% html_text() 

#met_zone vektor sadrži imena svih postaja koje smo 
#unijeli u OSM Nominatim tražilicu kako bi spremili koordinate mjesta
met_zone <- str_replace_all(gsub("[\r\n]", "", met_zone), fixed(" "), "")

#pomoćni vektor za preuzimanje mjerenja iz sata u sat
time_node <- website %>% html_nodes(".hours-browser-v2 .hours-browser-v2__hours") %>% html_nodes('a') %>% html_attr('href')

#inicijalizacija liste tlakova
pressure <- list()

for (time_link in time_node){
  #svaki sat ima drugaciji link na web stranicu koji smo izdvojili iz html-a stranice tj. <a href>  
  #elementa pomocu kojeg ocitavamo tlak izmjeren na svim postajama za taj sat
  link <- gsub(" ", "", paste('https:',time_link))
  website <- read_html(link)
  hour_pressure <- website %>% html_nodes("td:nth-child(6)") %>% html_text()
  
  #neki tlakovi imaju * pa i to mičemo
  hour_pressure <- str_replace_all(gsub("[*]", "", hour_pressure), fixed(" "), "")
  
  #dodajemo tlak izmjeren za trenutni sat u listu tlakova
  pressure <- append(pressure, paste(str_sub(link, start=-2), met_zone, hour_pressure))
}

#listu tlakova po satima i mjestu mjerenja pretvaramo u matricu za lakše računanje
i = 0
matrix <- c(0, 0, 0)

for (row in pressure){
  i <- i + 1
  split <- str_split(row, " ")
  matrix <- rbind(matrix, c(split[[1]][1], split[[1]][2], split[[1]][3]))
}

#dodajemo naziv stupaca i uklanjamo prvi redak matrice koji je bio pomoćni vektor
#za inicijalizaciju matrice
colnames(matrix) <- c("hour", "place", "pressure")
matrix <- matrix[-c(1), ]

#za koristit relativnu putanju
#setwd(getwd())
#setwd('C:\\Users\\user\\Desktop\\Faks\\2.god\\UZNL_projekt\\Lokacije')
setwd('C:\\Users\\ivana\\Documents\\Faks\\5. godina\\III semestar\\Usluge zasnovane na lokaciji\\Seminar\\Lokacije')
file <- read.csv(file='./postaje.csv', header=TRUE, encoding="UTF-8")

cities <- data.frame(longitude = file[, 3],
                     latitude = file[, 2],
                     names = file[, 1])

# https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
dists <- as.matrix(dist(cbind(cities$longitude, cities$latitude)))

dists.inv <- 1/dists
diag(dists.inv) <- 0

# onoliko koliko je postaja ali nemamo uvijek isti broj
step = 45
for (i in seq(1, length(matrix), step)) {
  index = i + step
  mat = as.numeric(as.matrix(matrix[i:index, "pressure"]))
  # nije dobro, tu je samo da ne baca error
  # možda uzet jedno sljedeće mjerenje koje imamo ili prosjek svih mjerenja za tu postaju
  mat[is.na(mat)] <- 0
  print(matrix[i, "hour"])
  print(matrix[i, "place"])
  print(Moran.I(mat, dists.inv)) 
}

# tu sam dosta tog pobrisala, možda će trebat nešto vratit haha

#print_data = paste(cities$names, matrix$pressure)

map.point <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = cities$longitude, lat = cities$latitude,
             label = cities$names, labelOptions = labelOptions(
               noHide = FALSE))
map.point