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
#za preuzeti koordinate postaja
library(jsonlite)

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


df <- data.frame(place = c(met_zone))
rownames(df) <- df$place

for (time_link in time_node){
  #svaki sat ima drugaciji link na web stranicu koji smo izdvojili iz html-a stranice tj. <a href>  
  #elementa pomocu kojeg ocitavamo tlak izmjeren na svim postajama za taj sat
  link <- gsub(" ", "", paste('https:',time_link))
  website <- read_html(link)
  hour_pressure <- website %>% html_nodes("td:nth-child(6)") %>% html_text()
  
  hour_place <- website %>% html_nodes(".fd-c-table td.fd-u-text-align--left")
  removed_nodes <- hour_place %>% html_nodes("sup")
  xml_remove(removed_nodes)
  hour_place <- hour_place %>% html_text()
  hour_place <- str_replace_all(gsub("[\r\n]", "", hour_place), fixed(" "), "")
  
  #neki tlakovi imaju * pa i to mičemo
  hour_pressure <- str_replace_all(gsub("[*]", "", hour_pressure), fixed(" "), "")
  hour <- str_sub(link, start=-2) 
  
  hour_matrix <- data.frame(hour_place, hour_pressure)
  rownames(hour_matrix) <- hour_matrix$hour_place
  df$hour <- c()
  
  for (place1 in df$place){
    for (place2 in hour_matrix$hour_place){
      if (place1 == place2) {
        if (hour_matrix[place2, 'hour_pressure'] == '-'){
          df[place1, hour] = NA
        } else {
          df[place1, hour] = hour_matrix[place2, 'hour_pressure'] 
        }
      }
    }
  }

}

# pretvoriti u numeričke vrijednost
library(dplyr)
df2 <- mutate_all(df[, 2:25], function(x) as.numeric(as.character(x)))

# uklanjamo redove s NA vrijednostima
df = na.omit(df2)


#za koristit relativnu putanju
#setwd(getwd())
setwd('C:\\Users\\user\\Desktop\\Faks\\2.god\\UZNL_projekt\\Lokacije')
#setwd('C:\\Users\\ivana\\Documents\\Faks\\5. godina\\III semestar\\Usluge zasnovane na lokaciji\\Seminar\\Lokacije')
file <- read.csv(file='./postaje.csv', header=TRUE, encoding="UTF-8")

#spremi preuzete podatke u csv datoteku
write.table(df, file='ocitanja.csv', quote=F, fileEncoding='UTF-8', sep=',')


cities <- data.frame(longitude = file[, 3],
                     latitude = file[, 2],
                     names = file[, 1])

for (city in cities$names){
  if (!(city %in% rownames(df))){
    cities[which(cities$names == city), 3] <- NA
  }
}

# uklanjamo redove s NA vrijednostima
cities = na.omit(cities)

library(sf)
s.sf <- st_read("gis_osm_places_free_1.shp")

# uklanjamo redove s nazivom Centar koji se odnosi na centar gradova kako bi ostalo ime grada
for (i in 1:8){
  s.sf <- s.sf[-c(match("Centar", s.sf$name)), ]
}


a = st_coordinates(s.sf$geometry) 
cities2 = data.matrix(cities)

sp <- data.frame()

for (i in (1:nrow(a))){
  for (j in (1:nrow(cities))){
    if (abs(a[i, 1] - cities2[j, 1]) < 0.001 
        && abs(a[i, 2] - cities2[j, 2]) < 0.001){
      
      sp <- rbind(sp, s.sf[i, , ])
    }
  }
}

cities2 <- cities

# uklanjamo redove koje ne možemo prikazati kao prostorne točke (nedostaju podaci u bazi)
for (i in (1:nrow(cities))){
  if (!(cities[i, , ]$longitude %in% st_coordinates(sp$geometry)[, 1])){
    cities2 <- cities2[-c(i), ]
  }
}

cities <- cities2

df2 <- df

for (i in (1:nrow(df))){
  if (!(rownames(df[i, , ]) %in% cities$names)){
    df2 <- df2[-c(i), ]
  }
}

df <- df2


# grafički prikaz postaja s prostornim podacima

popup = paste0(
  str_to_title(sp$fclass), ": ", sp$name,  "<br/>",
  "Population: ", sp$population,  "<br/>")

map2 <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    data = sp,
    label = lapply(popup, htmltools::HTML) 
  )
map2

# priprema podataka za računjanje Moranovih koeficijenata

# https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
dists <- as.matrix(dist(cbind(cities$longitude, cities$latitude)))

dists.inv <- 1/dists
diag(dists.inv) <- 0

moran_coefficient <- data.frame(matrix(nrow = 4, ncol = 24))
colnames(moran_coefficient) <- colnames(df)
rownames(moran_coefficient) <- c('observed', 'expected', 'sd', 'p.value')

# računjanje Moranovih koeficijenata i spremanje u data.frame objekt
for (col in colnames(df)) {
  moran <- Moran.I(df[, col], dists.inv)
  moran_coefficient[, col] <- c(moran$observed, moran$expected, moran$sd, moran$p.value)
}

# vizualizacija koeficijenata u periodu od 24h

colors <- c('gray', 'pink', col=rgb(1.0,0.7,0.5,0.6), rgb(0.9,0.6,0.7,0.9))

barplot(as.matrix(rbind(moran_coefficient[2, 1:24], 
                        moran_coefficient[1, 1:24])),
        main="observed vs expected",
        xlab="hour",
        ylab="result",
        beside = TRUE,
        col=colors[1:2],
        las=2
        )
legend("topleft",
       c("Expected","Observed"),
       fill = c("gray","pink"))

for (i in 3:4){
  barplot(as.matrix(moran_coefficient[i, 1:24]),
          main=rownames(moran_coefficient)[i],
          xlab="hour",
          ylab="result",
          col=colors[i],
          las=2)
}

