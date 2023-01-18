rm(list = ls())

library(spdep)
library(leaflet)
library(sf)
library(rvest)
library(dplyr)
library(stringr)
library(xml2)
library(dplyr)

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

df <- na.omit(df)

setwd('C:\\Users\\user\\Desktop\\Faks\\2.god\\UZNL_projekt\\Lokacije')
#setwd('C:\\Users\\ivana\\Documents\\Faks\\5. godina\\III semestar\\Usluge zasnovane na lokaciji\\Seminar\\Lokacije')

file <- read.csv(file='./postaje.csv', header=TRUE, encoding="UTF-8")

cities <- data.frame(longitude = file[, 3],
                     latitude = file[, 2],
                     names = file[, 1])

for (city in cities$names){
  if (!(city %in% rownames(df))){
    cities[which(cities$names == city), 3] <- NA
  }
}

# uklanjamo gradove za koje nemamo mjerenja
cities <- na.omit(cities)

df <- df[(row.names(df) %in% cities$names),]

df <- df[, 2:25]
df <- mutate_all(df, function(x) as.numeric(as.character(x)))


k <- knearneigh(coordinates(cbind(cities$longitude, cities$latitude)), longlat = TRUE)
nb <- knn2nb(k)
colW <- nb2listw(nb)

moran_array <- c()

for (col in colnames(df)){
  I <- moran(as.numeric(df[, col]), nb2listw(nb), length(nb), Szero(nb2listw(nb)))
  MC <- moran.mc(as.numeric(df[, col]), nb2listw(nb), nsim=99)
    
  moran_array <- rbind(moran_array, c(I$I, I$K, MC$statistic, MC$p.value, MC$parameter))
}

colnames(moran_array) <- c('moran_i', 'moran_k', 'moran_mc', 'mc_p_value', 'mc_observed')
moran_array <- data.frame(moran_array)

cities <- cities[order(cities$names), ] 
df <- df[order(rownames(df)), ] 
cities$mean <- rowMeans(df, na.rm=T)
rownames(cities) <- 1:nrow(cities)

cr <- raster::getData('GADM', country='HRV', level=2)
cr <- st_as_sf(cr)

for (j in 1:nrow(cities)){
  my_point <- st_point(c(cities[j, ]$longitude, cities[j, ]$latitude))
  i <- st_within(my_point, cr)
  if (!is.na(cr[as.integer(i),]$NAME_2)){
    cities[j, ]$names <- cr[as.integer(i),]$NAME_2 
  }
}

# grafički prikaz postaja s prostornim podacima
popup_var = paste0(
  "Station: ", cities$name,  "<br/>",
  "Average pressure: ", round(cities$mean, digits=4), " hPa<br/>")

map <- leaflet() %>%
  addTiles()%>%
  addAwesomeMarkers(lng = cities$longitude,
                    lat = cities$latitude,
                    popup = popup_var)
map



# vizualizacija prosječnog tlaka po postajama

library(RColorBrewer)
coul <- brewer.pal(min(12, nrow(cities)), "Set3") 

barplot(cities$mean,
        main="Average pressure",
        names.arg=cities$names,
        las=2,
        ylim=c(0, max(cities$mean)+50),
        col=coul)

# vizualizacija moran i moran.mc koeficijenata

colors <- c('gray', 'pink', col=rgb(1.0,0.7,0.5,0.6), rgb(0.9,0.6,0.7,0.9))

if (min(moran_array$moran_i) < 0){
  y_1 <- 0
  y_2 <- min(moran_array$moran_i) - 0.01
} else {
  y_1 <- 0
  y_2 <- max(moran_array$moran_i) + 0.01
}

barplot(as.matrix(rbind(moran_array$moran_i, 
                        moran_array$moran_mc)),
        main="Moran vs. Moran.mc",
        xlab="hour",
        names.arg=colnames(df),
        ylab="result",
        beside = TRUE,
        col=colors[1:2],
        ylim=c(y_1, y_2),
        las=2
)

legend("topleft",
       c("Moran","Moran MC"),
       fill = c("gray","pink"))

# vizualizacija p_vrijednosti

barplot(moran_array$mc_p_value,
        main="p - value",
        xlab="hour",
        names.arg=colnames(df),
        ylab="result",
        ylim=c(0, max(moran_array$mc_p_value) + max(moran_array$mc_p_value)*0.2),
        las=2
)


