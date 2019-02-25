#Mape paket leaflet

install.packages("leaflet")
library(leaflet)

#kreiranje mape bez markera - prazna mapa sveta

serbia <- leaflet() %>%
  addTiles()
serbia

#dodavanje markera na mapu

serbia <- serbia %>% 
  addMarkers(lat = 44.81857842, lng = 20.47332555,
             popup = "Vitina kancelarija")
serbia

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))



#dodavanje kružića na mapu

serbia %>%
  addTiles() %>%
  addCircles(lat = 44.81857842, lng = 20.47332555)

#dodavanje više markera na mapu
set.seed(001)
#kreiranje random baze sa slučajnim markerima
tačkice <- data.frame(lat=runif(30, min = 44.8, max = 44.9),
                 lng=runif(30, min=20.4, max = 20.5))

head(tačkice)

tačkice %>%
  leaflet %>%
  addTiles() %>%
  addMarkers()

#dodavanje više kruzica na mapu

kruzici <- data.frame(lat=runif(30, min = 44.5, max = 44.6),
                      lng=runif(30, min=20.3, max = 20.4))

kruzici %>%
  leaflet %>%
  addTiles() %>%
  addCircles()

#kreiranje kustom oznake na mapi

copikon <- makeIcon(
  iconUrl = "C:/Users/vjovanovic/Desktop/R Udemy/Mapa srbije/COP logo.png",
  iconWidth = 31*300/200, iconHeight = 15,
  iconAnchorX = 31*300/230/2, iconAnchorY = 16
)

pestaloci <- data.frame(
  lat= c(43.5098344, 43.1532199, 42.9962574, 43.55252937, 44.05798552, 43.91570183, 45.6192225, 45.32406535, 43.2927068, 42.6919329),
  lng= c(21.6942727, 21.877667, 21.9480858, 22.24868774, 22.11135864, 21.37252808, 20.0484962, 20.06992215, 22.0068813, 22.1701473)
)

gradovi <- c("Žitkovac", "Brestovac", "Leskovac", "Knjaževac", "Bor", "Ćuprija", "Bečej", "Đurđevo", "Niška banja", "Surdulica")

pestaloci %>%
  leaflet %>%
  addTiles() %>%
  addMarkers(icon=copikon)

#umesto tačkica dodajemo velike krugove

pestaloci %>%
  leaflet %>%
  addTiles() %>%
  addCircleMarkers(popup = gradovi)

#dodavanje kružića na pestaloci gradove plus naziva
pestaloci %>%
  leaflet %>%
  addTiles() %>%
  addCircles(popup = gradovi)

#Mapiranje tačaka po klasteru - prebrojavanje određenih tačaka po klasteru
#random baza za Srbiju

df <- data.frame(lat=runif(500, min = 44.3, max = 44.6),
                      lng=runif(500, min=20.1, max = 20.4))

df %>%
  leaflet %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

#Umesto markera sa istim podacima dodajemo kruzice

df %>%
  leaflet %>%
  addTiles() %>%
  addCircleMarkers()

#Dizajniranje veličine kruga u zavisnosti od populacije - na postojeće Pestaloci gradove dodaje se 
#još jedan vektor koji se odnosi na veličinu populacije

gradovi <- data.frame(name= c("Žitkovac", "Brestovac", "Leskovac", "Knjaževac", "Bor", "Ćuprija", "Bečej", "Đurđevo", "Niška banja", "Surdulica"),
  pop=c(2569, 2027, 60288, 18404, 34160, 19471, 23985, 5092, 4380, 10888),
  lat= c(43.5098344, 43.1532199, 42.9962574, 43.55252937, 44.05798552, 43.91570183, 45.6192225, 45.32406535, 43.2927068, 42.6919329),
  lng= c(21.6942727, 21.877667, 21.9480858, 22.24868774, 22.11135864, 21.37252808, 20.0484962, 20.06992215, 22.0068813, 22.1701473)
)

gradovi %>%
  leaflet %>%
  addTiles() %>%
  addCircles(weight = 10, radius = sqrt(gradovi$pop)*30)

#####################################################################
######čitanje mape srbije sp package u leaflet - čitanje poligona###
####################################################################

library(sp)
serbiasp <- readRDS("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/mapa srbije/srbija.rds")
library(leaflet)

opstinesrbije <- serbia %>%
  leaflet() %>% 
  addTiles() %>%
  addPolygons(data = serbiasp, weight = 0.5)
opstinesrbije

###############################################################################
###pravljenje boja za kontinualnu varijable u leafletu na osnovu sp mape#######
##############################################################################
#https://rstudio.github.io/leaflet/colors.html 

opstine_indeks <- read_excel("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/mapa srbije/opstine.xlsx")
#u mapi sa poligonima odredjivanje imena opstine kao faktora
serbiasp$NAME_2 <- as.factor(serbiasp$NAME_2)
str(serbiasp@data)
#promena imena kako bi se baza iz eksela uparila sa sp mapom poligona
opstine_indeks$NAME_2 <- opstine_indeks$opstina 
#spajanje podataka iz eksel baze i poligona za svaku opštinu
serbia1 <- merge(serbiasp, opstine_indeks)
str(serbia1)

#skidanje mape za ceo svet - ovde nevažno

library(rgdal)

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countries <- readOGR("json/countries.geojson", "OGRGeoJSON")
map <- leaflet(countries)


# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = serbia1$indeks)


#spajanje sa mp za srbiju indeks razvoja - fillOpacity koliko boje prekrivaju izvornu mapu 1=max

opstinesrbije <- serbia1 %>%
  leaflet() %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~pal(serbia1$indeks)) %>% 
  addLegend("bottomright", pal = pal, values = ~serbia1$indeks,
            title = "Indeks razvoja opština u Srbiji",
            labFormat = labelFormat(prefix = "..."),
            opacity = 1)
opstinesrbije

#create a continuous pallet function with bins (sa određenim brojem kategorija - nijansi kontinualne varijable)

binpal <- colorBin("Blues", serbia1$indeks, 4, pretty = FALSE)


#create continuous pallet function and puts it into quintiles

qpal <- colorQuantile("Reds", serbia1$indeks, n = 7)

opstinesrbije %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~qpal(serbia1$indeks)) %>% 
  addLegend(pal = qpal, values = ~serbia1$indeks, title = "Indeks razvoja opština u Srbiji", opacity = 0.5)



###############################################################################
###pravljenje boja za kategoricke varijable u leafletu na osnovu sp mape#######
###############################################################################

inkluzija_klasteri <- leaflet() %>%
  addTiles()
inkluzija_klasteri

library(sp)
serbiasp <- readRDS("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/mapa srbije/srbija.rds")
library(leaflet)





library(readxl)
#ucitavanje podataka za opstine u ekselu

opstineeksel <- read_excel("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/mapa srbije/opstine_za_r.xlsx")

#u mapi sa poligonima odredjivanje imena opstine kao faktora

serbiasp$NAME_2 <- as.factor(serbiasp$NAME_2)
str(serbiasp@data)

#promena imena kako bi se baza iz eksela uparila sa sp mapom poligona

opstineeksel$NAME_2 <- opstineeksel$opstina 

#spajanje podataka iz eksel baze i poligona za svaku opštinu

serbia2 <- merge(serbiasp, opstineeksel)

inkluzija_klasteri %>%
  leaflet() %>% 
  addTiles() %>%
  addPolygons(data = serbia2, weight = 0.5)

#kreiranje faktora na osnovu koga se prave boje - topo.colors(5) - broj i vrsta boja

pal <- colorFactor(topo.colors(4), serbia2$klasteri)

inkluzija_klasteri %>%
  addPolygons(data = serbia2, weight = 0.5, stroke = FALSE, 
              smoothFactor = 0.2, fillOpacity = 0.5, color = ~pal(serbia2$klasteri)) %>% 
  addLegend(pal = pal, values = serbia2$klasteri, title = "Klasteri opština u Srbiji po inkluzivnosti",
            labFormat = labelFormat(prefix = "Klaster - "))
