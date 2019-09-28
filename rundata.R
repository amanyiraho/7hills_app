library(tidyverse)
library(zoo)
library(readxl)
library(lubridate)
library(magrittr)
library(gganimate)
library(ggmap)
library(ggrepel)

## https://www.gpscoordinaten.nl/route-track-detail.php?id=27527 ; and with the help of Qgis
## nodig voor bewerking grotere bestanden: 
memory.limit(size = 999999)


##routecoordinaten
route <- read.csv2("zevenheuvelenpointsm.csv", stringsAsFactors = FALSE)
##cumulatieve afstanden per punt toevoegen
route <- route %>% 
  mutate(x = 15000/(nrow(route)-1),
         ##cumsum(x)-x omdat het eerste punt de start is van het parcour, 0 meter afstand
         meter = cumsum(x)-x) %>% 
  select(X, Y, meter)
##dichtsbijzijnde punten (per km) voor koppeling met tijden, kan vast mooier, maar het werkt =)
route$km <- NA  



times <- data.frame(runners = rep(1:2, each = 15),
                    km = rep(1:15),
                    time = c("00:02:37", "00:05:27",	"00:08:19",	"00:11:07",	"00:13:58",	"00:16:46",	"00:19:27",	"00:22:22",
                             "00:25:11", "00:27:46",	"00:30:37",	"00:33:24",	"00:36:02",	"00:38:39",	"00:41:16",
                             "00:02:37", "00:05:27",	"00:08:19",	"00:11:07",	"00:13:58",	"00:16:46",	"00:19:27",	"00:22:22",
                             "00:25:11", "00:27:46",	"00:30:37",	"00:33:24",	"00:36:02",	"00:38:39",	"00:41:16"))

for (i in seq(0, 15000, by = 1000)) {
  route$km[which(abs(route$meter-i) == min(abs(route$meter-i)))] <- i / 1000
}


# save(m, file = "my_map.RData")
load(file = "my_map.RData")

