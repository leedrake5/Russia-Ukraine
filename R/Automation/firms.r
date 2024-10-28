tryCatch(detach("package:do", unload=TRUE), error=function(e) NULL)

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

list.of.packages <- c("ggplot2", "RCurl", "reshape2", "data.table", "gsheet", "tidyverse", "lubridate", "scales", "rvest", "sf", "mapview", "raster", "ggmap", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(get_os()!="linux"){
  if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE, ask=FALSE, type="binary"))
} else if(get_os()=="linux"){
  if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE, ask=FALSE, type="source"))
}


library(ggplot2)
library(RCurl)
library(reshape2)
library(data.table)
library(gsheet)
library(tidyverse)
library(lubridate)
library(scales)
library(rvest)
library(sf)
library(mapview)
library(raster)
library(ggmap)
library(dplyr)
library(zoo)

country_colors <-   c("Russia" = "#E4181C", "Ukraine" = "#0057B8")


ggplot2::theme_set(ggplot2::theme_minimal())
# options(ggplot2.continuous.fill  = function() scale_fill_viridis_c())
# options(ggplot2.continuous.colour = function() scale_color_viridis_c())
# options(ggplot2.discrete.colour = function() scale_color_brewer(palette = "Dark2"))
# options(ggplot2.discrete.fill = function() scale_fill_brewer(palette = "Dark2"))

###Map
ggmap::register_google(key = maps_key)


btgs <- read.csv("https://raw.githubusercontent.com/simonhuwiler/uawardata/master/data/csv/btgs_current.csv")
colnames(btgs)[3] <- "lon"
colnames(btgs)[5] <- "Russian_BTGS"

firms <- read.csv("https://firms.modaps.eosdis.nasa.gov/data/active_fire/noaa-20-viirs-c2/csv/J1_VIIRS_C2_Russia_Asia_24h.csv")
write.csv(firms, paste0("~/GitHub/Russia-Ukraine/data/FIRMS/", Sys.Date(), ".csv"))
firms <- firms[firms$latitude < 52.3 & firms$latitude > 44.1 & firms$longitude < 40.3 & firms$latitude > 26,]
colnames(firms)[1] <- "lat"
colnames(firms)[2] <- "lon"
firms$NASA <- "FIRMS"

###Northern Donbass
north_donbass <- ggmap::get_map(location=c(lon=37.6, lat=49.5), source="google", maptype="roadmap", crop=FALSE, zoom=8)

north_donbas_map <- ggmap(north_donbass) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Luhansk and Kharkiv regions on ", Sys.Date())) + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/north_donbas_map.jpg", north_donbas_map, device="jpg", width=6, height=5, dpi=600)

###Southern Donbass
south_donbass <- ggmap::get_map(location=c(lon=37.0, lat=48.1), source="google", maptype="roadmap", crop=FALSE, zoom=8)

south_donbas_map <- ggmap(south_donbass) +
#geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
ggtitle(paste0("Donetsk and Zaporizhizhia regions on ", Sys.Date())) + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/south_donbas_map.jpg", south_donbas_map, device="jpg", width=6, height=5, dpi=600)

###Kherson
kherson <- ggmap::get_map(location=c(lon=32.4, lat=46.4), source="google", maptype="roadmap", crop=FALSE, zoom=8)

kherson_map <- ggmap(kherson) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Kherson region on ", Sys.Date()))  + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/kherson_map.jpg", kherson_map, device="jpg", width=6, height=5, dpi=600)


###Zaporizhizhia
zaporizhizhia <- ggmap::get_map(location=c(lon=34.8, lat=47.8), source="google", maptype="roadmap", crop=FALSE, zoom=8)

zaporizhizhia_map <- ggmap(zaporizhizhia) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Zaporizhizhia region on ", Sys.Date()))  + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/zaporizhizhia_map.jpg", zaporizhizhia_map, device="jpg", width=6, height=5, dpi=600)


###Tokmak
tokmak <- ggmap::get_map(location=c(lon=35.778822, lat=47.363644), source="google", maptype="roadmap", crop=FALSE, zoom=10)

tokmak_map <- ggmap(tokmak) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Tokmak axis on ", Sys.Date()))  + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/tokmak_map.jpg", tokmak_map, device="jpg", width=6, height=5, dpi=600)

###Velyka Novosilka
velyka_novosilka <- ggmap::get_map(location=c(lon=36.828799, lat=47.687471), source="google", maptype="roadmap", crop=FALSE, zoom=10)

velyka_novosilka_map <- ggmap(velyka_novosilka) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Velyka Novosilka axis on ", Sys.Date()))  + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/velyka_novosilka_map.jpg", velyka_novosilka_map, device="jpg", width=6, height=5, dpi=600)




###Bakhmut
bakhmut <- ggmap::get_map(location=c(lon=38.000474, lat=48.593271), source="google", maptype="roadmap", crop=FALSE, zoom=10)

bakhmut_map <- ggmap(bakhmut) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Bakhmut axis on ", Sys.Date()))  + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/bakhmut_map.jpg", bakhmut_map, device="jpg", width=6, height=5, dpi=600)


###Kupyansk
kupyansk <- ggmap::get_map(location=c(lon=37.707357, lat=49.732736), source="google", maptype="roadmap", crop=FALSE, zoom=10)

kupyansk_map <- ggmap(kupyansk) +
  #geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
  geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
  ggtitle(paste0("Kupyansk axis on ", Sys.Date()))  + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/kupyansk_map.jpg", kupyansk_map, device="jpg", width=6, height=5, dpi=600)



###FIRMS Analysis
dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")

firms_list <- list()
for(i in dates){
  tryCatch(firms_list[[as.character(i)]] <- data.table::fread(paste0("~/GitHub/Russia-Ukraine/data/FIRMS/",  as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))[,-1], error=function(e) NULL)
}

new_firms_frame <- as.data.frame(data.table::rbindlist(firms_list, use.names=TRUE, fill=TRUE))
new_firms_frame$acq_date <- as.Date(new_firms_frame$acq_date)
zap_firms_frame <- new_firms_frame[new_firms_frame$latitude < 50 & new_firms_frame$latitude > 44 & new_firms_frame$longitude < 40 & new_firms_frame$longitude > 31 & new_firms_frame$acq_date >= "2023-06-01",]
zap_firms_frame <- zap_firms_frame[complete.cases(zap_firms_frame$acq_date),c("latitude", "longitude", "brightness", "scan", "track", "acq_date", "acq_time", "satellite", "instrument", "confidence", "version", "bright_t31", "frp", "daynight", "bright_ti4", "bright_ti5")]
write.csv(zap_firms_frame, "~/GitHub/Russia-Ukraine/Apps/zapMap/data/current_firms.csv")

kyiv_firms <- new_firms_frame[new_firms_frame$latitude < 52 & new_firms_frame$latitude > 50 & new_firms_frame$longitude < 32 & new_firms_frame$longitude > 29,]
kyiv_dates = seq(as.Date("2022-02-23"), as.Date("2022-04-01"), by="days")
kyiv_date_firms <- list()
kyiv_means_firms <- list()
for(i in kyiv_dates){
  kyiv_date_firms[[i]] <- kyiv_firms[as.Date(kyiv_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
  kyiv_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(kyiv_date_firms[[i]]$frp), Region="Kyiv")
}
kyiv_firms_summary <- as.data.frame(data.table::rbindlist(kyiv_means_firms))

north_donbas_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
north_donbas_firms <- new_firms_frame[new_firms_frame$latitude < 50 & new_firms_frame$latitude > 48.5 & new_firms_frame$longitude < 39 & new_firms_frame$longitude > 36,]
north_donbas_date_firms <- list()
north_donbas_means_firms <- list()
for(i in north_donbas_dates){
  north_donbas_date_firms[[i]] <- north_donbas_firms[as.Date(north_donbas_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
  north_donbas_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(north_donbas_date_firms[[i]]$frp), Region="North Donbas")
}
north_donbas_firms_summary <- as.data.frame(data.table::rbindlist(north_donbas_means_firms))

south_donbas_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
south_donbas_firms <- new_firms_frame[new_firms_frame$latitude < 48.5 & new_firms_frame$latitude > 46.5 & new_firms_frame$longitude < 39 & new_firms_frame$longitude > 36,]
south_donbas_date_firms <- list()
south_donbas_means_firms <- list()
for(i in south_donbas_dates){
  south_donbas_date_firms[[i]] <- south_donbas_firms[as.Date(south_donbas_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
  south_donbas_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(south_donbas_date_firms[[i]]$frp), Region="South Donbas")
}
south_donbas_firms_summary <- as.data.frame(data.table::rbindlist(south_donbas_means_firms))

zaporizhizhia_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
zaporizhizhia_firms <- new_firms_frame[new_firms_frame$latitude < 47.3 & new_firms_frame$latitude > 46.0 & new_firms_frame$longitude < 37 & new_firms_frame$longitude > 34.2,]
zaporizhizhia_date_firms <- list()
zaporizhizhia_means_firms <- list()
for(i in zaporizhizhia_dates){
    zaporizhizhia_date_firms[[i]] <- zaporizhizhia_firms[as.Date(zaporizhizhia_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    zaporizhizhia_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(zaporizhizhia_date_firms[[i]]$frp), Region="Zaporizhizhia")
}
zaporizhizhia_firms_summary <- as.data.frame(data.table::rbindlist(zaporizhizhia_means_firms))


kherson_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
kherson_firms <- new_firms_frame[new_firms_frame$latitude < 47.3 & new_firms_frame$latitude > 45.5 & new_firms_frame$longitude < 34.2 & new_firms_frame$longitude > 32.6,]
kherson_date_firms <- list()
kherson_means_firms <- list()
for(i in kherson_dates){
    kherson_date_firms[[i]] <- kherson_firms[as.Date(kherson_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    kherson_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(kherson_date_firms[[i]]$frp), Region="Kherson")
}
kherson_firms_summary <- as.data.frame(data.table::rbindlist(kherson_means_firms))

crimea_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
crimea_firms <- new_firms_frame[new_firms_frame$latitude < 47.3 & new_firms_frame$latitude > 44.3 & new_firms_frame$longitude < 34.2 & new_firms_frame$longitude > 33.4,]
crimea_date_firms <- list()
crimea_means_firms <- list()
for(i in crimea_dates){
    crimea_date_firms[[i]] <- crimea_firms[as.Date(crimea_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    crimea_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(crimea_date_firms[[i]]$frp), Region="Crimea")
}
crimea_firms_summary <- as.data.frame(data.table::rbindlist(crimea_means_firms))



north_firms_summary <- as.data.frame(data.table::rbindlist(list(kyiv_firms_summary, north_donbas_firms_summary, south_donbas_firms_summary)))

north_firms_summary_plot <- ggplot(north_firms_summary, aes(Date, FRP, colour=Region)) +
  #geom_point() +
  geom_line() +
  #stat_smooth(method="gam") +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Fire Radiative Power (MegaWatts)", breaks=scales::pretty_breaks(n=10), labels=scales::comma) +
  ggtitle("FIRMS VIIRS I-Band 375 m Active Fire") +
  theme_light()

ggsave("~/Github/Russia-Ukraine/Plots/north_firms_summary_plot.jpg", north_firms_summary_plot, device="jpg", width=6, height=5, dpi=600)


south_firms_summary <- as.data.frame(data.table::rbindlist(list(zaporizhizhia_firms_summary, kherson_firms_summary, crimea_firms_summary)))

south_firms_summary_plot <- ggplot(south_firms_summary, aes(Date, FRP, colour=Region)) +
  #geom_point() +
  geom_line() +
  #stat_smooth(method="gam") +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Fire Radiative Power (MegaWatts)", breaks=scales::pretty_breaks(n=10), labels=scales::comma) +
  ggtitle("FIRMS VIIRS I-Band 375 m Active Fire") +
  theme_light()

ggsave("~/Github/Russia-Ukraine/Plots/south_firms_summary_plot.jpg", south_firms_summary_plot, device="jpg", width=6, height=5, dpi=600)

###Axis Maps
tokmak_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
tokmak_firms <- new_firms_frame[new_firms_frame$latitude < 47.7 & new_firms_frame$latitude > 47.0 & new_firms_frame$longitude < 36.2 & new_firms_frame$longitude > 35.4,]
tokmak_date_firms <- list()
tokmak_means_firms <- list()
for(i in tokmak_dates){
    tokmak_date_firms[[i]] <- tokmak_firms[as.Date(tokmak_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    tokmak_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(tokmak_date_firms[[i]]$frp), Axis="Tokmak")
}
tokmak_firms_summary <- as.data.frame(data.table::rbindlist(tokmak_means_firms))

velyka_novosilka_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
velyka_novosilka_firms <- new_firms_frame[new_firms_frame$latitude < 47.9 & new_firms_frame$latitude > 47.4 & new_firms_frame$longitude < 37.1 & new_firms_frame$longitude > 36.4,]
velyka_novosilka_date_firms <- list()
velyka_novosilka_means_firms <- list()
for(i in velyka_novosilka_dates){
    velyka_novosilka_date_firms[[i]] <- tokmak_firms[as.Date(velyka_novosilka_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    velyka_novosilka_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(velyka_novosilka_date_firms[[i]]$frp), Axis="Velyka Novosilka")
}
velyka_novosilka_firms_summary <- as.data.frame(data.table::rbindlist(velyka_novosilka_means_firms))

bakhmut_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
bakhmut_firms <- new_firms_frame[new_firms_frame$latitude < 48.9 & new_firms_frame$latitude > 48.3 & new_firms_frame$longitude < 38.4 & new_firms_frame$longitude > 37.6,]
bakhmut_date_firms <- list()
bakhmut_means_firms <- list()
for(i in bakhmut_dates){
    bakhmut_date_firms[[i]] <- bakhmut_firms[as.Date(bakhmut_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    bakhmut_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(bakhmut_date_firms[[i]]$frp), Axis="Bakhmut")
}
bakhmut_firms_summary <- as.data.frame(data.table::rbindlist(bakhmut_means_firms))

kupyansk_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
kupyansk_firms <- new_firms_frame[new_firms_frame$latitude < 48.9 & new_firms_frame$latitude > 48.3 & new_firms_frame$longitude < 38.4 & new_firms_frame$longitude > 37.6,]
kupyansk_date_firms <- list()
kupyansk_means_firms <- list()
for(i in kupyansk_dates){
    kupyansk_date_firms[[i]] <- kupyansk_firms[as.Date(kupyansk_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    kupyansk_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(kupyansk_date_firms[[i]]$frp), Axis="Kupyansk")
}
kupyansk_firms_summary <- as.data.frame(data.table::rbindlist(kupyansk_means_firms))

avdiivka_dates = seq(as.Date("2022-02-23"), Sys.Date(), by="days")
avdiivka_firms <- new_firms_frame[new_firms_frame$latitude < 48.3 & new_firms_frame$latitude > 48.0 & new_firms_frame$longitude < 37.9 & new_firms_frame$longitude > 37.5,]
avdiivka_date_firms <- list()
avdiivka_means_firms <- list()
for(i in avdiivka_dates){
    avdiivka_date_firms[[i]] <- avdiivka_firms[as.Date(avdiivka_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    avdiivka_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(avdiivka_date_firms[[i]]$frp), Axis="Avdiivka")
}
avdiivka_firms_summary <- as.data.frame(data.table::rbindlist(avdiivka_means_firms))

axis_firms_summary <- as.data.frame(data.table::rbindlist(list(tokmak_firms_summary, velyka_novosilka_firms_summary, bakhmut_firms_summary, kupyansk_firms_summary, avdiivka_firms_summary)))

axis_firms_summary_plot <- ggplot(axis_firms_summary, aes(Date, FRP, colour=Axis)) +
  #geom_point() +
  geom_line() +
  #stat_smooth(method="gam") +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Fire Radiative Power (MegaWatts)", breaks=scales::pretty_breaks(n=10), labels=scales::comma) +
  ggtitle("FIRMS VIIRS I-Band 375 m Active Fire") +
  facet_wrap(.~Axis, ncol=1) +
  theme_light() +
  theme(legend.position = "none")

ggsave("~/Github/Russia-Ukraine/Plots/axis_firms_summary_plot.jpg", axis_firms_summary_plot, device="jpg", width=6, height=8, dpi=600)
