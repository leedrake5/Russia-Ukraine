library(dplyr)
#library(rgdal)

lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    
    
    as.character(as.expression(eq));
}

data <- read.csv("data/zaporizhizhia.csv")
avdiivka_data <- read.csv("data/avdiivka.csv")
krynky_data <- read.csv("data/krynky.csv")
kharkiv_data <- read.csv("data/kharkiv.csv")
kursk_data <- read.csv("data/kursk.csv")
data <- as.data.frame(data.table::rbindlist(list(data, avdiivka_data, krynky_data, kharkiv_data, kursk_data), use.names=T, fill=T))
data$Date <- as.Date(data$Date)
data$ID <- paste0(data$Country, "_", seq(1, nrow(data), 1))

data <- data[complete.cases(data$Geolocation),]
data$lat <- as.numeric(sapply(data$Geolocation, function(x) strsplit(x, split=",")[[1]][1]))
data$lng <- as.numeric(sapply(data$Geolocation, function(x) strsplit(x, split=",")[[1]][2]))

firms <- read.csv("data/current_firms.csv")
firms$acq_date <- as.Date(firms$acq_date)

