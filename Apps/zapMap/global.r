library(dplyr)
library(rgdal)

lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    
    
    as.character(as.expression(eq));
}

data <- read.csv("data/zaporizhizhia.csv")
firms <- read.csv("data/current_firms.csv")

data$Date <- as.Date(data$Date)
firms$acq_date <- as.Date(firms$acq_date)

data <- data[complete.cases(data$Geolocation),]
data$Latitude <- as.numeric(sapply(data$Geolocation, function(x) strsplit(x, split=",")[[1]][1]))
data$Longitude <- as.numeric(sapply(data$Geolocation, function(x) strsplit(x, split=",")[[1]][2]))
