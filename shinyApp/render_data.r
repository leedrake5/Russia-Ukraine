library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(shinythemes)
library(DT)
library(R.utils)
library(pbapply)
library(parallel)
shiny::devmode(TRUE)
options(shiny.fullstacktrace=TRUE)

country_colors <-   c("Russia" = "#E4181C", "Ukraine" = "#0057B8")

full_data <- read.csv("data/2023-04-20.csv")

dates = seq(as.Date("2022-02-24"), as.Date("2023-04-20"), by="days")


daily_list <- list()

for(i in dates){
    
    daily_list[[as.character(as.Date(i, format="%Y-%m-%d", origin="1970-01-01"))]] <- read.csv(paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/", as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))
    
}

daily_frame <- rbindlist(daily_list, use.names=TRUE, fill=TRUE)

daily_frame <- as.data.frame(merge(daily_frame, read.csv("~/GitHub/shinyApp/data/classes.csv")[,-1], by="system", all=TRUE, fill=TRUE, allow.cartesian=TRUE))
daily_frame <- daily_frame[,!colnames(daily_frame) %in% c("date_recorded", "X")]
daily_frame <- daily_frame[!is.na(daily_frame$status),]
daily_frame <- daily_frame[order(as.Date(daily_frame$Date, format="%m-%d-%Y")),]

number_string <- as.character(seq(1, 500, 1))
daily_frame_system_list <- sapply(daily_frame$system, function(x) strsplit(x, " ")[[1]])
names(daily_frame_system_list) <- daily_frame$system

daily_frame_system_fixed_list <- list()
for(i in 1:length(daily_frame_system_list)){
    if(daily_frame_system_list[[i]][1] %in% number_string){
        daily_frame_system_fixed_list[[i]] <- paste(daily_frame_system_list[[i]][2:length(daily_frame_system_list[[i]])], collapse=" ")
    } else {
        daily_frame_system_fixed_list[[i]] <- paste(daily_frame_system_list[[i]], collapse=" ")
    }
}

daily_frame$system <- as.vector(unlist(daily_frame_system_fixed_list))

write.csv(daily_frame, "~/GitHub/Russia-Ukraine/shinyApp/data/daily_frame.csv")
