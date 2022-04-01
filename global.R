library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

dates = seq(as.Date("2022-02-24"), Sys.Date(), by="days")


daily_list <- list()

for(i in dates){
    
    daily_list[[as.Date(i, format="%Y-%m-%d", origin="1970-01-01")]] <- read.csv(paste0("data/bySystem/Raw/Daily/", as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))
    
}

daily_frame <- rbindlist(daily_list, use.names=TRUE, fill=TRUE)

daily_frame <- merge(daily_frame, read.csv("data/classes.csv"), by="system", all=TRUE)


total_by_system_wide <- function(indsn){
    tidy_frame <- indsn %>% dplyr::select(country, system, class, status, Date) %>%
    dplyr::group_by(country, system, class, status, Date) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = status, values_from = count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
                  total = destroyed + captured + damaged + abandoned)
                  
    
    return(tidy_frame)
}

