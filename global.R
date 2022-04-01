list.of.packages <- c("shinythemes", "ggplot2", "scales", "data.table", "magrittr", "dplyr", "tidyr", "lubridate", "zoo", "DT", "R.utils")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE, ask=FALSE, type="binary"))

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

full_data <- read.csv(paste0("data/bySystem/Raw/Full/", Sys.Date(), ".csv"))

dates = seq(as.Date("2022-02-24"), Sys.Date(), by="days")


daily_list <- list()

for(i in dates){
    
    daily_list[[as.Date(i, format="%Y-%m-%d", origin="1970-01-01")]] <- read.csv(paste0("data/bySystem/Raw/Daily/", as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))
    
}

daily_frame <- rbindlist(daily_list, use.names=TRUE, fill=TRUE)

daily_frame <- as.data.frame(merge(daily_frame, read.csv("data/classes.csv"), by="system", all=TRUE))
daily_frame <- daily_frame[,!colnames(daily_frame) %in% c("date_recorded", "X")]
daily_frame <- daily_frame[!is.na(daily_frame$status),]



total_by_system_wide <- function(indsn){
    tidy_frame <- indsn %>% dplyr::select(country, system, class, status, Date) %>%
    dplyr::group_by(country, system, class, status, Date) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = status, values_from = count) %>%
    dplyr::ungroup()
    
    if(!"destroyed" %in% names(tidy_frame)){
        tidy_frame$destroyed <- 0
    }
    if(!"captured" %in% names(tidy_frame)){
        tidy_frame$captured <- 0
    }
    if(!"abandoned" %in% names(tidy_frame)){
        tidy_frame$abandoned <- 0
    }
    if(!"damaged" %in% names(tidy_frame)){
        tidy_frame$damaged <- 0
    }
    tidy_frame <- tidy_frame %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
                  total = destroyed + captured + damaged + abandoned)
                  
    return(tidy_frame)
}

date_crunch <- function(indsn){
    
    tidy_frame <- indsn %>% dplyr::select(country, system, class, status, Date) %>%
    dplyr::group_by(country, status, Date) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = status, values_from = count) %>%
    dplyr::ungroup()
    
    if(!"destroyed" %in% names(tidy_frame)){
        tidy_frame$destroyed <- 0
    }
    if(!"captured" %in% names(tidy_frame)){
        tidy_frame$captured <- 0
    }
    if(!"abandoned" %in% names(tidy_frame)){
        tidy_frame$abandoned <- 0
    }
    if(!"damaged" %in% names(tidy_frame)){
        tidy_frame$damaged <- 0
    }
    tidy_frame <- tidy_frame %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
                  total = destroyed + captured + damaged + abandoned)
                  
    return(tidy_frame)
    
}


#' totals_by_type
#' @description Gets data by system category.
#'
#' @return a tibble
totals_by_type <- function(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", date=NULL) {
    
    if(is.null(date)){
        date <- format(Sys.Date(), "%m/%d/%Y")
     }
    
    heads <-
    get_data(
      link,
      "article div"
    ) %>%
    rvest::html_elements("h3") %>%
    rvest::html_text2()

  # Drop the empty cell padding
  heads <- heads[nchar(heads) > 0]

  # Get the positons of the Russia and Ukraine headers
  rus_pos <- heads %>% stringr::str_which("Russia") %>% as.double()
  ukr_pos <- heads %>% stringr::str_which("Ukraine") %>% as.double()

  totals <- tibble(
    country = character(),
    equipment = character(),
    destroyed = character(),
    abandoned = character(),
    captured = character(),
    damaged = character()
  )

  for (l in seq_along(heads)) {
    totals[l, "equipment"] <-
      heads[l] %>% stringr::str_remove_all(" \\(.*\\)")
    totals[l, "destroyed"] <-
      heads[l] %>% stringr::str_extract("destroyed: \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "abandoned"] <-
      heads[l] %>% stringr::str_extract("(abandoned|aboned): \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "captured"] <-
      heads[l] %>% stringr::str_extract("captured: \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "damaged"] <-
      heads[l] %>% stringr::str_extract("damaged: \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
  }


  totals_df <- totals %>%
    dplyr::mutate(
      dplyr::across(destroyed:damaged, ~ as.double(tidyr::replace_na(.x, "0"))),
      type_total = destroyed + abandoned + captured + damaged,
      row_id = 1:n(),
      country = dplyr::case_when(row_id < ukr_pos ~ "Russia",
                                 row_id >= ukr_pos ~ "Ukraine")
    ) %>%
    select(-row_id) %>%
    dplyr::mutate(
      equipment = replace(equipment, rus_pos, "All Types"),
      equipment = replace(equipment, ukr_pos, "All Types")
    ) %>%
    dplyr::rename(equipment_type = equipment)
    totals_df <- as.data.frame(totals_df)
    totals_df$Date <- date
    
  return(totals_df)
}


