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

list.of.packages <- c("ggplot2", "RCurl", "reshape2", "data.table", "gsheet", "dplyr", "tidyverse", "lubridate", "scales", "rvest", "do")
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
library(dplyr)
library(tidyverse)
library(lubridate)
library(scales)
library(rvest)
library(do)

source("~/GitHub/scrape_oryx/R/functions.R")

source("~/GitHub/Russia-Ukraine/R/Automation/back.r")

googlesheets4::gs4_auth_configure(api_key=api_key)

#' totals_by_type
#' @description Gets data by system category.
#'
#' @return a tibble
create_by_type <- function(country, date=NULL) {
    
    if(is.null(date)){
        date <- format(Sys.Date(), "%m/%d/%Y")
     }
    
  if (country == "Russia") {
    url <-
      russia_url
  } else {
    url <-
      ukraine_url
  }

  heads <-
    get_data(
      url,
      "article div"
    ) %>%
    rvest::html_elements("h3") %>%
    rvest::html_text2()

  # Drop the empty cell padding
  heads <- heads[nchar(heads) > 0]

  # Get the positons of the Russia and Ukraine headers
  pos <- heads %>% stringr::str_which(country) %>% as.double()

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


  country_df <- totals %>%
    dplyr::mutate(
      dplyr::across(destroyed:damaged, ~ as.double(tidyr::replace_na(.x, "0"))),
      type_total = destroyed + abandoned + captured + damaged,
      row_id = 1:n()
    ) %>%
    dplyr::mutate(country = tidyr::replace_na(country, !!!country)) %>%
    #select(-row_id) %>%
    dplyr::mutate(
      equipment = replace(equipment, pos, "All Types"),
    ) %>%
    dplyr::rename(equipment_type = equipment)
    
    country_df <- as.data.frame(country_df)
    country_df$Date <- date

  return(country_df)
}

totals_by_type <- function(date=NULL) {
  russia <- create_by_type("Russia", date=date)
  ukraine <- create_by_type("Ukraine",date=date)

  totals_df <- russia %>%
    dplyr::bind_rows(ukraine, .id=NULL)

  return(totals_df)
}



totals_fold <- function(totals_df=NULL, russia_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", ukraine_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html", date=NULL){
    if(is.null(totals_df)){
      totals_df <- totals_by_type(date=date)
    }
    
    totals_df <- totals_df[complete.cases(totals_df),]

        totals_df$equipment_type <- as.vector(sapply(totals_df$equipment_type, function(x) strsplit(x, " (", fixed=TRUE)[[1]][1]))
        totals_df <- totals_df[!grep(")", totals_df$equipment_type, fixed=TRUE),]
        totals_df <- totals_df[!grep("(", totals_df$equipment_type, fixed=TRUE),]

        totals_df$equipment_type[totals_df$equipment_type %in% "All Types"] <- "Total"
        totals_df$equipment_type <- gsub(" ", "_", totals_df$equipment_type)
        totals_df$equipment_type[totals_df$equipment_type %in% "Trucks,_Vehicles,_and_Jeeps"] <- "Trucks,_Vehicles_and_Jeeps"
        totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Station"] <- "Communications_Stations"
        totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Vehicles"] <- "Communications_Stations"
        totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Stations"] <- "Command_Posts_And_Communications_Stations"
        totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Stations"] <- "Command_Posts_And_Communications_Stations"
        totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Stations"] <- "Command_Posts_And_Communications_Stations"
        totals_df$equipment_type[totals_df$equipment_type %in% "Engineering_Vehicles"] <- "Engineering_Vehicles_And_Equipment"
        totals_df$equipment_type[totals_df$equipment_type %in% "Mine-resistant_ambush_protected"] <- "Mine-Resistant_Ambush_Protected"
        totals_df$equipment_type[totals_df$equipment_type %in% "Mine-resistant_Ambush_Protected"] <- "Mine-Resistant_Ambush_Protected"
        totals_df$equipment_type[totals_df$equipment_type %in% "Self-propelled_Anti-Aircraft_Guns"] <- "Self-Propelled_Anti-Aircraft_Guns"
        totals_df$equipment_type[totals_df$equipment_type %in% "Self-propelled_artillery"] <- "Self-Propelled_Artillery"
        totals_df$equipment_type[totals_df$equipment_type %in% "Surface-to-air_missile_systems"] <- "Surface-To-Air_Missile_Systems"
        totals_df$equipment_type[totals_df$equipment_type %in% "Anti-tank_Guided_Missiles"] <- "Anti-Tank_Guided_Missiles"
        totals_df$equipment_type[totals_df$equipment_type %in% "Artillery"] <- "Towed_Artillery"
        totals_df$equipment_type[totals_df$equipment_type %in% "Mortars"] <- "Heavy_Mortars"
    colnames(totals_df) <- c("Country", "EquipmentType", "Destroyed", "Abandoned", "Captured", "Damaged", "Quantity", "Date")
    totals_df$Type <- gsub("\n", "", paste0(totals_df$Country, "_", totals_df$EquipmentType))
    totals_df$Type[totals_df$Type %in% "Ukraine_Surface-to-air_missile_systems"] <- "Ukraine_Surface-To-Air_Missile_Systems"
    totals_df$Type[totals_df$Type %in% "Ukraine_Radars"] <- "Ukraine_Radars_And_Communications_Equipment"
    totals_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Quantity", fun.aggregate = mean)
    totals_formatted <- totals_formatted[,!colnames(totals_formatted) %in% c("Russia_", "Ukraine_")]
    colnames(totals_formatted) <- gsub("\n", "", colnames(totals_formatted))
    totals_formatted[is.na(totals_formatted)] <- 0
    destroyed_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Destroyed", fun.aggregate = mean)
    destroyed_formatted <- destroyed_formatted[,!colnames(destroyed_formatted) %in% c("Russia_", "Ukraine_")]
    colnames(destroyed_formatted) <- gsub("\n", "", colnames(destroyed_formatted))
    destroyed_formatted[is.na(destroyed_formatted)] <- 0
    damaged_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Damaged", fun.aggregate = mean)
    damaged_formatted <- damaged_formatted[,!colnames(damaged_formatted) %in% c("Russia_", "Ukraine_")]
    colnames(damaged_formatted) <- gsub("\n", "", colnames(damaged_formatted))
    damaged_formatted[is.na(damaged_formatted)] <- 0
    captured_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Captured", fun.aggregate = mean)
    captured_formatted <- captured_formatted[,!colnames(captured_formatted) %in% c("Russia_", "Ukraine_")]
    colnames(captured_formatted) <- gsub("\n", "", colnames(captured_formatted))
    captured_formatted[is.na(captured_formatted)] <- 0
    abandoned_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Abandoned", fun.aggregate = mean)
    abandoned_formatted <- abandoned_formatted[,!colnames(abandoned_formatted) %in% c("Russia_", "Ukraine_")]
    colnames(abandoned_formatted) <- gsub("\n", "", colnames(abandoned_formatted))
    abandoned_formatted[is.na(abandoned_formatted)] <- 0
      return(list(Totals=totals_formatted, Destroyed=destroyed_formatted, Damaged=damaged_formatted, Captured=captured_formatted, Abandoned=abandoned_formatted))
}


googleSheetPush <- function(results, sheet_url="https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI"){

    #googlesheets4::gs4_auth()

    gsheet_totals <- googlesheets4::sheet_write(ss=sheet_url, data=results$Totals, sheet="Totals")
    gsheet_destroyed <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Destroyed, sheet="Destroyed"), error=function(e) NULL)
    gsheet_damaged <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Damaged, sheet="Damaged"), error=function(e) NULL)
    gsheet_abandoned <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Abandoned, sheet="Abandoned"), error=function(e) NULL)
    gsheet_captured <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Captured, sheet="Captured"), error=function(e) NULL)
    gsheet_totals <- googlesheets4::sheet_write(ss=sheet_url, data=results$Totals, sheet="Totals")
    gsheet_destroyed <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Destroyed, sheet="Destroyed"), error=function(e) NULL)
    gsheet_damaged <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Damaged, sheet="Damaged"), error=function(e) NULL)
    gsheet_abandoned <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Abandoned, sheet="Abandoned"), error=function(e) NULL)
    gsheet_captured <- tryCatch(googlesheets4::sheet_write(ss=sheet_url, data=results$Captured, sheet="Captured"), error=function(e) NULL)
}


daily_update <- function(russia_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", ukraine_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html", to_return=NULL){
    dates = seq(as.Date("2022-02-24"), Sys.Date(), by="days")
    
    result_list <- list()
    for(i in dates[1:length(dates)-1]){
        result_list[[as.character(as.Date(i, format="%Y-%m-%d", origin="1970-01-01"))]] <- read.csv(paste0("~/GitHub/Russia-Ukraine/data/byType/", as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))[,-1]
    }
    result_list[[as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01"))]] <- totals_by_type(date=as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01")))
    write.csv(result_list[[as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01"))]], paste0("~/GitHub/Russia-Ukraine/data/byType/", as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01")), ".csv"))

    current_frame <- data.table::rbindlist(result_list, use.names=TRUE, fill=TRUE)
    current_frame <- as.data.frame(current_frame)
    current_frame <- current_frame[,colnames(current_frame)[!colnames(current_frame) %in% "row_id"]]
    current_frame <- as.data.table(current_frame)
    results <- totals_fold(totals_df=current_frame)
    
    googleSheetPush(results)
    
    if(!is.null(to_return)){
        return(results)
    }
    
}

russia_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html"
ukraine_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html"
russia_url = russia_link
ukraine_url = ukraine_link

scrape_data <- function(country, date=NULL) {
    
    if(is.null(date)){
        date <- format(Sys.Date(), "%m/%d/%Y")
     }
    
  if (country == "Russia") {
    url <-
      russia_url
  } else {
    url <-
      ukraine_url
  }

  materiel <-
    get_data(url,
             "article") %>%
    rvest::html_elements("li")

  data <-
    tibble::tibble(
      country = character(),
      origin = character(),
      system = character(),
      status = character(),
      url = character()
    )

  counter = 0
  for (a in seq_along(materiel)) {
    status <- materiel[[a]] %>% rvest::html_elements("a")
    for (b in seq_along(status)) {
      counter = counter + 1
      data[counter, 1] <- country
      data[counter, 2] <- extract_origin(materiel, a)
      data[counter, 3] <- extract_system(materiel, a)
      data[counter, 4] <- extract_status(status, b)
      data[counter, 5] <- extract_url(status, b)
    }
  }

  data <- data %>%
    dplyr::mutate(status = stringr::str_extract_all(status, "destroyed|captured|abandoned|damaged")) %>%
    tidyr::unnest_longer(status) %>%
    dplyr::mutate(date_recorded = as.Date(lubridate::today())) %>%
    trim_all()
    
    data <- as.data.frame(data)
    data$Date <- date
    
    return(data)
}

create_data <- function() {
  russia <- scrape_data("Russia")
  ukraine <- scrape_data("Ukraine")

  data <- russia %>%
    dplyr::bind_rows(ukraine) %>%
    dplyr::select(country, origin, system, status, url, date_recorded)

  data <- create_keys(data) %>%
    dplyr::group_by(matID) %>%
    dplyr::filter(date_recorded == min(date_recorded)) %>%
    dplyr::ungroup()

  return(data)

}

#' scrape_data
#' @description Gets data by system.
#'
#' @return a tibble
#' @export
scrape_data <- function(russia_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", ukraine_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html", date=NULL, remove=NULL) {
    
    create_data()
        
}

#' scrape_data
#' @description Gets data by system.
#'
#' @return a tibble
#' @export
scrape_data <- function(russia_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", ukraine_link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html", date=NULL, remove=NULL) {
    
    if(is.null(date)){
        date <- format(Sys.Date(), "%m/%d/%Y")
     }
    russia_materiel <-
      get_data(
        russia_link,
        "article"
      ) %>%
      rvest::html_elements("li")
      
      ukraine_materiel <-
        get_data(
        ukraine_link,
          "article"
        ) %>%
        rvest::html_elements("li")
        
        materiel <- c(russia_materiel, ukraine_materiel)

    # Retreive the start position of each country
    country_pos <- materiel %>% rvest::html_text2() %>%
      # T-64BV is the first row in the tank list and marks the beginning of each country
      stringr::str_which("T-64BV")

    #' Run Program
    data <-
      tibble::tibble(
        country = character(),
        origin = character(),
        system = character(),
        status = character(),
        url = character()
      )

    counter = 0
    for (a in seq_along(materiel)) {
      status <- materiel[[a]] %>% rvest::html_elements("a")
      for (b in seq_along(status)) {
        counter = counter + 1
        data[counter, 1] <-
          ifelse(a < country_pos[2], "Russia", "Ukraine")
        data[counter, 2] <- extract_origin(materiel, a)
        data[counter, 3] <- extract_system(materiel, a)
        data[counter, 4] <- extract_status(status, b)
        data[counter, 5] <- extract_url(status, b)
      }
    }

    data <- data %>%
      dplyr::mutate(status = stringr::str_extract_all(status, "destroyed|captured|abandoned|damaged")) %>%
      tidyr::unnest_longer(status) %>%
      dplyr::mutate(date_recorded = as.Date(lubridate::today())) %>%
      trim_all()

    #previous <- get_inputfile("totals_by_system") %>%
    #  trim_all() %>%
    #  dplyr::mutate(date_recorded = as.Date(date_recorded))

    #check <- data %>%
    #  dplyr::anti_join(previous, by = c("url")) %>%
    #  dplyr::mutate(date_recorded = as.Date(date_recorded))

    #if (nrow(check) > 0) {
    #  data <-
    #    check %>% dplyr::bind_rows(readr::read_csv(
    #      glue::glue("inputfiles/totals_by_system{lubridate::today()}.csv")
    #    )) %>%
    #    dplyr::arrange(country, system, date_recorded)

    #data <- check %>% dplyr::bind_rows(get_inputfile("totals_by_system")) %>%
    #  dplyr::arrange(country, system, date_recorded)

    #previous %>% readr::write_csv("inputfiles/totals_by_system.csv.bak")

    #data %>% readr::write_csv(glue::glue("inputfiles/totals_by_system{lubridate::today()+1}.csv"))

    #} else {
    #  logr::put("No new data")
    #  data <- previous
    #}

    data <- create_keys(data) %>%
      dplyr::group_by(matID) %>%
      dplyr::filter(date_recorded == min(date_recorded)) %>%
      dplyr::ungroup()
      
      data <- as.data.frame(data)
      data$Date <- date
    
    if(!is.null(remove)){
        data$url <- gsub(remove, "", data$url)
    }

  return(data)

}

total_by_system_wide <- function(indsn, date=NULL){
    if(is.null(date)){
        date <- unique(indsn$Date)
    }
    tidy_frame <- indsn %>% dplyr::select(country, system, status) %>%
    dplyr::group_by(country, system, status) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::pivot_wider(names_from = status, values_from = count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
                  total = destroyed + captured + damaged + abandoned)
    tidy_frame$Date <- date
    return(tidy_frame)
}

systems <- scrape_data()
write.csv(systems, paste0("/Users/lee/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/", Sys.Date(), ".csv"))
previous_day <- read.csv(paste0("/Users/lee/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/", Sys.Date()-1, ".csv"))
daily_systems <- systems[!systems$url %in% previous_day$url, ]
write.csv(daily_systems, paste0("/Users/lee/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/", Sys.Date(), ".csv"))

tidy_systems <- total_by_system_wide(systems)
write.csv(tidy_systems, paste0("/Users/lee/GitHub/Russia-Ukraine/data/bySystem/Totals/Full/", Sys.Date(), ".csv"))

daily_update()
source("~/GitHub/Russia-Ukraine/R/Automation/losses.r")
source("~/GitHub/Russia-Ukraine/R/Automation/losses_june_01_2023.r")
source("~/GitHub/Russia-Ukraine/R/Automation/losses_zaporizhizhia.r")
