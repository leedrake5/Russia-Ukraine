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

source("~/GitHub/scrape_oryx/R/functions.R")

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



totals_fold <- function(totals_df=NULL, link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", date=NULL){
    if(is.null(totals_df)){
      totals_df <- totals_by_type(link=link, date=date)
    }
    
    totals_df <- totals_df[complete.cases(totals_df),]

      
    totals_df$equipment_type[totals_df$equipment_type %in% "All Types"] <- "Total"
    totals_df$equipment_type <- gsub(" ", "_", totals_df$equipment_type)
    totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Station"] <- "Communications_Stations"
    totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Vehicles"] <- "Communications_Stations"
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

googlesheets4::gs4_auth(email=TRUE)

googleSheetPush <- function(results, sheet_url="https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0"){

    #googlesheets4::gs4_auth()

  gsheet_totals <- googlesheets4::sheet_write(ss=sheet_url, data=results$Totals, sheet="Totals")
    gsheet_destroyed <- googlesheets4::sheet_write(ss=sheet_url, data=results$Destroyed, sheet="Destroyed")
    gsheet_damaged <- googlesheets4::sheet_write(ss=sheet_url, data=results$Damaged, sheet="Damaged")
    gsheet_abandoned <- googlesheets4::sheet_write(ss=sheet_url, data=results$Abandoned, sheet="Abandoned")
    gsheet_captured <- googlesheets4::sheet_write(ss=sheet_url, data=results$Captured, sheet="Captured")

}


daily_update <- function(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", to_return=NULL){
    dates = seq(as.Date("2022-02-24"), Sys.Date(), by="days")
    
    result_list <- list()
    for(i in dates[1:length(dates)-1]){
        result_list[[as.character(as.Date(i, format="%Y-%m-%d", origin="1970-01-01"))]] <- read.csv(paste0("~/GitHub/Russia-Ukraine/data/byType/", as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))[,-1]
    }
    result_list[[as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01"))]] <- totals_by_type(link=link, date=as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01")))
    write.csv(result_list[[as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01"))]], paste0("~/GitHub/Russia-Ukraine/data/byType/", as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01")), ".csv"))

    current_frame <- data.table::rbindlist(result_list, use.names=TRUE, fill=TRUE)
    results <- totals_fold(totals_df=current_frame)
    
    googleSheetPush(results)
    
    if(!is.null(to_return)){
        return(results)
    }
    
}

daily_update()
source("~/GitHub/Russia-Ukraine/R/losses.r")
