list.of.packages <- c("shinythemes", "ggplot2", "scales", "data.table", "magrittr", "dplyr", "tidyr", "lubridate", "zoo", "DT", "R.utils")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE, ask=FALSE, type="binary"))

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


full_data <- read.csv(paste0("data/bySystem/Raw/Full/", Sys.Date()-1, ".csv"))

dates = seq(as.Date("2022-02-24"), Sys.Date()-1, by="days")


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


dupuySample <- function(seed=1, strength_ru=180, strength_ukr=77, ru_strength_modifier=0.2, ru_strength_lock=FALSE, ukr_strength_modifier=0.2, ukr_strength_lock=FALSE, terrain_ru=1.4, terrain_ukr=1.5, ru_terrain_modifier=0.1, ukr_terrain_modifier=0.1, season_ru=1.1, season_ukr=1.1, ru_season_modifier=0.1, ukr_season_modifier=0.1, posture_ru=1.5, posture_ukr=1.5, ru_posture_modifier=0.1, ukr_posture_modifier=0.1, air_ru=0.8, air_ukr=1, ru_air_modifier=0.1, ukr_air_modifier=0.1, morale_ru=0.8, morale_ukr=1, ru_morale_modifier=0.2, ukr_morale_modifier=0.2){
    
    set.seed(seed)
    if(ru_strength_modifier<=1){
        ru_rand_strength <- rnorm(1, strength_ru, strength_ru*ru_strength_modifier)
    } else if(ru_strength_modifier>1){
        ru_rand_strength <- rnorm(1, strength_ru, ru_strength_modifier)
    }
    if(ru_strength_lock==TRUE){
        if(ru_rand_strength>strength_ru){
            ru_rand_strength <- strength_ru - (ru_rand_strength-strength_ru)
        }
    }
    ru_rand_terrain <- rnorm(1, terrain_ru, ru_terrain_modifier)
    ru_rand_season <- rnorm(1, season_ru, ru_season_modifier)
    ru_rand_posture <- rnorm(1, posture_ru, ru_posture_modifier)
    ru_rand_air <- rnorm(1, air_ru, ru_air_modifier)
    ru_rand_morale <- rnorm(1, morale_ru, ru_morale_modifier)
    if(ru_rand_morale > 1){
        ru_rand_morale <- 1
    }
    if(ukr_strength_modifier<=1){
        ukr_rand_strength <- rnorm(1, strength_ukr, strength_ukr*ukr_strength_modifier)
    } else if(ukr_strength_modifier>1){
        ukr_rand_strength <- rnorm(1, strength_ukr, ukr_strength_modifier)
    }
    if(ukr_strength_lock==TRUE){
        if(ukr_rand_strength>strength_ru){
            ukr_rand_strength <- strength_ukr - (ukr_rand_strength-strength_ukr)
        }
    }
    ukr_rand_terrain <- rnorm(1, terrain_ukr, ukr_terrain_modifier)
    ukr_rand_season <- rnorm(1, season_ukr, ukr_season_modifier)
    ukr_rand_posture <- rnorm(1, posture_ukr, ukr_posture_modifier)
    ukr_rand_air <- rnorm(1, air_ukr, ukr_air_modifier)
    ukr_rand_morale <- rnorm(1, morale_ukr, ukr_morale_modifier)
    if(ukr_rand_morale > 1){
        ukr_rand_morale <- 1
    }
    ru_outcome <-  ru_rand_strength + ru_rand_terrain + ru_rand_season + ru_rand_posture + ru_rand_air + ru_rand_morale
    ukr_outcome <- ukr_rand_strength + ukr_rand_terrain + ukr_rand_season + ukr_rand_posture + ukr_rand_air + ukr_rand_morale
    
    outcome <- ru_outcome/ukr_outcome
    
    return(list(Outcome=outcome, Russian_Outcome=ru_outcome, Ukranian_Outcome=ukr_outcome, Russian_Strength=ru_rand_strength, Russian_Terrain=ru_rand_terrain, Russian_Posture=ru_rand_posture, Russian_Air=ru_rand_air, Russian_Morale=ru_rand_morale, Ukrainian_Strength=ukr_rand_strength, Ukrainian_Terrain=ukr_rand_terrain, Ukrainian_Posture=ukr_rand_posture, Ukrainian_Air=ukr_rand_air, Ukrainian_Morale=ukr_rand_morale))
}

#outcome_list <- NULL
#outcome_list <- pbapply::pblapply(1:100000, function(x) dupeySample(seed=x, strength_modifier=50, morale_ru=0.8), cl=8)

#just_outcomes <- sapply(outcome_list, function(x) x[[1]])

#ggplot() +
#geom_vline(xintercept=1, lty=2) +
#geom_density(aes(x=just_outcomes), fill="grey80", alpha=0.5) +
#scale_x_continuous("Dupoy's Ratio", limits=c(0, 5)) +
#theme_light()

#table(just_outcomes>1)


