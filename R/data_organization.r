source("~/GitHub/scrape_oryx/R/functions.R")

oryx_scrape <- function(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv", return="Raw"){
    
    class_guide <- read.csv(class_guide_file)
    
    oryx <- rvest::read_html(
    link
    ) %>% rvest::html_elements("article")
    
    materiel <- oryx %>% rvest::html_elements("li")

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
          url = character(),
        )
        
        counter = 0
        for (a in seq_along(materiel)) {
          status <- materiel[[a]] %>% rvest::html_elements("a")
          for (b in seq_along(status)) {
            counter = counter + 1
            data[counter, 1] <- ifelse(a < country_pos[2], "Russia", "Ukraine")
            data[counter, 2] <- extract_origin(materiel, a)
            data[counter, 3] <- extract_system(materiel, a)
            data[counter, 4] <- extract_status(status, b)
            data[counter, 5] <- extract_url(status, b)
          }
        }
        
        data <- merge(data, class_guide, by="system")
        
        
        data_wide <- data %>%
          dplyr::select(country, system, status) %>%
          dplyr::group_by(country, system, status) %>%
          dplyr::summarise(count = n())
        data_wide <- merge(data_wide, class_guide, by="system")
      
      if(return=="Raw"){
          return(data)
      } else if(return=="Ready"){
          return(data_wide)
      }
}

summaries <- function(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv", date){
    
    day_data <- oryx_scrape(link=link, class_guide_file=class_guide_file, return="Ready")
    
    short_data <- data.frame(Country=day_data$country, Class=day_data$class, Status=day_data$status, Count=day_data$count)
    
    status_summary <- data.frame(data.table::as.data.table(short_data)[, lapply(.SD, sum, na.rm=TRUE), by=list(Country, Class, Status) ], Date=date)
    
    simple_summary <- as.data.frame(data.table::as.data.table(short_data[,c("Country", "Class", "Count")])[, lapply(.SD, sum, na.rm=TRUE), by=list(Country, Class) ])
    
    simple_summary <- data.frame(Date=date, simple_summary[order(simple_summary$Class, simple_summary$Country),])
    
    return(list(Status=status_summary, Simple=simple_summary))
}

today <- oryx_scrape(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv")

feb_24 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220224231142/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="02/24/2022")
write.csv(feb_24, "~/GitHub/Russia-Ukraine/Dates Raw/Feb242022.csv")
feb_25 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220225233528/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="02/25/2022")
write.csv(feb_25, "~/GitHub/Russia-Ukraine/Dates Raw/Feb252022.csv")
feb_26 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220226185336/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="02/26/2022")
write.csv(feb_26, "~/GitHub/Russia-Ukraine/Dates Raw/Feb262022.csv")
feb_27 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220227175728/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="02/27/2022")
write.csv(feb_27, "~/GitHub/Russia-Ukraine/Dates Raw/Feb272022.csv")
feb_28 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220228231935/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="02/28/2022")
write.csv(feb_28, "~/GitHub/Russia-Ukraine/Dates Raw/Feb282022.csv")
mar_01 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220301185329/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/01/2022")
write.csv(mar_01, "~/GitHub/Russia-Ukraine/Dates Raw/Mar012022.csv")
mar_02 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220302205559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/02/2022")
write.csv(mar_02, "~/GitHub/Russia-Ukraine/Dates Raw/Mar022022.csv")
mar_03 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220303195154/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/03/2022")
write.csv(mar_03, "~/GitHub/Russia-Ukraine/Dates Raw/Mar032022.csv")
mar_04 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220304235636/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/04/2022")
write.csv(mar_04, "~/GitHub/Russia-Ukraine/Dates Raw/Mar042022.csv")
mar_05 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220305211400/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/05/2022")
write.csv(mar_05, "~/GitHub/Russia-Ukraine/Dates Raw/Mar052022.csv")
mar_06 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220306205522/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/06/2022")
write.csv(mar_06, "~/GitHub/Russia-Ukraine/Dates Raw/Mar062022.csv")
mar_07 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220307164915/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/07/2022")
write.csv(mar_07, "~/GitHub/Russia-Ukraine/Dates Raw/Mar072022.csv")
mar_08 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220308204303/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/08/2022")
write.csv(mar_08, "~/GitHub/Russia-Ukraine/Dates Raw/Mar082022.csv")
mar_09 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220309213817/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/09/2022")
write.csv(mar_09, "~/GitHub/Russia-Ukraine/Dates Raw/Mar092022.csv")
mar_10 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220310201012/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/10/2022")
write.csv(mar_10, "~/GitHub/Russia-Ukraine/Dates Raw/Mar102022.csv")
mar_11 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220311205005/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/11/2022")
write.csv(mar_11, "~/GitHub/Russia-Ukraine/Dates Raw/Mar112022.csv")
mar_12 <- data.frame(oryx_scrape(link="https://web.archive.org/web/20220312181142/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", class_guide_file="~/GitHub/scrape_oryx/classes.csv"), Date="03/12/2022")
write.csv(mar_12, "~/GitHub/Russia-Ukraine/Dates Raw/Mar122022.csv")



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
      heads[l] %>% stringr::str_extract("destroyed: ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "abandoned"] <-
      heads[l] %>% stringr::str_extract("(abandoned|aboned): ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "captured"] <-
      heads[l] %>% stringr::str_extract("captured: ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "damaged"] <-
      heads[l] %>% stringr::str_extract("damaged: ...") %>%
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
    totals_df$Date <- date

  return(totals_df)
}


totals_fold <- function(totals_df=NULL, link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", date=NULL){   
    if(is.null(totals_df)){
      totals_df <- totals_by_type(link=link, date=date) 
    }
    

      
    totals_df$equipment_type[totals_df$equipment_type %in% "All Types"] <- "Total"
    totals_df$equipment_type <- gsub(" ", "_", totals_df$equipment_type)
    totals_df$equipment_type[totals_df$equipment_type %in% "Communications_Station"] <- "Communications_Stations"
        totals_df$equipment_type[totals_df$equipment_type %in% "Engineering_Vehicles"] <- "Engineering_Vehicles_And_Equipment"
        totals_df$equipment_type[totals_df$equipment_type %in% "Mine-resistant_ambush_protected"] <- "Mine-Resistant_Ambush_Protected"
        totals_df$equipment_type[totals_df$equipment_type %in% "Mine-resistant_Ambush_Protected"] <- "Mine-Resistant_Ambush_Protected"
        totals_df$equipment_type[totals_df$equipment_type %in% "Self-propelled_Anti-Aircraft_Guns"] <- "Self-Propelled_Anti-Aircraft_Guns"
        totals_df$equipment_type[totals_df$equipment_type %in% "Self-propelled_artillery"] <- "Self-Propelled_Artillery"
        totals_df$equipment_type[totals_df$equipment_type %in% "Surface-to-air_missile_systems"] <- "Surface-To-Air_Missile_Systems" 
        totals_df$equipment_type[totals_df$equipment_type %in% "Anti-tank_Guided_Missiles"] <- "Anti-Tank_Guided_Missiles"   
        totals_df$equipment_type[totals_df$equipment_type %in% "Artillery"] <- "Towed_Artillery"    
    colnames(totals_df) <- c("Country", "EquipmentType", "Destroyed", "Abandoned", "Captured", "Damaged", "Quantity", "Date")
    totals_df$Type <- paste0(totals_df$Country, "_", totals_df$EquipmentType)  
    totals_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Quantity", fun.aggregate = mean)
    totals_formatted <- totals_formatted[,!colnames(totals_formatted) %in% c("Russia_\n", "Ukraine_\n")]
    totals_formatted[is.na(totals_formatted)] <- 0
    destroyed_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Destroyed", fun.aggregate = mean)
    destroyed_formatted <- destroyed_formatted[,!colnames(destroyed_formatted) %in% c("Russia_\n", "Ukraine_\n")]
    destroyed_formatted[is.na(destroyed_formatted)] <- 0
    damaged_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Damaged", fun.aggregate = mean)
    damaged_formatted <- damaged_formatted[,!colnames(damaged_formatted) %in% c("Russia_\n", "Ukraine_\n")]
    damaged_formatted[is.na(damaged_formatted)] <- 0
    captured_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Captured", fun.aggregate = mean)
    captured_formatted <- captured_formatted[,!colnames(captured_formatted) %in% c("Russia_\n", "Ukraine_\n")]
    captured_formatted[is.na(captured_formatted)] <- 0
    abandoned_formatted <- reshape2::dcast(data=totals_df, formula=Date~Type, value.var="Abandoned", fun.aggregate = mean)  
    abandoned_formatted <- abandoned_formatted[,!colnames(abandoned_formatted) %in% c("Russia_\n", "Ukraine_\n")] 
    abandoned_formatted[is.na(abandoned_formatted)] <- 0
      return(list(Totals=totals_formatted, Destroyed=destroyed_formatted, Damaged=damaged_formatted, Captured=captured_formatted, Abandoned=abandoned_formatted))  
}

dates = seq(as.Date("2022-02-24"), Sys.Date(), by="days")
links = c("https://web.archive.org/web/20220224231142/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220225233528/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220226185336/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220227175728/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220228231935/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220301185329/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220302205559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220303195154/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220304235636/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220305211400/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220306205522/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220307164915/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220308204303/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220309213817/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220310201012/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220311205005/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220312181142/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220313095112/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220314190653/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220315165310/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220316152314/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220317193934/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220318215226/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220319212345/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220320235959/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220321190729/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220322151602/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220323143821/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220324190934/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220325192145/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220326183518/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://web.archive.org/web/20220327235658/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html")
    


date_frame <- data.frame(Dates=dates, Links=links)

list_of_lists <- pbapply::pblapply(seq(1, nrow(date_frame), 1), function(x) totals_by_type(link=date_frame[x, "Links"], date=date_frame[x,"Dates"]), cl=18)

current_frame <- data.table::rbindlist(list_of_lists, use.names=T, fill=T)
results <- totals_fold(totals_df=current_frame)


googleSheetPush <- function(results, sheet_url="https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0"){

  

  gsheet_totals <- googlesheets4::sheet_write(ss=sheet_url, data=results$Totals, sheet="Totals")
    gsheet_destroyed <- googlesheets4::sheet_write(ss=sheet_url, data=results$Destroyed, sheet="Destroyed")
    gsheet_damaged <- googlesheets4::sheet_write(ss=sheet_url, data=results$Damaged, sheet="Damaged")
    gsheet_abandoned <- googlesheets4::sheet_write(ss=sheet_url, data=results$Abandoned, sheet="Abandoned")
    gsheet_captured <- googlesheets4::sheet_write(ss=sheet_url, data=results$Captured, sheet="Captured")

}







