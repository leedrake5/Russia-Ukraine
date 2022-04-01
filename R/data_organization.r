####byType

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
"https://web.archive.org/web/20220329122252/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
"https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html")
    


date_frame <- data.frame(Dates=dates, Links=links)

list_of_lists <- pbapply::pblapply(seq(1, nrow(date_frame), 1), function(x) totals_by_type(link=date_frame[x, "Links"], date=date_frame[x,"Dates"]))
list_of_lists[[33]] <- data.frame(read.csv("/Users/lee/GitHub/oryx_data/totals_by_type_0328.csv"))
list_of_lists[[33]]$Date <- as.Date("2022-03-28")
names(list_of_lists) <- dates

for(i in names(list_of_lists)){
    write.csv(list_of_lists[[i]], paste0("~/GitHub/Russia-Ukraine/data/", i, ".csv"))
}
current_frame <- data.table::rbindlist(list_of_lists, use.names=TRUE, fill=TRUE)
results <- totals_fold(totals_df=current_frame)

result_list <- list()
for(i in dates[1:length(dates)-1]){
    result_list[[as.character(as.Date(i, format="%Y-%m-%d", origin="1970-01-01"))]] <- read.csv(paste0("~/GitHub/Russia-Ukraine/data/byType/", as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))[,-1]
}
result_list[[as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01"))]] <- totals_by_type(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", date=as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01")))
write.csv(result_list[[as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01"))]], paste0("~/GitHub/Russia-Ukraine/data/byType/", as.character(as.Date(Sys.Date(), format="%Y-%m-%d", origin="1970-01-01")), ".csv"))

current_frame <- data.table::rbindlist(result_list, use.names=TRUE, fill=TRUE)
results <- totals_fold(totals_df=current_frame)

googleSheetPush <- function(results, sheet_url="https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0"){

    #googlesheets4::gs4_auth()

  gsheet_totals <- googlesheets4::sheet_write(ss=sheet_url, data=results$Totals, sheet="Totals")
    gsheet_destroyed <- googlesheets4::sheet_write(ss=sheet_url, data=results$Destroyed, sheet="Destroyed")
    gsheet_damaged <- googlesheets4::sheet_write(ss=sheet_url, data=results$Damaged, sheet="Damaged")
    gsheet_abandoned <- googlesheets4::sheet_write(ss=sheet_url, data=results$Abandoned, sheet="Abandoned")
    gsheet_captured <- googlesheets4::sheet_write(ss=sheet_url, data=results$Captured, sheet="Captured")

}

googleSheetPush(results)

daily_update <- function(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", to_return=NULL){
    
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

###By Equipment
source("~/GitHub/scrape_oryx/R/functions.R")

#' scrape_data
#' @description Gets data by system.
#'
#' @return a tibble
#' @export
scrape_data <- function(link="https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", date=NULL, remove=NULL) {
    
    if(is.null(date)){
        date <- format(Sys.Date(), "%m/%d/%Y")
     }
    materiel <-
    get_data(
      link,
      "article"
    ) %>%
    rvest::html_elements("li")

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
    tidy_frame <- indsn %>% dplyr::select(country, system, status, Date) %>%
    dplyr::group_by(country, system, status) %>%
    dplyr::summarise(count = dplyr::n(), na.rm=TRUE) %>%
    tidyr::pivot_wider(names_from = status, values_from = count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
                  total = destroyed + captured + damaged + abandoned)
    tidy_frame$Date <- date
    return(tidy_frame)
}

test <- scrape_data()
test_2 <- total_by_system_wide(test)
classes <- read.csv("/Users/lee/GitHub/Russia-Ukraine/data/classes.csv")

classes_2 <- merge(classes, data.frame(system=unique(test_2$system)), by="system", all=TRUE)
classes_2[is.na(classes_2)] <- ""
write.csv(classes_2, "~/Desktop/classes2.csv")

dates = seq(as.Date("2022-02-24"), Sys.Date(), by="days")
dates <- dates[1:32]
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
"https://web.archive.org/web/20220327235658/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html")
    
web_prefix_remove <- c("https://web.archive.org/web/20220224231142/",
"https://web.archive.org/web/20220225233528/",
"https://web.archive.org/web/20220226185336/",
"https://web.archive.org/web/20220227175728/",
"https://web.archive.org/web/20220228231935/",
"https://web.archive.org/web/20220301185329/",
"https://web.archive.org/web/20220302205559/",
"https://web.archive.org/web/20220303195154/",
"https://web.archive.org/web/20220304235636/",
"https://web.archive.org/web/20220305211400/",
"https://web.archive.org/web/20220306205522/",
"https://web.archive.org/web/20220307164915/",
"https://web.archive.org/web/20220308204303/",
"https://web.archive.org/web/20220309213817/",
"https://web.archive.org/web/20220310201012/",
"https://web.archive.org/web/20220311205005/",
"https://web.archive.org/web/20220312181142/",
"https://web.archive.org/web/20220313095112/",
"https://web.archive.org/web/20220314190653/",
"https://web.archive.org/web/20220315165310/",
"https://web.archive.org/web/20220316152314/",
"https://web.archive.org/web/20220317193934/",
"https://web.archive.org/web/20220318215226/",
"https://web.archive.org/web/20220319212345/",
"https://web.archive.org/web/20220320235959/",
"https://web.archive.org/web/20220321190729/",
"https://web.archive.org/web/20220322151602/",
"https://web.archive.org/web/20220323143821/",
"https://web.archive.org/web/20220324190934/",
"https://web.archive.org/web/20220325192145/",
"https://web.archive.org/web/20220326183518/",
"https://web.archive.org/web/20220327235658/")

date_frame <- data.frame(Dates=dates, Links=links, Remove=web_prefix_remove)

raw_list <- pbapply::pblapply(seq(1, nrow(date_frame), 1), function(x) scrape_data(link=date_frame[x, "Links"], date=date_frame[x,"Dates"], remove=date_frame[x,"Remove"]), cl=18)
names(raw_list) <- dates
raw_list[["2022-03-28"]] <- read.csv("/Users/lee/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/2022-03-28.csv")
raw_list[["2022-03-29"]] <- read.csv("/Users/lee/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/2022-03-29.csv")
raw_list[["2022-03-30"]] <- scrape_data()
names(raw_list) <- dates
for(i in names(raw_list)){
    write.csv(raw_list[[i]], paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/", i, ".csv"))
}

tidy_list <- pbapply::pblapply(raw_list, total_by_system_wide)
for(i in names(tidy_list)){
    write.csv(tidy_list[[i]], paste0("~/GitHub/Russia-Ukraine/data/bySystem/Totals/Full/", i, ".csv"))
}

raw_mod_list <- list()
for(i in names(raw_list)){
    raw_mod_list[[i]] <- raw_list[[i]][, !colnames(raw_list[[i]]) %in% c("Date", "date_recorded")]
}

for(i in 2:length(raw_mod_list)){
    raw_mod_list[[names(raw_mod_list)[i]]] <- raw_mod_list[[i]][!raw_mod_list[[i]]$url %in% raw_list[[i-1]]$url,]
}
for(i in names(raw_mod_list)){
    raw_mod_list[[i]]$Date <- i
    write.csv(raw_mod_list[[i]], paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/", i, ".csv"))
}

daily_tidy_list <- pbapply::pblapply(raw_mod_list, total_by_system_wide)
for(i in names(tidy_list)){
    write.csv(tidy_list[[i]], paste0("~/GitHub/Russia-Ukraine/data/bySystem/Totals/Full/", i, ".csv"))
}

daily_list <- list()

for(i in dates){
    
    daily_list[[i]] <- read.csv(paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/", i, ".csv"))
    
}
