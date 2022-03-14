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


