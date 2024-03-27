source("Automation/automation.r")

russia_redo_daily_url_list <- list(
    x2024_01_03 = "https://web.archive.org/web/20240104001559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_06="https://web.archive.org/web/20240107001711/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_09="https://web.archive.org/web/20240109225201/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_10="https://web.archive.org/web/20240111002342/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_13="https://web.archive.org/web/20240114001702/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_16="https://web.archive.org/web/20240117002341/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_20="https://web.archive.org/web/20240121001558/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_22="https://web.archive.org/web/20240123001704/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_23="https://web.archive.org/web/20240124001706/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_26="https://web.archive.org/web/20240127001652/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_01_29="https://web.archive.org/web/20240130001600/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_01="https://web.archive.org/web/20240202001717/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_02="https://web.archive.org/web/20240203000649/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_03="https://web.archive.org/web/20240204004939/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_05="https://web.archive.org/web/20240206001559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_09="https://web.archive.org/web/20240210001559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_11="https://web.archive.org/web/20240212001623/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_14="https://web.archive.org/web/20240215001712/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_17="https://web.archive.org/web/20240217230531/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_22="https://web.archive.org/web/20240222232157/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_26="https://web.archive.org/web/20240227001735/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_02_28="https://web.archive.org/web/20240229002519/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_03_02="https://web.archive.org/web/20240303001633/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_03_04="https://web.archive.org/web/20240305001739/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_03_07="https://web.archive.org/web/20240308001707/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_03_09="https://web.archive.org/web/20240310001559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_03_13="https://web.archive.org/web/20240314001559/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
    x2024_03_20="https://web.archive.org/web/20240321001724/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html"
)
names(russia_redo_daily_url_list) <- gsub("x", "", names(russia_redo_daily_url_list))

ukraine_redo_daily_url_list <-  list(
    x2024_01_03 = "https://web.archive.org/web/20240104001603/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_06="https://web.archive.org/web/20240107001715/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_09="https://web.archive.org/web/20240110001719/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_10="https://web.archive.org/web/20240111002340/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_13="https://web.archive.org/web/20240114001705/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_16="https://web.archive.org/web/20240117002338/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_20="https://web.archive.org/web/20240121001602/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_22="https://web.archive.org/web/20240123001708/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_23="https://web.archive.org/web/20240124001709/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_26="https://web.archive.org/web/20240127001657/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_01_29="https://web.archive.org/web/20240130001604/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_01="https://web.archive.org/web/20240202001721/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_02="https://web.archive.org/web/20240203001625/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_03="https://web.archive.org/web/20240204001604/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_05="https://web.archive.org/web/20240206001602/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_09="https://web.archive.org/web/20240210001601/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_11="https://web.archive.org/web/20240212001628/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_14="https://web.archive.org/web/20240215001717/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_17="https://web.archive.org/web/20240218001603/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_22="https://web.archive.org/web/20240223000755/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_26="https://web.archive.org/web/20240227001737/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_02_28="https://web.archive.org/web/20240228001634/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_03_02="https://web.archive.org/web/20240303035241/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_03_04="https://web.archive.org/web/20240305001743/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_03_07="https://web.archive.org/web/20240308121602/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_03_09="https://web.archive.org/web/20240311001658/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_03_13="https://web.archive.org/web/20240314001605/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html",
    x2024_03_20="https://web.archive.org/web/20240321001729/https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html"
)
names(ukraine_redo_daily_url_list) <- gsub("x", "", names(ukraine_redo_daily_url_list))

empty_daily <- read.csv("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/2023-12-29.csv")[,-1]

date_seq <- seq(as.Date("2024-01-03"), Sys.Date(), by="days")
for(i in seq_along(date_seq)){
    print(format(as.Date(date_seq[i]), "%Y-%m-%d"))
    if(as.character(gsub("-", "_", date_seq[i])) %in% names(russia_redo_daily_url_list)){
        systems <- scrape_data(russia_link=russia_redo_daily_url_list[[as.character(gsub("-", "_", date_seq[i]))]], ukraine_link=ukraine_redo_daily_url_list[[as.character(gsub("-", "_", date_seq[i]))]], date=format(as.Date(date_seq[i]), "%Y-%m-%d"))
        gsub_russia <- gsub("https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html", "",  russia_redo_daily_url_list[[as.character(gsub("-", "_", date_seq[i]))]])
        gsub_ukraine <- gsub("https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html", "",  ukraine_redo_daily_url_list[[as.character(gsub("-", "_", date_seq[i]))]])
        systems$url <- gsub(gsub_russia, "", systems$url)
        systems$url <- gsub(gsub_ukraine, "", systems$url)
        write.csv(systems, paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/", format(as.Date(date_seq[i]), "%Y-%m-%d"), ".csv"))
        previous_day <- read.csv(paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/", format(as.Date(date_seq[i]-1), "%Y-%m-%d"), ".csv"))
        daily_systems <- systems[!systems$url %in% previous_day$url, ]
        write.csv(daily_systems, paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/", format(as.Date(date_seq[i]), "%Y-%m-%d"), ".csv"))
        tidy_systems <- total_by_system_wide(systems)
        write.csv(tidy_systems, paste0("~/GitHub/Russia-Ukraine/data/bySystem/Totals/Full/", format(as.Date(date_seq[i]), "%Y-%m-%d"), ".csv"))
    } else {
        write.csv(systems, paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Full/", format(as.Date(date_seq[i]), "%Y-%m-%d"), ".csv"))
        write.csv(empty_daily, paste0("~/GitHub/Russia-Ukraine/data/bySystem/Raw/Daily/", format(as.Date(date_seq[i]), "%Y-%m-%d"), ".csv"))
        write.csv(tidy_systems, paste0("~/GitHub/Russia-Ukraine/data/bySystem/Totals/Full/", format(as.Date(date_seq[i]), "%Y-%m-%d"), ".csv"))
    }
}
