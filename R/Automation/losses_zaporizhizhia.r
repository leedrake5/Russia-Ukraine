russia <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Oxj79cNh5GR27RBwHirHiQ9VMr5A_g7cGZ1B57zu0jk/edit#gid=25066677", sheetid="Russian Losses")
ukraine <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Oxj79cNh5GR27RBwHirHiQ9VMr5A_g7cGZ1B57zu0jk/edit#gid=25066677", sheetid="Ukrainian Losses")

total <- as.data.frame(data.table::rbindlist(list(russia, ukraine)))
total_mod <- total
total_mod$Type[total_mod$Type=="Towed artillery"] <- "Artillery"
total_mod$Type[total_mod$Type=="MLRS"] <- "Artillery"
total_mod$Type[total_mod$Type=="Self-propelled artillery"] <- "Artillery"
total_mod$Type[total_mod$Type=="Tanks"] <- "Armor"
total_mod$Type[total_mod$Type=="Armoured Fighting Vehicles"] <- "Armor"




write.csv(total, "data/Naalsio/zaporizhizhia.csv")

