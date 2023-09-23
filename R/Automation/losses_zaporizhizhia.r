country_colors <-   c("Russia" = "#E4181C", "Ukraine" = "#0057B8")


russia <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Oxj79cNh5GR27RBwHirHiQ9VMr5A_g7cGZ1B57zu0jk", sheet="Russian Losses")
russia <- russia[,c("Type", "Model", "Status", "Lost by", "Unit", "Date", "Nearest location", "Oryx URL", "Source", "Geolocation", "Tags")]
russia <- russia[complete.cases(russia$Model),]
russia$Country <- "Russia"

ukraine <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Oxj79cNh5GR27RBwHirHiQ9VMr5A_g7cGZ1B57zu0jk", sheet="Ukrainian Losses")
ukraine <- ukraine[,c("Type", "Model", "Status", "Lost by", "Unit", "Date", "Nearest location", "Oryx URL",  "Source", "Geolocation", "Tags")]
ukraine <- ukraine[complete.cases(ukraine$Model),]
ukraine$Country <- "Ukraine"

total <- as.data.frame(data.table::rbindlist(list(russia, ukraine)))
total_mod <- total
total_mod$GeneralType <- total_mod$Type
total_mod$GeneralType[total_mod$GeneralType=="Towed artillery"] <- "Artillery"
total_mod$GeneralType[total_mod$GeneralType=="MLRS"] <- "Artillery"
total_mod$GeneralType[total_mod$GeneralType=="Self-propelled artillery"] <- "Artillery"
total_mod$GeneralType[total_mod$GeneralType=="Tanks"] <- "Armor"
total_mod$GeneralType[total_mod$GeneralType=="Armoured Fighting Vehicles"] <- "Armor"
total_mod$lat <- as.numeric(sapply(total_mod$Geolocation, function(x) strsplit(x, split=",")[[1]][1]))
total_mod$lng <- as.numeric(sapply(total_mod$Geolocation, function(x) strsplit(x, split=",")[[1]][2]))


write.csv(total_mod, "~/GitHub/Russia-Ukraine/data/Naalsio/zaporizhizhia.csv")
write.csv(total_mod, "~/GitHub/Russia-Ukraine/Apps/zapMap/data/zaporizhizhia.csv")
total_mod$Date <- as.Date(total_mod$Date)
total_mod <- total_mod[order(total_mod$Date),]

ukraine_mod <- total_mod[total_mod$Country=="Ukraine", ]
russia_mod <- total_mod[total_mod$Country=="Russia", ]



###Totals
ukraine_total_temp <- data.frame(Date=ukraine_mod[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_total_temp <- data.frame(Date=russia_mod[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Equipment Losses") +
  ggtitle(paste0("Total equipment losses through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_total.jpg", current_total, device="jpg", width=6, height=5)


####Totals Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Total equipment loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

###Total Map

orkhiv <- ggmap::get_map(location=c(lon=35.860405, lat=47.470226), source="google", maptype="roadmap", crop=FALSE, zoom=10)

orkhiv_map <- ggmap(orkhiv) +
#geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
#geom_point(data=firms, mapping=aes(x=lon, y=lat, colour=NASA), alpha=0.5) +
geom_point(data=total_mod, mapping=aes(x=lng, y=lat, colour=Country, shape=Country)) +
ggtitle(paste0("Tokmak Axis on ", Sys.Date())) + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/orkhiv.jpg", orkhiv_map, device="jpg", width=6, height=5, dpi=600)


vn <- ggmap::get_map(location=c(lon=36.818071, lat=47.751817), source="google", maptype="roadmap", crop=FALSE, zoom=10)

vn_map <- ggmap(vn) +
#geom_point(data=btgs, mapping=aes(x=lon, y=lat, shape=Russian_BTGS), alpha=0.9, colour="purple") +
geom_point(data=total_mod, mapping=aes(x=lng, y=lat, colour=Country, shape=Country)) +
ggtitle(paste0("Velyka Novosilka Axis on ", Sys.Date())) + theme_light()

ggsave("~/Github/Russia-Ukraine/Maps/vn.jpg", vn_map, device="jpg", width=6, height=5, dpi=600)

###Destroyed
ukraine_destroyed <- ukraine_mod[ukraine_mod$Status=="Destroyed",]
ukraine_total_temp <- data.frame(Date=ukraine_destroyed[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_destroyed <- russia_mod[russia_mod$Status=="Destroyed",]
russia_total_temp <- data.frame(Date=russia_mod[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Equipment Destroyed") +
  ggtitle(paste0("Total equipment destroyed through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_destroyed.jpg", current_total, device="jpg", width=6, height=5)


####Destroyed Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Destroyed Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Destroyed equipment loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/destroyed_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

###Abandoned
ukraine_abandoned <- ukraine_mod[ukraine_mod$Status=="Abandoned",]
ukraine_total_temp <- data.frame(Date=ukraine_abandoned[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_abandoned <- russia_mod[russia_mod$Status=="Abandoned",]
russia_total_temp <- data.frame(Date=russia_abandoned[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Equipment Abandoned") +
  ggtitle(paste0("Total equipment abandoned through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_abandoned.jpg", current_total, device="jpg", width=6, height=5)


####Abandoned Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Abandoned Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Abandoned equipment loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/abandoned_ratio.jpg", current_ratio, device="jpg", width=6, height=5)


###Captured
ukraine_captured <- ukraine_mod[ukraine_mod$Status=="Captured",]
ukraine_total_temp <- data.frame(Date=ukraine_captured[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_captured <- russia_mod[russia_mod$Status=="Captured",]
russia_total_temp <- data.frame(Date=russia_captured[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Equipment Captured") +
  ggtitle(paste0("Total equipment captured through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_captured.jpg", current_total, device="jpg", width=6, height=5)


####Captured Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Captured Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Captured equipment loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/captured_ratio.jpg", current_ratio, device="jpg", width=6, height=5)


###Damaged
ukraine_damaged <- ukraine_mod[ukraine_mod$Status=="Damaged",]
ukraine_total_temp <- data.frame(Date=ukraine_captured[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_damaged <- russia_mod[russia_mod$Status=="Damaged",]
russia_total_temp <- data.frame(Date=russia_captured[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Total Equipment Damaged") +
  ggtitle(paste0("Total equipment damaged through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_damaged.jpg", current_total, device="jpg", width=6, height=5)


####Damaged Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Damaged Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Damaged equipment loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/damaged_ratio.jpg", current_ratio, device="jpg", width=6, height=5)


###Tanks
ukraine_temp_hold <- ukraine_mod[ukraine_mod$Type=="Tanks",]
ukraine_total_temp <- data.frame(Date=ukraine_temp_hold[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_temp_hold <- russia_mod[russia_mod$Type=="Tanks",]
russia_total_temp <- data.frame(Date=russia_temp_hold[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Tanks Lost") +
  ggtitle(paste0("Tanks lost through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_tanks.jpg", current_total, device="jpg", width=6, height=5)


####Tank Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Tank Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Tank loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/tank_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

####Armored Fighting Vehicles (AFVs)
ukraine_temp_hold <- ukraine_mod[ukraine_mod$Type=="Armoured Fighting Vehicles",]
ukraine_total_temp <- data.frame(Date=ukraine_temp_hold[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_temp_hold <- russia_mod[russia_mod$Type=="Armoured Fighting Vehicles",]
russia_total_temp <- data.frame(Date=russia_temp_hold[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Armored Fighting Vehicles Lost") +
  ggtitle(paste0("AFV lost through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_afv.jpg", current_total, device="jpg", width=6, height=5)


####AFV Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("AFV Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("AFV loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/afv_ratio.jpg", current_ratio, device="jpg", width=6, height=5)


####Artillery
ukraine_temp_hold <- ukraine_mod[ukraine_mod$GeneralType=="Artillery",]
ukraine_total_temp <- data.frame(Date=ukraine_temp_hold[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_temp_hold <- russia_mod[russia_mod$GeneralType=="Artillery",]
russia_total_temp <- data.frame(Date=russia_temp_hold[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Artillery Lost") +
  ggtitle(paste0("Artillery lost through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_artillery.jpg", current_total, device="jpg", width=6, height=5)


####Artillery Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Artillery Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Artillery loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/artillery_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

####Trucks
ukraine_temp_hold <- ukraine_mod[ukraine_mod$Type=="Trucks",]
ukraine_total_temp <- data.frame(Date=ukraine_temp_hold[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_temp_hold <- russia_mod[russia_mod$Type=="Trucks",]
russia_total_temp <- data.frame(Date=russia_temp_hold[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Vehicles Lost") +
  ggtitle(paste0("Vehicles lost through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_vehicles.jpg", current_total, device="jpg", width=6, height=5)


####Vehicle Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Vehicle Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Vehicle loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/vehicle_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

####Armor
ukraine_temp_hold <- ukraine_mod[ukraine_mod$GeneralType=="Armor",]
ukraine_total_temp <- data.frame(Date=ukraine_temp_hold[,c("Date")])
ukraine_total_melt <- ukraine_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
ukraine_total_melt$Country <- "Ukraine"

russia_temp_hold <- russia_mod[russia_mod$GeneralType=="Armor",]
russia_total_temp <- data.frame(Date=russia_temp_hold[,c("Date")])
russia_total_melt <- russia_total_temp %>%
    group_by(Date) %>%
    summarise(Daily=n())

russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
russia_total_melt$Country <- "Russia"

total_melt <- as.data.frame(data.table::rbindlist(list(russia_total_melt, ukraine_total_melt)))

current_total <-
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Armor Lost") +
  ggtitle(paste0("Armor lost through ", Sys.Date())) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_armor.jpg", current_total, device="jpg", width=6, height=5)


####Armor Ratio
ukraine_small <- ukraine_total_melt[,c("Date", "Total")]
colnames(ukraine_small) <- c("Date", "Ukraine_Total")
russia_small <- russia_total_melt[,c("Date", "Total")]
colnames(russia_small) <- c("Date", "Russia_Total")

total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%Y/%m/%d") +
  scale_y_continuous("Armor Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Armor loss ratio through ", Sys.Date())) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/armor_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

###Analysis
ukraine_final <- as.data.frame(table(ukraine_mod$Type))
colnames(ukraine_final) <- c("Type", "Count")
ukraine_final$Type <- as.character(ukraine_final$Type)
russia_final <- as.data.frame(table(russia_mod$Type))
colnames(russia_final) <- c("Type", "Count")
russia_final$Type <- as.character(russia_final$Type)

unshared_types <- c(setdiff(russia_mod$Type, ukraine_mod$Type), setdiff(ukraine_mod$Type, russia_mod$Type))
shared_types <- unique(c(russia_mod$Type, ukraine_mod$Type))[!unique(c(russia_mod$Type, ukraine_mod$Type)) %in% unshared_types]


ratio_list <- list()
for(i in shared_types){
    ratio_list[[i]] <- russia_final[russia_final$Type==i,2]/ukraine_final[ukraine_final$Type==i,2]
}

ratio_frame <- data.frame(Type=shared_types, Ratio=unlist(ratio_list))


unit_type <- ggplot(ratio_frame, aes(Type, Ratio, colour=Type, fill=Type)) +
  geom_col() +
  scale_y_continuous("Ratio (Russian/Ukrainian Losses)", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    scale_x_discrete("") +
  ggtitle(paste0("Unit type ratios lost through ", Sys.Date())) +
    coord_flip() +
    theme_light()
  #scale_fill_brewer(palette="Accent") +
  #scale_color_brewer(palette="Accent")
ggsave("~/Github/Russia-Ukraine/Plots/zaporizhizhia/current_unit_type.jpg", unit_type, device="jpg", width=6, height=5)

