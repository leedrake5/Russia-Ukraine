tryCatch(detach("package:do", unload=TRUE), error=function(e) NULL)

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

list.of.packages <- c("ggplot2", "RCurl", "reshape2", "data.table", "gsheet", "tidyverse", "lubridate", "scales", "rvest", "sf", "mapview", "raster", "ggmap", "dplyr")
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
library(tidyverse)
library(lubridate)
library(scales)
library(rvest)
library(sf)
library(mapview)
library(raster)
library(ggmap)
library(dplyr)
library(zoo)

country_colors <-   c("Russia" = "#E4181C", "Ukraine" = "#0057B8")



ggplot2::theme_set(ggplot2::theme_minimal())
# options(ggplot2.continuous.fill  = function() scale_fill_viridis_c())
# options(ggplot2.continuous.colour = function() scale_color_viridis_c())
# options(ggplot2.discrete.colour = function() scale_color_brewer(palette = "Dark2"))
# options(ggplot2.discrete.fill = function() scale_fill_brewer(palette = "Dark2"))



equipment_losses <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0", sheetid="Origional")
equipment_losses_before <- equipment_losses[equipment_losses$Date == "2022-08-31",]
equipment_losses$Date <- as.Date(equipment_losses$Date, format="%Y-%m-%d")
equipment_losses <- equipment_losses %>%
    filter(Date >= as.Date("2022-09-01")) %>%
    filter(Date <= as.Date("2022-12-31"))
for (name in names(equipment_losses[,-1])) {
   # Subtract the value associated with that name from each row of the column
   equipment_losses[[name]] <- equipment_losses[[name]] - as.numeric(equipment_losses_before[1,name])
}

equipment_totals <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0", sheetid="Totals")
equipment_totals_before <- equipment_totals[equipment_totals$Date == "2022-08-31",-1]
equipment_totals$Date <- as.Date(equipment_totals$Date, format="%Y-%m-%d")
equipment_totals <- equipment_totals %>%
    filter(Date >= as.Date("2022-09-01")) %>%
    filter(Date <= as.Date("2022-12-31"))
for (name in names(equipment_totals[,-1])) {
   # Subtract the value associated with that name from each row of the column
   equipment_totals[,name] <- equipment_totals[,name] - as.numeric(equipment_totals_before[1,name])
}


equipment_destroyed <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0", sheetid="Destroyed")
equipment_destroyed_before <- equipment_destroyed[equipment_destroyed$Date == "2022-08-31",-1]
equipment_destroyed$Date <- as.Date(equipment_destroyed$Date, format="%Y-%m-%d")
equipment_destroyed <- equipment_destroyed %>%
    filter(Date >= as.Date("2022-09-01")) %>%
    filter(Date <= as.Date("2022-12-31"))
for (name in names(equipment_destroyed[,-1])) {
   # Subtract the value associated with that name from each row of the column
   equipment_destroyed[,name] <- equipment_destroyed[,name] - as.numeric(equipment_destroyed_before[1,name])
}


equipment_damaged <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0", sheetid="Damaged")
equipment_damaged_before <- equipment_damaged[equipment_damaged$Date == "2022-08-31",-1]
equipment_damaged$Date <- as.Date(equipment_damaged$Date, format="%Y-%m-%d")
equipment_damaged <- equipment_damaged %>%
    filter(Date >= as.Date("2022-09-01")) %>%
    filter(Date <= as.Date("2022-12-31"))
for (name in names(equipment_damaged[,-1])) {
   # Subtract the value associated with that name from each row of the column
   equipment_damaged[,name] <- equipment_damaged[,name] - as.numeric(equipment_damaged_before[1,name])
}


equipment_captures <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0", sheetid="Captures")
equipment_captures_before <- equipment_captures[equipment_captures$Date == "2022-08-31",-1]
equipment_captures$Date <- as.Date(equipment_captures$Date, format="%Y-%m-%d")
equipment_captures <- equipment_captures %>%
    filter(Date >= as.Date("2022-09-01")) %>%
    filter(Date <= as.Date("2022-12-31"))
for (name in names(equipment_captures[,-1])) {
   # Subtract the value associated with that name from each row of the column
   equipment_captures[,name] <- equipment_captures[,name] - as.numeric(equipment_captures_before[1,name])
}



equipment_synthetic <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0", sheetid="Synthetic")
equipment_synthetic_before <- equipment_synthetic[equipment_synthetic$Date == "2022-08-31",-1]
equipment_synthetic$Date <- as.Date(equipment_synthetic$Date, format="%Y-%m-%d")
equipment_synthetic <- equipment_synthetic %>%
    filter(Date >= as.Date("2022-09-01")) %>%
    filter(Date <= as.Date("2022-12-31"))
for (name in names(equipment_synthetic[,-1])) {
   # Subtract the value associated with that name from each row of the column
   equipment_synthetic[,name] <- equipment_synthetic[,name] - as.numeric(equipment_synthetic_before[1,name])
}




####Totals
total_melt <- melt(equipment_losses[,c("Date", "Russia_Total", "Ukraine_Total")], id.var="Date")
total_melt$Date <- as.Date(total_melt$Date, format="%m/%d/%Y")
colnames(total_melt) <- c("Date", "Country", "Total")
total_melt$Country <- gsub("_Total", "", total_melt$Country)
total_melt <- total_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Total - lag(Total, default = first(Total)))

current_total <- 
  ggplot(total_melt, aes(Date, Total, colour=Country)) +
  geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Losses") +
  ggtitle(paste0("Total equipment losses through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_total.jpg", current_total, device="jpg", width=6, height=5)

####Totals Ratio
total_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Total/equipment_losses$Ukraine_Total)
total_ratio_frame$Date <- as.Date(total_ratio_frame$Date, format="%m/%d/%Y")
total_ratio_frame <- total_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

current_ratio <-
  ggplot(total_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Total equipment loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_ratio.jpg", current_ratio, device="jpg", width=6, height=5)

####Destroyed
destroyed_melt <- melt(equipment_losses[,c("Date", "Russia_Destroyed", "Ukraine_Destroyed")], id.var="Date")
destroyed_melt$Date <- as.Date(destroyed_melt$Date, format="%m/%d/%Y")
colnames(destroyed_melt) <- c("Date", "Country", "Destroyed")
destroyed_melt$Country <- gsub("_Destroyed", "", destroyed_melt$Country)
destroyed_melt <- destroyed_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Destroyed - lag(Destroyed, default = first(Destroyed)))


current_destroyed <- ggplot(destroyed_melt, aes(Date, Destroyed, colour=Country)) +
  geom_col(data=destroyed_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Destroyed") +
  ggtitle(paste0("Total equipment destroyed through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_destroyed.jpg", current_destroyed, device="jpg", width=6, height=5)

destroyed_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Destroyed/equipment_losses$Ukraine_Destroyed)
destroyed_ratio_frame$Date <- as.Date(destroyed_ratio_frame$Date, format="%m/%d/%Y")
destroyed_ratio_frame <- destroyed_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

destroyed_ratio <-
  ggplot(destroyed_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=destroyed_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Destroyed Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Destroyed equipment loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/destroyed_ratio.jpg", destroyed_ratio, device="jpg", width=6, height=5)

####Abandoned
abandoned_melt <- melt(equipment_losses[,c("Date", "Russia_Abandoned", "Ukraine_Abandoned")], id.var="Date")
abandoned_melt$Date <- as.Date(abandoned_melt$Date, format="%m/%d/%Y")
colnames(abandoned_melt) <- c("Date", "Country", "Abandoned")
abandoned_melt$Country <- gsub("_Abandoned", "", abandoned_melt$Country)
abandoned_melt <- abandoned_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Abandoned - lag(Abandoned, default = first(Abandoned)))

current_abandoned <- ggplot(abandoned_melt, aes(Date, Abandoned, colour=Country)) +
  geom_col(data=abandoned_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Abandoned") +
  ggtitle(paste0("Total equipment abandoned through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_abandoned.jpg", current_abandoned, device="jpg", width=6, height=5)

abandoned_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Abandoned/equipment_losses$Ukraine_Abandoned)
abandoned_ratio_frame$Date <- as.Date(abandoned_ratio_frame$Date, format="%m/%d/%Y")
abandoned_ratio_frame <- abandoned_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

abandoned_ratio <-
  ggplot(abandoned_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=destroyed_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Abandoned Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Abandoned equipment loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/abandoned_ratio.jpg", abandoned_ratio, device="jpg", width=6, height=5)


####Captured
captured_melt <- melt(equipment_losses[,c("Date", "Russia_Captured", "Ukraine_Captured")], id.var="Date")
captured_melt$Date <- as.Date(captured_melt$Date, format="%m/%d/%Y")
colnames(captured_melt) <- c("Date", "Country", "Captured")
captured_melt$Country <- gsub("_Captured", "", captured_melt$Country)
captured_melt <- captured_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Captured - lag(Captured, default = first(Captured)))

current_captured <- ggplot(captured_melt, aes(Date, Captured, colour=Country)) +
  geom_col(data=captured_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Captured by Enemy") +
  ggtitle(paste0("Total equipment captured by enemy through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_captured.jpg", current_captured, device="jpg", width=6, height=5)

captured_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Captured/equipment_losses$Ukraine_Captured)
captured_ratio_frame$Date <- as.Date(captured_ratio_frame$Date, format="%m/%d/%Y")
captured_ratio_frame <- captured_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

captured_ratio <-
  ggplot(captured_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=destroyed_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Captured Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Captured equipment loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/captured_ratio.jpg", captured_ratio, device="jpg", width=6, height=5)

####Damaged
damaged_melt <- melt(equipment_losses[,c("Date", "Russia_Damaged", "Ukraine_Damaged")], id.var="Date")
damaged_melt$Date <- as.Date(damaged_melt$Date, format="%m/%d/%Y")
colnames(damaged_melt) <- c("Date", "Country", "Damaged")
damaged_melt$Country <- gsub("_Damaged", "", damaged_melt$Country)
damaged_melt <- damaged_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Damaged - lag(Damaged, default = first(Damaged)))

current_damaged <- ggplot(damaged_melt, aes(Date, Damaged, colour=Country)) +
  geom_col(data=damaged_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Damaged") +
  ggtitle(paste0("Total equipment damaged through ", as.Date("2022-12-31"))) +
  theme_light() +
  scale_colour_manual(values = country_colors)  +
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_damaged.jpg", current_damaged, device="jpg", width=6, height=5)

damaged_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Damaged/equipment_losses$Ukraine_Damaged)
damaged_ratio_frame$Date <- as.Date(damaged_ratio_frame$Date, format="%m/%d/%Y")
damaged_ratio_frame <- damaged_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

damaged_ratio <-
  ggplot(damaged_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=destroyed_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Damaged Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Damaged equipment loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/damaged_ratio.jpg", damaged_ratio, device="jpg", width=6, height=5)



###All together now
total_melt$Type <- "Total"
colnames(total_melt)[3] <- "Number"
destroyed_melt$Type <- "Destroyed"
colnames(destroyed_melt)[3] <- "Number"
abandoned_melt$Type <- "Abandoned"
colnames(abandoned_melt)[3] <- "Number"
captured_melt$Type <- "Captured"
colnames(captured_melt)[3] <- "Number"

all_melt <- rbindlist(list(destroyed_melt, abandoned_melt, captured_melt))

current_grid <- ggplot(all_melt, aes(Date, Number, colour=Country)) +
  geom_col(data=all_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Lost") +
  ggtitle(paste0("Total equipment lost through ", as.Date("2022-12-31"))) +
  facet_grid(rows=vars(Type)) +
  theme_light() +
  theme(legend.position="bottom")  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_grid.jpg", current_grid, device="jpg", width=6, height=10)


##Ratio
####Totals
total_melt <- melt(equipment_losses[,c("Date", "Russia_Total", "Ukraine_Total")], id.var="Date")
total_melt$Date <- as.Date(total_melt$Date, format="%m/%d/%Y")
colnames(total_melt) <- c("Date", "Country", "Total")
total_melt$Country <- gsub("_Total", "", total_melt$Country)
total_melt <- total_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Total - lag(Total, default = first(Total)))
total_cast = reshape2::dcast(total_melt[,c("Date", "Country", "Daily")], formula=Date~Country, fun.aggregate=mean, value.var="Daily")
total_cast$RussiaRatio <- total_cast$Russia/(total_cast$Russia+total_cast$Ukraine)
total_cast$UkraineRatio <- total_cast$Ukraine/(total_cast$Russia+total_cast$Ukraine)
total_cast$Ratio <- total_cast$Russia/total_cast$Ukraine
total_cast <- total_cast[-1,]

total_cast_ratio <- total_cast[,c("Date", "Ratio")]
total_cast_melt <- reshape2::melt(total_cast[,c("Date", "RussiaRatio", "UkraineRatio")], id.var="Date")
total_cast_melt$variable <- as.character(total_cast_melt$variable)
total_cast_melt$variable[total_cast_melt$variable=="RussiaRatio"] <- "Russia"
total_cast_melt$variable[total_cast_melt$variable=="UkraineRatio"] <- "Ukraine"
colnames(total_cast_melt) <- c("Date", "Country", "Ratio")
total_cast_melt$Type <- "Total"

destroyed_melt <- melt(equipment_losses[,c("Date", "Russia_Destroyed", "Ukraine_Destroyed")], id.var="Date")
destroyed_melt$Date <- as.Date(destroyed_melt$Date, format="%m/%d/%Y")
colnames(destroyed_melt) <- c("Date", "Country", "Destroyed")
destroyed_melt$Country <- gsub("_Destroyed", "", destroyed_melt$Country)
destroyed_melt <- destroyed_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Destroyed - lag(Destroyed, default = first(Destroyed)))
destroyed_cast = reshape2::dcast(destroyed_melt[,c("Date", "Country", "Daily")], formula=Date~Country, fun.aggregate=mean, value.var="Daily")
destroyed_cast$RussiaRatio <- destroyed_cast$Russia/(destroyed_cast$Russia+total_cast$Ukraine)
destroyed_cast$UkraineRatio <- destroyed_cast$Ukraine/(destroyed_cast$Russia+destroyed_cast$Ukraine)
destroyed_cast$Ratio <- destroyed_cast$Russia/destroyed_cast$Ukraine
destroyed_cast <- destroyed_cast[-1,]

destroyed_cast_ratio <- destroyed_cast[,c("Date", "Ratio")]
destroyed_cast_melt <- reshape2::melt(destroyed_cast[,c("Date", "RussiaRatio", "UkraineRatio")], id.var="Date")
destroyed_cast_melt$variable <- as.character(destroyed_cast_melt$variable)
destroyed_cast_melt$variable[destroyed_cast_melt$variable=="RussiaRatio"] <- "Russia"
destroyed_cast_melt$variable[destroyed_cast_melt$variable=="UkraineRatio"] <- "Ukraine"
colnames(destroyed_cast_melt) <- c("Date", "Country", "Ratio")
destroyed_cast_melt$Type <- "Destroyed"


abandoned_melt <- melt(equipment_losses[,c("Date", "Russia_Abandoned", "Ukraine_Abandoned")], id.var="Date")
abandoned_melt$Date <- as.Date(abandoned_melt$Date, format="%m/%d/%Y")
colnames(abandoned_melt) <- c("Date", "Country", "Abandoned")
abandoned_melt$Country <- gsub("_Abandoned", "", abandoned_melt$Country)
abandoned_melt <- abandoned_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Abandoned - lag(Abandoned, default = first(Abandoned)))
abandoned_cast = reshape2::dcast(abandoned_melt[,c("Date", "Country", "Daily")], formula=Date~Country, fun.aggregate=mean, value.var="Daily")
abandoned_cast$RussiaRatio <- abandoned_cast$Russia/(abandoned_cast$Russia+total_cast$Ukraine)
abandoned_cast$UkraineRatio <- abandoned_cast$Ukraine/(abandoned_cast$Russia+destroyed_cast$Ukraine)
abandoned_cast$Ratio <- abandoned_cast$Russia/abandoned_cast$Ukraine
abandoned_cast <- abandoned_cast[-1,]

abandoned_cast_ratio <- abandoned_cast[,c("Date", "Ratio")]
abandoned_cast_melt <- reshape2::melt(abandoned_cast[,c("Date", "RussiaRatio", "UkraineRatio")], id.var="Date")
abandoned_cast_melt$variable <- as.character(abandoned_cast_melt$variable)
abandoned_cast_melt$variable[abandoned_cast_melt$variable=="RussiaRatio"] <- "Russia"
abandoned_cast_melt$variable[abandoned_cast_melt$variable=="UkraineRatio"] <- "Ukraine"
colnames(abandoned_cast_melt) <- c("Date", "Country", "Ratio")
abandoned_cast_melt$Type <- "Abandoned"


captured_melt <- melt(equipment_losses[,c("Date", "Russia_Captured", "Ukraine_Captured")], id.var="Date")
captured_melt$Date <- as.Date(captured_melt$Date, format="%m/%d/%Y")
colnames(captured_melt) <- c("Date", "Country", "Captured")
captured_melt$Country <- gsub("_Captured", "", captured_melt$Country)
captured_melt <- captured_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Captured - lag(Captured, default = first(Captured)))
captured_cast = reshape2::dcast(captured_melt[,c("Date", "Country", "Daily")], formula=Date~Country, fun.aggregate=mean, value.var="Daily")
captured_cast$RussiaRatio <- captured_cast$Russia/(captured_cast$Russia+total_cast$Ukraine)
captured_cast$UkraineRatio <- captured_cast$Ukraine/(captured_cast$Russia+destroyed_cast$Ukraine)
captured_cast$Ratio <- captured_cast$Russia/captured_cast$Ukraine
captured_cast <- captured_cast[-1,]

captured_cast_ratio <- captured_cast[,c("Date", "Ratio")]
captured_cast_melt <- reshape2::melt(captured_cast[,c("Date", "RussiaRatio", "UkraineRatio")], id.var="Date")
captured_cast_melt$variable <- as.character(captured_cast_melt$variable)
captured_cast_melt$variable[captured_cast_melt$variable=="RussiaRatio"] <- "Russia"
captured_cast_melt$variable[captured_cast_melt$variable=="UkraineRatio"] <- "Ukraine"
colnames(captured_cast_melt) <- c("Date", "Country", "Ratio")
captured_cast_melt$Type <- "Captured"

all_types <- as.data.frame(rbindlist(list(total_cast_melt, destroyed_cast_melt, abandoned_cast_melt, captured_cast_melt)))

ratio_plot <- ggplot(data=all_types, aes(Date, Ratio, colour=Country)) +
  geom_point(size=0.1) +
  geom_line(stat="smooth", method="loess", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Lost", limits=c(-0.2, 1.1), labels=scales::percent, breaks=seq(0, 1, 0.25)) +
  ggtitle(paste0("Total equipment lost through ", as.Date("2022-12-31"))) +
  facet_wrap(~Type, nrow=2, ncol=2) +
  theme_light() +
  theme(legend.position="bottom") + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/ratio_grid.jpg", ratio_plot, device="jpg", width=6, height=10)



###All together now
total_melt$Type <- "Total"
colnames(total_melt)[3] <- "Number"
destroyed_melt$Type <- "Destroyed"
colnames(destroyed_melt)[3] <- "Number"
abandoned_melt$Type <- "Abandoned"
colnames(abandoned_melt)[3] <- "Number"
captured_melt$Type <- "Captured"
colnames(captured_melt)[3] <- "Number"

all_melt <- rbindlist(list(destroyed_melt, abandoned_melt, captured_melt))

current_grid <- ggplot(all_melt, aes(Date, Number, colour=Country)) +
  geom_col(data=all_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Equipment Lost") +
  ggtitle(paste0("Total equipment lost through ", as.Date("2022-12-31"))) +
  facet_grid(rows=vars(Type)) +
  theme_light() +
  theme(legend.position="bottom") + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_grid.jpg", current_grid, device="jpg", width=6, height=10)



####Raw Equipment Types
####Tanks
tanks_melt <- melt(equipment_losses[,c("Date", "Russia_Tanks", "Ukraine_Tanks")], id.var="Date")
tanks_melt$Date <- as.Date(tanks_melt$Date, format="%m/%d/%Y")
colnames(tanks_melt) <- c("Date", "Country", "Tanks")
tanks_melt$Country <- gsub("_Tanks", "", tanks_melt$Country)
tanks_melt <- tanks_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Tanks - lag(Tanks, default = first(Tanks)))

current_tanks <- ggplot(tanks_melt, aes(Date, Tanks, colour=Country)) +
  geom_col(data=tanks_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Tanks Lost") +
  ggtitle(paste0("Tanks lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_tanks.jpg", current_tanks, device="jpg", width=6, height=5)

tanks_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Tanks/equipment_losses$Ukraine_Tanks)
tanks_ratio_frame$Date <- as.Date(tanks_ratio_frame$Date, format="%m/%d/%Y")
tanks_ratio_frame <- tanks_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

tank_ratio <-
  ggplot(tanks_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=tanks_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Tank Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Tank loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/tank_ratio.jpg", tank_ratio, device="jpg", width=6, height=5)


####Armored Fighting Vehicles (AFVs)
afv_melt <- melt(equipment_losses[,c("Date", "Russia_AFV", "Ukraine_AFV")], id.var="Date")
afv_melt$Date <- as.Date(afv_melt$Date, format="%m/%d/%Y")
colnames(afv_melt) <- c("Date", "Country", "AFV")
afv_melt$Country <- gsub("_AFV", "", afv_melt$Country)
afv_melt <- afv_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = AFV - lag(AFV, default = first(AFV)))

current_afv <- ggplot(afv_melt, aes(Date, AFV, colour=Country)) +
  geom_col(data=afv_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Armored Fighting Vehicles Lost") +
  ggtitle(paste0("AFV lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_afv.jpg", current_afv, device="jpg", width=6, height=5)

afv_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_AFV/equipment_losses$Ukraine_AFV)
afv_ratio_frame$Date <- as.Date(afv_ratio_frame$Date, format="%m/%d/%Y")
afv_ratio_frame <- afv_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

afv_ratio <-
  ggplot(afv_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=afv_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("AFV Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("AFV loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/afv_ratio.jpg", afv_ratio, device="jpg", width=6, height=5)



####Artillery
artillery_melt <- melt(equipment_losses[,c("Date", "Russia_Artillery", "Ukraine_Artillery")], id.var="Date")
artillery_melt$Date <- as.Date(artillery_melt$Date, format="%m/%d/%Y")
colnames(artillery_melt) <- c("Date", "Country", "Artillery")
artillery_melt$Country <- gsub("_Artillery", "", artillery_melt$Country)
artillery_melt <- artillery_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Artillery - lag(Artillery, default = first(Artillery)))

current_artillery <- ggplot(artillery_melt, aes(Date, Artillery, colour=Country)) +
  geom_col(data=artillery_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Artillery Lost") +
  ggtitle(paste0("Artillery lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_artillery.jpg", current_artillery, device="jpg", width=6, height=5)


artillery_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Artillery/equipment_losses$Ukraine_Artillery)
artillery_ratio_frame$Date <- as.Date(artillery_ratio_frame$Date, format="%m/%d/%Y")
artillery_ratio_frame <- artillery_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

artillery_ratio <-
  ggplot(artillery_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=artillery_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Artillery Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Artillery loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/artillery_ratio.jpg", artillery_ratio, device="jpg", width=6, height=5)



####Infantry Fighting Vehicles (AFVs)
ifv_melt <- melt(equipment_losses[,c("Date", "Russia_IFV", "Ukraine_IFV")], id.var="Date")
ifv_melt$Date <- as.Date(ifv_melt$Date, format="%m/%d/%Y")
colnames(ifv_melt) <- c("Date", "Country", "IFV")
ifv_melt$Country <- gsub("_IFV", "", ifv_melt$Country)
ifv_melt <- ifv_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = IFV - lag(IFV, default = first(IFV)))

current_ifv <- ggplot(ifv_melt, aes(Date, IFV, colour=Country)) +
  geom_col(data=ifv_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Infantry Fighting Vehicles Lost") +
  ggtitle(paste0("IFV lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_ifv.jpg", current_ifv, device="jpg", width=6, height=5)

ifv_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_IFV/equipment_losses$Ukraine_IFV)
ifv_ratio_frame$Date <- as.Date(ifv_ratio_frame$Date, format="%m/%d/%Y")
ifv_ratio_frame <- ifv_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

ifv_ratio <-
  ggplot(ifv_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=ifv_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("IFV Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("IFV loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/ifv_ratio.jpg", ifv_ratio, device="jpg", width=6, height=5)


####Armored Personal Carriers
apc_melt <- melt(equipment_losses[,c("Date", "Russia_APC", "Ukraine_APC")], id.var="Date")
apc_melt$Date <- as.Date(apc_melt$Date, format="%m/%d/%Y")
colnames(apc_melt) <- c("Date", "Country", "APC")
apc_melt$Country <- gsub("_APC", "", apc_melt$Country)
apc_melt <- apc_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = APC - lag(APC, default = first(APC)))

current_apc <- ggplot(apc_melt, aes(Date, APC, colour=Country)) +
  geom_col(data=apc_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Armored Personal Carriers Lost") +
  ggtitle(paste0("APC lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_apc.jpg", current_apc, device="jpg", width=6, height=5)

apc_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_APC/equipment_losses$Ukraine_APC)
apc_ratio_frame$Date <- as.Date(apc_ratio_frame$Date, format="%m/%d/%Y")
apc_ratio_frame <- apc_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

apc_ratio <-
  ggplot(apc_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=apc_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("APC Loss Ratio Ru:Ukr") +
  ggtitle(paste0("APC loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/apc_ratio.jpg", apc_ratio, device="jpg", width=6, height=5)


####Infantry Mobility Vehicles (IMVs)
imv_melt <- melt(equipment_losses[,c("Date", "Russia_IMV", "Ukraine_IMV")], id.var="Date")
imv_melt$Date <- as.Date(imv_melt$Date, format="%m/%d/%Y")
colnames(imv_melt) <- c("Date", "Country", "IMV")
imv_melt$Country <- gsub("_IMV", "", imv_melt$Country)
imv_melt <- imv_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = IMV - lag(IMV, default = first(IMV)))

current_imv <- ggplot(imv_melt, aes(Date, IMV, colour=Country)) +
  geom_col(data=imv_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Infantry Mobility Vehicles Lost") +
  ggtitle(paste0("IMV lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_imv.jpg", current_imv, device="jpg", width=6, height=5)

imv_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_IMV/equipment_losses$Ukraine_IMV)
imv_ratio_frame$Date <- as.Date(imv_ratio_frame$Date, format="%m/%d/%Y")
imv_ratio_frame <- imv_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

imv_ratio <-
  ggplot(imv_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=imv_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("IMV Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("IMV loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/imv_ratio.jpg", imv_ratio, device="jpg", width=6, height=5)


####Engineering Vehicles (EVs)
ev_melt <- melt(equipment_losses[,c("Date", "Russia_Engineering", "Ukraine_Engineering")], id.var="Date")
ev_melt$Date <- as.Date(ev_melt$Date, format="%m/%d/%Y")
colnames(ev_melt) <- c("Date", "Country", "EV")
#ev_melt[ev_melt$EV==0] <- 0.1
ev_melt$Country <- gsub("_Engineering", "", ev_melt$Country)
ev_melt <- ev_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = EV - lag(EV, default = first(EV)))

current_ev <- ggplot(ev_melt, aes(Date, EV, colour=Country)) +
  geom_col(data=ev_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Engineering Vehicles Lost") +
  ggtitle(paste0("EV lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_ev.jpg", current_ev, device="jpg", width=6, height=5)

ev_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Engineering/equipment_losses$Ukraine_Engineering)
ev_ratio_frame$Date <- as.Date(ev_ratio_frame$Date, format="%m/%d/%Y")
ev_ratio_frame <- ev_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

ev_ratio <-
  ggplot(ev_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=ev_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Engineering Vehicle Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Engineering vehicle loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/ev_ratio.jpg", ev_ratio, device="jpg", width=6, height=5)


####Vehicles
vehicles_melt <- melt(equipment_losses[,c("Date", "Russia_Vehicles", "Ukraine_Vehicles")], id.var="Date")
vehicles_melt$Date <- as.Date(vehicles_melt$Date, format="%m/%d/%Y")
colnames(vehicles_melt) <- c("Date", "Country", "Vehicles")
vehicles_melt$Country <- gsub("_Vehicles", "", vehicles_melt$Country)
vehicles_melt <- vehicles_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Vehicles - lag(Vehicles, default = first(Vehicles)))

current_vehicles <- ggplot(vehicles_melt, aes(Date, Vehicles, colour=Country)) +
  geom_col(data=vehicles_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Vehicles Lost") +
  ggtitle(paste0("Vehicles lost through ", as.Date("2022-12-31"))) +
  theme_light()+ 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_vehicles.jpg", current_vehicles, device="jpg", width=6, height=5)


vehicle_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Vehicles/equipment_losses$Ukraine_Vehicles)
vehicle_ratio_frame$Date <- as.Date(vehicle_ratio_frame$Date, format="%m/%d/%Y")
vehicle_ratio_frame <- vehicle_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

vehicle_ratio <-
  ggplot(vehicle_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=vehicle_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Vehicle Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Vehicle loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/vehicle_ratio.jpg", vehicle_ratio, device="jpg", width=6, height=5)

####Synthetic Units (combinations of equipment categories to highlight strategy)

####Aircraft
aircraft_melt <- melt(equipment_losses[,c("Date", "Russia_Aircraft", "Ukraine_Aircraft")], id.var="Date")
aircraft_melt$Date <- as.Date(aircraft_melt$Date, format="%m/%d/%Y")
colnames(aircraft_melt) <- c("Date", "Country", "Aircraft")
aircraft_melt$Country <- gsub("_Aircraft", "", aircraft_melt$Country)
aircraft_melt <- aircraft_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Aircraft - lag(Aircraft, default = first(Aircraft)))

current_aircraft <- ggplot(aircraft_melt, aes(Date, Aircraft, colour=Country)) +
  geom_col(data=aircraft_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Aircraft Lost") +
  ggtitle(paste0("Aircraft lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_aircraft.jpg", current_aircraft, device="jpg", width=6, height=5)

aircraft_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Aircraft/equipment_losses$Ukraine_Aircraft)
aircraft_ratio_frame$Date <- as.Date(aircraft_ratio_frame$Date, format="%m/%d/%Y")
aircraft_ratio_frame <- aircraft_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

aircraft_ratio <-
  ggplot(aircraft_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=aircraft_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Aircraft Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Aircraft loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/aircraft_ratio.jpg", aircraft_ratio, device="jpg", width=6, height=5)


####Anti-Aircraft
antiaircraft_melt <- melt(equipment_losses[,c("Date", "Russia_Antiair", "Ukraine_Antiair")], id.var="Date")
antiaircraft_melt$Date <- as.Date(antiaircraft_melt$Date, format="%m/%d/%Y")
colnames(antiaircraft_melt) <- c("Date", "Country", "Antiair")
antiaircraft_melt$Country <- gsub("_Antiair", "", antiaircraft_melt$Country)
antiaircraft_melt <- antiaircraft_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Antiair - lag(Antiair, default = first(Antiair)))

current_antiair <- ggplot(antiaircraft_melt, aes(Date, Antiair, colour=Country)) +
  geom_col(data=antiaircraft_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Anti-Air Systems Lost") +
  ggtitle(paste0("Anti-air systems lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_antiair.jpg", current_antiair, device="jpg", width=6, height=5)

antiair_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Antiair/equipment_losses$Ukraine_Antiair)
antiair_ratio_frame$Date <- as.Date(antiair_ratio_frame$Date, format="%m/%d/%Y")
antiair_ratio_frame <- antiair_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

antiair_ratio <-
  ggplot(antiair_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=antiair_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Anti-Air Systems Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Anti-air system loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/antiair_ratio.jpg", antiair_ratio, device="jpg", width=6, height=5)


####Infantry
infantry_melt <- melt(equipment_losses[,c("Date", "Russia_Infantry", "Ukraine_Infantry")], id.var="Date")
infantry_melt$Date <- as.Date(infantry_melt$Date, format="%m/%d/%Y")
colnames(infantry_melt) <- c("Date", "Country", "Infantry")
infantry_melt$Country <- gsub("_Infantry", "", infantry_melt$Country)
infantry_melt <- infantry_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Infantry - lag(Infantry, default = first(Infantry)))

current_infantry <- ggplot(infantry_melt, aes(Date, Infantry, colour=Country)) +
  geom_col(data=infantry_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Infantry Support Lost") +
  ggtitle(paste0("Infantry support lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_infantry.jpg", current_infantry, device="jpg", width=6, height=5)

infantry_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Infantry/equipment_losses$Ukraine_Infantry)
infantry_ratio_frame$Date <- as.Date(infantry_ratio_frame$Date, format="%m/%d/%Y")
infantry_ratio_frame <- infantry_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

infantry_ratio <-
  ggplot(infantry_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=infantry_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Anti-Air Systems Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Anti-air system loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/infantry_ratio.jpg", infantry_ratio, device="jpg", width=6, height=5)


####Armor
armor_melt <- melt(equipment_losses[,c("Date", "Russia_Armor", "Ukraine_Armor")], id.var="Date")
armor_melt$Date <- as.Date(armor_melt$Date, format="%m/%d/%Y")
colnames(armor_melt) <- c("Date", "Country", "Armor")
armor_melt$Country <- gsub("_Armor", "", armor_melt$Country)
armor_melt <- armor_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Armor - lag(Armor, default = first(Armor)))

current_armor <- ggplot(armor_melt, aes(Date, Armor, colour=Country)) +
  geom_col(data=armor_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Armor Lost") +
  ggtitle(paste0("Armor support lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_armor.jpg", current_armor, device="jpg", width=6, height=5)

armor_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Armor/equipment_losses$Ukraine_Armor)
armor_ratio_frame$Date <- as.Date(armor_ratio_frame$Date, format="%m/%d/%Y")
armor_ratio_frame <- armor_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

armor_ratio <-
  ggplot(armor_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=armor_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Armor Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Armor loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/armor_ratio.jpg", armor_ratio, device="jpg", width=6, height=5)


####Logistics
logistics_melt <- melt(equipment_losses[,c("Date", "Russia_Logistics", "Ukraine_Logistics")], id.var="Date")
logistics_melt$Date <- as.Date(logistics_melt$Date, format="%m/%d/%Y")
colnames(logistics_melt) <- c("Date", "Country", "Logistics")
logistics_melt$Country <- gsub("_Logistics", "", logistics_melt$Country)
logistics_melt <- logistics_melt %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Logistics - lag(Logistics, default = first(Logistics)))

current_logistics <- ggplot(logistics_melt, aes(Date, Logistics, colour=Country)) +
  geom_col(data=logistics_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Logistics Systems Lost") +
  ggtitle(paste0("Logistics systems lost through ", as.Date("2022-12-31"))) +
  theme_light() + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_logistics.jpg", current_logistics, device="jpg", width=6, height=5)

logistics_ratio_frame <- data.frame(Date=equipment_losses$Date, Ratio=equipment_losses$Russia_Logistics/equipment_losses$Ukraine_Logistics)
logistics_ratio_frame$Date <- as.Date(logistics_ratio_frame$Date, format="%m/%d/%Y")
logistics_ratio_frame <- logistics_ratio_frame %>%
  arrange(Date) %>%
  mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

logistics_ratio <-
  ggplot(logistics_ratio_frame, aes(Date, Ratio)) +
  geom_col(data=logistics_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Logistics Systems Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Logistics systems loss ratio through ", as.Date("2022-12-31"))) +
  theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/logistics_ratio.jpg", logistics_ratio, device="jpg", width=6, height=5)


###Analysis
empty_columns <- colSums(is.na(equipment_losses) | equipment_losses == "") == nrow(equipment_losses)
equipment_totals <- equipment_losses[,c("Date", "Russia_Total", "Ukraine_Total", "Russia_Destroyed", "Ukraine_Destroyed", "Russia_Damaged", "Ukraine_Damaged", "Russia_Abandoned", "Ukraine_Abandoned", "Russia_Captured", "Ukraine_Captured", "Russia_Tanks", "Ukraine_Tanks", "Russia_Tank_Capture", "Ukraine_Tank_Capture", "Russia_AFV", "Ukraine_AFV", "Russia_AFV_Capture", "Ukraine_AFV_Capture", "Russia_IFV", "Ukraine_IFV", "Russia_APC", "Ukraine_APC", "Russia_IMV", "Ukraine_IMV", "Ukraine_Engineering", "Russia_Engineering", "Russia_Coms", "Ukraine_Coms", "Russia_Vehicles", "Ukraine_Vehicles", "Russia_Aircraft", "Ukraine_Aircraft", "Russia_Infantry", "Ukraine_Infantry", "Russia_Logistics", "Ukraine_Logistics", "Russia_Armor", "Ukraine_Armor", "Russia_Antiair", "Ukraine_Antiair", "Russia_Artillery", "Ukraine_Artillery")]
equipment_totals <- equipment_totals[complete.cases(equipment_totals),]
equipment_totals <- equipment_totals[nrow(equipment_totals),]

equipment_ratios <- data.frame(Total=equipment_totals[,"Russia_Total"]/equipment_totals[,"Ukraine_Total"],
                               Destroyed=equipment_totals[,"Russia_Destroyed"]/equipment_totals[,"Ukraine_Destroyed"],
                               Damaged=equipment_totals[,"Russia_Damaged"]/equipment_totals[,"Ukraine_Damaged"],
                               Abandoned=equipment_totals[,"Russia_Abandoned"]/equipment_totals[,"Ukraine_Abandoned"],
                               Captured=equipment_totals[,"Russia_Captured"]/equipment_totals[,"Ukraine_Captured"],
                               Tanks=equipment_totals[,"Russia_Tanks"]/equipment_totals[,"Ukraine_Tanks"],
                               AFV=equipment_totals[,"Russia_AFV"]/equipment_totals[,"Ukraine_AFV"],
                               IFV=equipment_totals[,"Russia_IFV"]/equipment_totals[,"Ukraine_IFV"],
                               APC=equipment_totals[,"Russia_APC"]/equipment_totals[,"Ukraine_APC"],
                               IMV=equipment_totals[,"Russia_IMV"]/equipment_totals[,"Ukraine_IMV"],
                               Engineering=equipment_totals[,"Russia_Engineering"]/equipment_totals[,"Ukraine_Engineering"],
                               Engineering=equipment_totals[,"Russia_Coms"]/equipment_totals[,"Ukraine_Coms"],
                               Vehicles=equipment_totals[,"Russia_Vehicles"]/equipment_totals[,"Ukraine_Vehicles"],
                               Aircraft=equipment_totals[,"Russia_Aircraft"]/equipment_totals[,"Ukraine_Aircraft"],
                               Infantry=equipment_totals[,"Russia_Infantry"]/equipment_totals[,"Ukraine_Infantry"],
                               Logistics=equipment_totals[,"Russia_Logistics"]/equipment_totals[,"Ukraine_Logistics"],
                               Armor=equipment_totals[,"Russia_Armor"]/equipment_totals[,"Ukraine_Armor"],
                               Antiair=equipment_totals[,"Russia_Antiair"]/equipment_totals[,"Ukraine_Antiair"],
                               Artillery=equipment_totals[,"Russia_Artillery"]/equipment_totals[,"Ukraine_Artillery"]
)
equipment_ratios_t <- data.frame(Type=gsub("Russia_", "", names(equipment_ratios)), Ratio=t(equipment_ratios))


loss_type <- ggplot(equipment_ratios_t[equipment_ratios_t$Type %in% c("Destroyed", "Abandoned", "Captured"),], aes(Type, Ratio, colour=Type, fill=Type)) +
  geom_col() +
  scale_y_continuous("Ratio (Russian/Ukrainian Losses)", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Loss ratios lost through ", as.Date("2022-12-31"))) +
  theme_light()
  #scale_fill_brewer(palette="Accent") +
  #scale_color_brewer(palette="Accent")
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_loss_type.jpg", loss_type, device="jpg", width=6, height=5)

unit_type <- ggplot(equipment_ratios_t[equipment_ratios_t$Type %in% c("Aircraft", "Antiair", "Artillery", "Infantry", "Armor", "Vehicles", "Logistics"),], aes(Type, Ratio, colour=Type, fill=Type)) +
  geom_col() +
  scale_y_continuous("Ratio (Russian/Ukrainian Losses)", breaks = function(x) unique(floor(pretty(seq(min(x)-1, (max(x) + 1) * 1.1))))) +
  ggtitle(paste0("Unit type ratios lost through ", as.Date("2022-12-31"))) +
  theme_light()
  #scale_fill_brewer(palette="Accent") +
  #scale_color_brewer(palette="Accent")
ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_unit_type.jpg", unit_type, device="jpg", width=6, height=5)


###Relative Tank Losses
### Percent Loss estimate per common request
### Total Tanks sourced from https://inews.co.uk/news/world/russia-tanks-how-many-putin-armoured-forces-ukraine-nato-explained-1504470
percent_tanks <- equipment_losses  %>%
  dplyr::select(Date, Russia = Russia_Tanks, Ukraine = Ukraine_Tanks, Russia_Capture=Russia_Tank_Capture, Ukraine_Capture=Ukraine_Tank_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 13300 - Russia + Russia_Capture,
         UT = 2100 - Ukraine + Ukraine_Capture,
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "Tanks")


percent_tanks <- percent_tanks %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Tanks - lag(Tanks, default = first(Tanks)))


current_percent_total_tanks <- ggplot(data=percent_tanks, mapping=aes(Date, Tanks, colour=Country)) +
  geom_col(data=percent_tanks, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "Tank Losses [% of total tanks]") +
  ggtitle(paste0("Proportional tank losses through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_total_tanks.jpg", current_percent_total_tanks, device="jpg", width=6, height=5)

###Percent Tanks Baseline Adjusted
percent_tanks <- equipment_losses  %>%
  dplyr::select(Date, Russia_Tanks = Russia_Tanks, Ukraine_Tanks = Ukraine_Tanks, Russia_Capture=Russia_Tank_Capture, Ukraine_Capture=Ukraine_Tank_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 13300,
         UT = 2100,
         Russia = Russia_Capture - Russia_Tanks,
         Ukraine = Ukraine_Capture - Ukraine_Tanks,
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "Tanks")


percent_tanks <- percent_tanks %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Tanks - lag(Tanks, default = first(Tanks)))


current_percent_total_tanks <- ggplot(data=percent_tanks, mapping=aes(Date, Tanks, colour=Country)) +
  geom_col(data=percent_tanks, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "Tank Gains/Losses [% of total tanks]") +
  ggtitle(paste0("Proportion of total tanks gained or lost through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_total_tanks_baseline.jpg", current_percent_total_tanks, device="jpg", width=6, height=5, dpi=600)

### Percent Loss estimate per common request
### Deployed Tanks sourced from https://en.as.com/en/2022/02/24/latest_news/1645729870_894320.html
percent_tanks <- equipment_losses  %>%
  dplyr::select(Date, Russia = Russia_Tanks, Ukraine = Ukraine_Tanks, Russia_Capture=Russia_Tank_Capture, Ukraine_Capture=Ukraine_Tank_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 2840 + Russia_Capture,
         UT = 2100 + Ukraine_Capture,
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "Tanks")


percent_tanks <- percent_tanks %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Tanks - lag(Tanks, default = first(Tanks)))


current_percent_deployed_tanks <- ggplot(data=percent_tanks, mapping=aes(Date, Tanks, colour=Country)) +
  geom_col(data=percent_tanks, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "Tank Losses [% of deployed tanks]") +
  ggtitle(paste0("Proportional tank losses through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_deployed_tanks.jpg", current_percent_deployed_tanks, device="jpg", width=6, height=5)

###Percent Tanks Baseline Adjusted
percent_tanks <- equipment_losses  %>%
  dplyr::select(Date, Russia_Tanks = Russia_Tanks, Ukraine_Tanks = Ukraine_Tanks, Russia_Capture=Russia_Tank_Capture, Ukraine_Capture=Ukraine_Tank_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 2840,
         UT = 2100,
         Russia = Russia_Capture - Russia_Tanks,
         Ukraine = Ukraine_Capture - Ukraine_Tanks,
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "Tanks")


percent_tanks <- percent_tanks %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Tanks - lag(Tanks, default = first(Tanks)))


current_percent_deployed_tanks <- ggplot(data=percent_tanks, mapping=aes(Date, Tanks, colour=Country)) +
  geom_col(data=percent_tanks, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "Tank Gains/Losses [% of deployed tanks]") +
  ggtitle(paste0("Proportion of deployed tanks gained or lost through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_deployed_tanks_baseline.jpg", current_percent_deployed_tanks, device="jpg", width=6, height=5)


###Relative AFV Losses
### Percent Loss estimate per common request
### Total Tanks sourced from https://inews.co.uk/news/world/russia-tanks-how-many-putin-armoured-forces-ukraine-nato-explained-1504470
percent_afv <- equipment_losses  %>%
  dplyr::select(Date, Russia = Russia_AFV, Ukraine = Ukraine_AFV, Russia_Capture = Russia_AFV_Capture, Ukraine_Capture = Ukraine_AFV_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 20000 - Russia + Russia_Capture,
         UT = 2870 - Ukraine + Ukraine_Capture,
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "AFV") 


percent_afv <- percent_afv %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = AFV - lag(AFV, default = first(AFV)))


current_percent_afv <- ggplot(data=percent_afv, mapping=aes(Date, AFV, colour=Country)) +
  geom_col(data=percent_afv, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "AFV Losses [% of AFV]") +
  ggtitle(paste0("Proportional AFV losses through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_afv.jpg", current_percent_afv, device="jpg", width=6, height=5)

###Percent AFV Baseline Adjusted
percent_afv<- equipment_losses  %>%
  dplyr::select(Date, Russia_AFV = Russia_AFV, Ukraine_AFV = Ukraine_AFV, Russia_Capture=Russia_AFV_Capture, Ukraine_Capture=Ukraine_AFV_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 20000,
         UT = 2870,
         Russia = Russia_Capture - Russia_AFV,
         Ukraine = Ukraine_Capture - Ukraine_AFV,
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "AFV")


percent_afv <- percent_afv %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = AFV - lag(AFV, default = first(AFV)))


current_percent_total_afv <- ggplot(data=percent_afv, mapping=aes(Date, AFV, colour=Country)) +
  geom_col(data=percent_afv, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "AFV Gains/Losses [% of total AFVs]") +
  ggtitle(paste0("Proportion of total AFVs gained or lost through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_total_afv_baseline.jpg", current_percent_total_afv, device="jpg", width=6, height=5, dpi=600)

###Percent Armor Baseline Adjusted
percent_armor <- equipment_losses  %>%
  dplyr::select(Date, Russia_Tanks = Russia_Tanks, Ukraine_Tanks = Ukraine_Tanks, Russia_Tank_Capture=Russia_Tank_Capture, Ukraine_Tank_Capture=Ukraine_Tank_Capture, Russia_AFV = Russia_AFV, Ukraine_AFV = Ukraine_AFV, Russia_AFV_Capture=Russia_AFV_Capture, Ukraine_AFV_Capture=Ukraine_AFV_Capture) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         RT = 20000 + 13300,
         UT = 2870 + 2100,
         Russia = (Russia_AFV_Capture + Ukraine_AFV_Capture) - (Russia_AFV + Russia_Tanks),
         Ukraine = (Ukraine_AFV_Capture + Ukraine_Tank_Capture) - (Ukraine_AFV+Ukraine_Tanks),
         Russia =  Russia / RT,
         Ukraine = Ukraine / UT) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "Armor")


percent_armor <- percent_armor %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Armor - lag(Armor, default = first(Armor)))


current_percent_total_armor <- ggplot(data=percent_armor, mapping=aes(Date, Armor, colour=Country)) +
  geom_col(data=percent_armor, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "Armor Gains/Losses [% of total Armor]") +
  ggtitle(paste0("Proportion of total Armor gained or lost through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_percent_total_armor_baseline.jpg", current_percent_total_armor, device="jpg", width=6, height=5, dpi=600)

###Relative Absolute Changes
###Percent AFV Baseline Adjusted
absolute_units <- equipment_losses  %>%
  dplyr::select(Date, Russia_Total = Russia_Total, Ukraine_Total = Ukraine_Total, Russia_Capture=Ukraine_Captured, Ukraine_Capture=Russia_Captured) %>%
  mutate(Date = as.Date(Date, format="%m-%d-%Y", origin="1970-01-01"),
         Russia_Total_Adjusted = Russia_Total - Ukraine_Capture,
         Ukraine_Total_Adjusted = Ukraine_Total - Russia_Capture,
         Russia = Russia_Capture - Russia_Total_Adjusted,
         Ukraine = Ukraine_Capture - Ukraine_Total_Adjusted) %>%
  pivot_longer(cols = c("Russia", "Ukraine"),
               names_to = "Country",
               values_to = "Net")


absolute_units <- absolute_units %>% group_by(Country) %>%
  arrange(Date) %>%
  mutate(Daily = Net - lag(Net, default = first(Net)))


current_absolute_total <- ggplot(data=absolute_units, mapping=aes(Date, Net, colour=Country)) +
  geom_col(data=absolute_units, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point(show.legend=FALSE, size=0.1) +
  geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) + 
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  labs(y = "Absolute Equipment Gains/Losses") +
  ggtitle(paste0("Equipment gained or lost through ", as.Date("2022-12-31"))) +
  theme_light()  + 
  scale_colour_manual(values = country_colors)  + 
  scale_fill_manual(values = country_colors)

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/current_absolute_total.jpg", current_absolute_total, device="jpg", width=6, height=5, dpi=600)

###Map


btgs <- read.csv("https://raw.githubusercontent.com/simonhuwiler/uawardata/master/data/csv/btgs_current.csv")
colnames(btgs)[3] <- "lon"
colnames(btgs)[5] <- "Russian_BTGS"


###FIRMS Analysis
dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")

firms_list <- list()
for(i in dates){
  tryCatch(firms_list[[as.character(i)]] <- data.table::fread(paste0("~/GitHub/Russia-Ukraine/data/FIRMS/",  as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), ".csv"))[,-1], error=function(e) NULL)
}

new_firms_frame <- as.data.frame(data.table::rbindlist(firms_list, use.names=TRUE, fill=TRUE))

kyiv_firms <- new_firms_frame[new_firms_frame$latitude < 52 & new_firms_frame$latitude > 50 & new_firms_frame$longitude < 32 & new_firms_frame$longitude > 29,]
kyiv_dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")
kyiv_date_firms <- list()
kyiv_means_firms <- list()
for(i in kyiv_dates){
  kyiv_date_firms[[i]] <- kyiv_firms[as.Date(kyiv_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
  kyiv_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(kyiv_date_firms[[i]]$frp), Region="Kyiv")
}
kyiv_firms_summary <- as.data.frame(data.table::rbindlist(kyiv_means_firms))

north_donbas_dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")
north_donbas_firms <- new_firms_frame[new_firms_frame$latitude < 50 & new_firms_frame$latitude > 48.5 & new_firms_frame$longitude < 39 & new_firms_frame$longitude > 36,]
north_donbas_date_firms <- list()
north_donbas_means_firms <- list()
for(i in north_donbas_dates){
  north_donbas_date_firms[[i]] <- north_donbas_firms[as.Date(north_donbas_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
  north_donbas_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(north_donbas_date_firms[[i]]$frp), Region="North Donbas")
}
north_donbas_firms_summary <- as.data.frame(data.table::rbindlist(north_donbas_means_firms))

south_donbas_dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")
south_donbas_firms <- new_firms_frame[new_firms_frame$latitude < 48.5 & new_firms_frame$latitude > 46.5 & new_firms_frame$longitude < 39 & new_firms_frame$longitude > 36,]
south_donbas_date_firms <- list()
south_donbas_means_firms <- list()
for(i in south_donbas_dates){
  south_donbas_date_firms[[i]] <- south_donbas_firms[as.Date(south_donbas_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
  south_donbas_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(south_donbas_date_firms[[i]]$frp), Region="South Donbas")
}
south_donbas_firms_summary <- as.data.frame(data.table::rbindlist(south_donbas_means_firms))

zaporizhizhia_dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")
zaporizhizhia_firms <- new_firms_frame[new_firms_frame$latitude < 47.3 & new_firms_frame$latitude > 46.0 & new_firms_frame$longitude < 37 & new_firms_frame$longitude > 34.2,]
zaporizhizhia_date_firms <- list()
zaporizhizhia_means_firms <- list()
for(i in zaporizhizhia_dates){
    zaporizhizhia_date_firms[[i]] <- zaporizhizhia_firms[as.Date(zaporizhizhia_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    zaporizhizhia_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(zaporizhizhia_date_firms[[i]]$frp), Region="Zaporizhizhia")
}
zaporizhizhia_firms_summary <- as.data.frame(data.table::rbindlist(zaporizhizhia_means_firms))


kherson_dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")
kherson_firms <- new_firms_frame[new_firms_frame$latitude < 47.3 & new_firms_frame$latitude > 45.5 & new_firms_frame$longitude < 34.2 & new_firms_frame$longitude > 32.6,]
kherson_date_firms <- list()
kherson_means_firms <- list()
for(i in kherson_dates){
    kherson_date_firms[[i]] <- kherson_firms[as.Date(kherson_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    kherson_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(kherson_date_firms[[i]]$frp), Region="Kherson")
}
kherson_firms_summary <- as.data.frame(data.table::rbindlist(kherson_means_firms))

crimea_dates = seq(as.Date("2022-09-01"), as.Date("2022-12-31"), by="days")
crimea_firms <- new_firms_frame[new_firms_frame$latitude < 47.3 & new_firms_frame$latitude > 44.3 & new_firms_frame$longitude < 34.2 & new_firms_frame$longitude > 33.4,]
crimea_date_firms <- list()
crimea_means_firms <- list()
for(i in crimea_dates){
    crimea_date_firms[[i]] <- crimea_firms[as.Date(kherson_firms$acq_date, format="%Y-%m-%d", origin="1970-01-01") %in% as.Date(i, format="%Y-%m-%d", origin="1970-01-01"),]
    crimea_means_firms[[i]] <- data.frame(Date=as.Date(i, format="%Y-%m-%d", origin="1970-01-01"), FRP=sum(crimea_date_firms[[i]]$frp), Region="Crimea")
}
crimea_firms_summary <- as.data.frame(data.table::rbindlist(crimea_means_firms))



north_firms_summary <- as.data.frame(data.table::rbindlist(list(kyiv_firms_summary, north_donbas_firms_summary, south_donbas_firms_summary)))

north_firms_summary_plot <- ggplot(north_firms_summary, aes(Date, FRP, colour=Region, lty=Region)) +
  geom_point() +
  geom_line() +
  #stat_smooth(method="gam") +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Fire Radiative Power (MegaWatts)", breaks=scales::pretty_breaks(n=10), labels=scales::comma) +
  ggtitle("FIRMS VIIRS I-Band 375 m Active Fire") +
  theme_light()

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/north_firms_summary_plot.jpg", north_firms_summary_plot, device="jpg", width=6, height=5, dpi=600)


south_firms_summary <- as.data.frame(data.table::rbindlist(list(zaporizhizhia_firms_summary, kherson_firms_summary, crimea_firms_summary)))

south_firms_summary_plot <- ggplot(south_firms_summary, aes(Date, FRP, colour=Region, lty=Region)) +
  geom_point() +
  geom_line() +
  #stat_smooth(method="gam") +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("Total Fire Radiative Power (MegaWatts)", breaks=scales::pretty_breaks(n=10), labels=scales::comma) +
  ggtitle("FIRMS VIIRS I-Band 375 m Active Fire") +
  theme_light()

ggsave("~/Github/Russia-Ukraine/Plots/2022-09-01/south_firms_summary_plot.jpg", south_firms_summary_plot, device="jpg", width=6, height=5, dpi=600)
