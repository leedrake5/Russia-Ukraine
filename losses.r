library(ggplot2)
library(RCurl)
library(reshape2)
library(data.table)

equipment_losses <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0")

####Totals
total_melt <- melt(equipment_losses[,c("Date", "Russia_Total", "Ukraine_Total")], id.var="Date")
total_melt$Date <- as.Date(total_melt$Date, format="%m/%d/%Y")
colnames(total_melt) <- c("Date", "Country", "Total")
total_melt$Country <- gsub("_Total", "", total_melt$Country)

current_total <- ggplot(total_melt, aes(Date, Total, colour=Country, shape=Country)) +
geom_point() +
stat_smooth() +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Losses") +
ggtitle(paste0("Total equipment losses through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/current_total.jpg", current_total, device="jpg", width=5, height=5)

####Destroyed
destroyed_melt <- melt(equipment_losses[,c("Date", "Russia_Destroyed", "Ukraine_Destroyed")], id.var="Date")
destroyed_melt$Date <- as.Date(destroyed_melt$Date, format="%m/%d/%Y")
colnames(destroyed_melt) <- c("Date", "Country", "Destroyed")
destroyed_melt$Country <- gsub("_Destroyed", "", destroyed_melt$Country)

current_destroyed <- ggplot(destroyed_melt, aes(Date, Destroyed, colour=Country, shape=Country)) +
geom_point() +
stat_smooth() +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Destroyed") +
ggtitle(paste0("Total equipment destroyed through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/current_destroyed.jpg", current_destroyed, device="jpg", width=5, height=5)

####Abandoned
abandoned_melt <- melt(equipment_losses[,c("Date", "Russia_Abandoned", "Ukraine_Abandoned")], id.var="Date")
abandoned_melt$Date <- as.Date(abandoned_melt$Date, format="%m/%d/%Y")
colnames(abandoned_melt) <- c("Date", "Country", "Abandoned")
abandoned_melt$Country <- gsub("_Abandoned", "", abandoned_melt$Country)

current_abandoned <- ggplot(abandoned_melt, aes(Date, Abandoned, colour=Country, shape=Country)) +
geom_point() +
stat_smooth() +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Abandoned") +
ggtitle(paste0("Total equipment abandoned through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/current_abandoned.jpg", current_abandoned, device="jpg", width=5, height=5)

####Captured
captured_melt <- melt(equipment_losses[,c("Date", "Russia_Captured", "Ukraine_Captured")], id.var="Date")
captured_melt$Date <- as.Date(captured_melt$Date, format="%m/%d/%Y")
colnames(captured_melt) <- c("Date", "Country", "Captured")
captured_melt$Country <- gsub("_Captured", "", captured_melt$Country)

current_captured <- ggplot(captured_melt, aes(Date, Captured, colour=Country, shape=Country)) +
geom_point() +
stat_smooth() +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Captured") +
ggtitle(paste0("Total equipment captured through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/current_captured.jpg", current_captured, device="jpg", width=5, height=5)


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

current_grid <- ggplot(all_melt, aes(Date, Number, colour=Country, shape=Country)) +
geom_point() +
stat_smooth() +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment") +
ggtitle(paste0("Total equipment through ", Sys.Date())) +
facet_grid(rows=vars(Type)) +
theme_light()
ggsave("~/Github/Russia-Ukraine/current_grid.jpg", current_grid, device="jpg", width=5, height=10)
