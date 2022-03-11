library(ggplot2)
library(RCurl)
library(reshape2)
library(data.table)
library(gsheet)
library(dplyr)
library(tidyverse)
library(lubridate)
library(scales)

equipment_losses <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0")

####Totals
total_melt <- melt(equipment_losses[,c("Date", "Russia_Total", "Ukraine_Total")], id.var="Date")
total_melt$Date <- as.Date(total_melt$Date, format="%m/%d/%Y")
colnames(total_melt) <- c("Date", "Country", "Total")
total_melt$Country <- gsub("_Total", "", total_melt$Country)
total_melt <- total_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Total - lag(Total, default = first(Total)))

current_total <- ggplot(total_melt, aes(Date, Total, colour=Country, shape=Country)) +
geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Losses") +
ggtitle(paste0("Total equipment losses through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_total.jpg", current_total, device="jpg", width=6, height=5)

####Destroyed
destroyed_melt <- melt(equipment_losses[,c("Date", "Russia_Destroyed", "Ukraine_Destroyed")], id.var="Date")
destroyed_melt$Date <- as.Date(destroyed_melt$Date, format="%m/%d/%Y")
colnames(destroyed_melt) <- c("Date", "Country", "Destroyed")
destroyed_melt$Country <- gsub("_Destroyed", "", destroyed_melt$Country)
destroyed_melt <- destroyed_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Destroyed - lag(Destroyed, default = first(Destroyed)))


current_destroyed <- ggplot(destroyed_melt, aes(Date, Destroyed, colour=Country, shape=Country)) +
geom_col(data=destroyed_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Destroyed") +
ggtitle(paste0("Total equipment destroyed through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_destroyed.jpg", current_destroyed, device="jpg", width=6, height=5)

####Abandoned
abandoned_melt <- melt(equipment_losses[,c("Date", "Russia_Abandoned", "Ukraine_Abandoned")], id.var="Date")
abandoned_melt$Date <- as.Date(abandoned_melt$Date, format="%m/%d/%Y")
colnames(abandoned_melt) <- c("Date", "Country", "Abandoned")
abandoned_melt$Country <- gsub("_Abandoned", "", abandoned_melt$Country)
abandoned_melt <- abandoned_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Abandoned - lag(Abandoned, default = first(Abandoned)))

current_abandoned <- ggplot(abandoned_melt, aes(Date, Abandoned, colour=Country, shape=Country)) +
geom_col(data=abandoned_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Abandoned") +
ggtitle(paste0("Total equipment abandoned through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_abandoned.jpg", current_abandoned, device="jpg", width=6, height=5)

####Captured
captured_melt <- melt(equipment_losses[,c("Date", "Russia_Captured", "Ukraine_Captured")], id.var="Date")
captured_melt$Date <- as.Date(captured_melt$Date, format="%m/%d/%Y")
colnames(captured_melt) <- c("Date", "Country", "Captured")
captured_melt$Country <- gsub("_Captured", "", captured_melt$Country)
captured_melt <- captured_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Captured - lag(Captured, default = first(Captured)))

current_captured <- ggplot(captured_melt, aes(Date, Captured, colour=Country, shape=Country)) +
geom_col(data=captured_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Captured by Enemy") +
ggtitle(paste0("Total equipment captured by enemy through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_captured.jpg", current_captured, device="jpg", width=6, height=5)


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
geom_col(data=all_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) + 
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Total Equipment Lost") +
ggtitle(paste0("Total equipment lost through ", Sys.Date())) +
facet_grid(rows=vars(Type)) +
theme_light() +
theme(legend.position="bottom")
ggsave("~/Github/Russia-Ukraine/Plots/current_grid.jpg", current_grid, device="jpg", width=6, height=10)


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

current_tanks <- ggplot(tanks_melt, aes(Date, Tanks, colour=Country, shape=Country)) +
geom_col(data=tanks_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Tanks Lost") +
ggtitle(paste0("Tanks lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_tanks.jpg", current_tanks, device="jpg", width=6, height=5)

####Armored Fighting Vehicles (AFVs)
afv_melt <- melt(equipment_losses[,c("Date", "Russia_AFV", "Ukraine_AFV")], id.var="Date")
afv_melt$Date <- as.Date(afv_melt$Date, format="%m/%d/%Y")
colnames(afv_melt) <- c("Date", "Country", "AFV")
afv_melt$Country <- gsub("_AFV", "", afv_melt$Country)
afv_melt <- afv_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = AFV - lag(AFV, default = first(AFV)))

current_afv <- ggplot(afv_melt, aes(Date, AFV, colour=Country, shape=Country)) +
geom_col(data=afv_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Armored Fighting Vehicles Lost") +
ggtitle(paste0("AFV lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_afv.jpg", current_afv, device="jpg", width=6, height=5)

####Infantry Fighting Vehicles (AFVs)
ifv_melt <- melt(equipment_losses[,c("Date", "Russia_IFV", "Ukraine_IFV")], id.var="Date")
ifv_melt$Date <- as.Date(ifv_melt$Date, format="%m/%d/%Y")
colnames(ifv_melt) <- c("Date", "Country", "IFV")
ifv_melt$Country <- gsub("_IFV", "", ifv_melt$Country)
ifv_melt <- ifv_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = IFV - lag(IFV, default = first(IFV)))

current_ifv <- ggplot(ifv_melt, aes(Date, IFV, colour=Country, shape=Country)) +
geom_col(data=ifv_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Infantry Fighting Vehicles Lost") +
ggtitle(paste0("IFV lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_ifv.jpg", current_ifv, device="jpg", width=6, height=5)

####Armored Personal Carriers
apc_melt <- melt(equipment_losses[,c("Date", "Russia_APC", "Ukraine_APC")], id.var="Date")
apc_melt$Date <- as.Date(apc_melt$Date, format="%m/%d/%Y")
colnames(apc_melt) <- c("Date", "Country", "APC")
apc_melt$Country <- gsub("_APC", "", apc_melt$Country)
apc_melt <- apc_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = APC - lag(APC, default = first(APC)))

current_apc <- ggplot(apc_melt, aes(Date, APC, colour=Country, shape=Country)) +
geom_col(data=apc_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Armored Personal Carriers Lost") +
ggtitle(paste0("APC lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_apc.jpg", current_apc, device="jpg", width=6, height=5)

####Infantry Mobility Vehicles (IMVs)
imv_melt <- melt(equipment_losses[,c("Date", "Russia_IMV", "Ukraine_IFV")], id.var="Date")
imv_melt$Date <- as.Date(imv_melt$Date, format="%m/%d/%Y")
colnames(imv_melt) <- c("Date", "Country", "IMV")
imv_melt$Country <- gsub("_IMV", "", imv_melt$Country)
imv_melt <- imv_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = IMV - lag(IMV, default = first(IMV)))

current_imv <- ggplot(imv_melt, aes(Date, IMV, colour=Country, shape=Country)) +
geom_col(data=imv_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Infantry Mobility Vehicles Lost") +
ggtitle(paste0("IMV lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_imv.jpg", current_imv, device="jpg", width=6, height=5)

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

current_ev <- ggplot(ev_melt, aes(Date, EV, colour=Country, shape=Country)) +
geom_col(data=ev_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Engineering Vehicles Lost") +
ggtitle(paste0("EV lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_ev.jpg", current_ev, device="jpg", width=6, height=5)

####Vehicles
vehicles_melt <- melt(equipment_losses[,c("Date", "Russia_Vehicles", "Ukraine_Vehicles")], id.var="Date")
vehicles_melt$Date <- as.Date(vehicles_melt$Date, format="%m/%d/%Y")
colnames(vehicles_melt) <- c("Date", "Country", "Vehicles")
vehicles_melt$Country <- gsub("_Vehicles", "", vehicles_melt$Country)
vehicles_melt <- vehicles_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Vehicles - lag(Vehicles, default = first(Vehicles)))

current_vehicles <- ggplot(vehicles_melt, aes(Date, Vehicles, colour=Country, shape=Country)) +
geom_col(data=vehicles_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Vehicles Lost") +
ggtitle(paste0("Vehicles lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_vehicles.jpg", current_vehicles, device="jpg", width=6, height=5)


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

current_aircraft <- ggplot(aircraft_melt, aes(Date, Aircraft, colour=Country, shape=Country)) +
geom_col(data=aircraft_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Aircraft Lost") +
ggtitle(paste0("Aircraft lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_aircraft.jpg", current_aircraft, device="jpg", width=6, height=5)

####Anti-Aircraft
antiaircraft_melt <- melt(equipment_losses[,c("Date", "Russia_Antiair", "Ukraine_Antiair")], id.var="Date")
antiaircraft_melt$Date <- as.Date(antiaircraft_melt$Date, format="%m/%d/%Y")
colnames(antiaircraft_melt) <- c("Date", "Country", "Antiair")
antiaircraft_melt$Country <- gsub("_Antiair", "", antiaircraft_melt$Country)
antiaircraft_melt <- antiaircraft_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Antiair - lag(Antiair, default = first(Antiair)))

current_antiair <- ggplot(antiaircraft_melt, aes(Date, Antiair, colour=Country, shape=Country)) +
geom_col(data=antiaircraft_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Anti-Air Systems Lost") +
ggtitle(paste0("Anti-air systems lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_antiair.jpg", current_antiair, device="jpg", width=6, height=5)

####Infantry
infantry_melt <- melt(equipment_losses[,c("Date", "Russia_Infantry", "Ukraine_Infantry")], id.var="Date")
infantry_melt$Date <- as.Date(infantry_melt$Date, format="%m/%d/%Y")
colnames(infantry_melt) <- c("Date", "Country", "Infantry")
infantry_melt$Country <- gsub("_Infantry", "", infantry_melt$Country)
infantry_melt <- infantry_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Infantry - lag(Infantry, default = first(Infantry)))

current_infantry <- ggplot(infantry_melt, aes(Date, Infantry, colour=Country, shape=Country)) +
geom_col(data=infantry_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Infantry Support Lost") +
ggtitle(paste0("Infantry support lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_infantry.jpg", current_infantry, device="jpg", width=6, height=5)

####Armor
armor_melt <- melt(equipment_losses[,c("Date", "Russia_Armor", "Ukraine_Armor")], id.var="Date")
armor_melt$Date <- as.Date(armor_melt$Date, format="%m/%d/%Y")
colnames(armor_melt) <- c("Date", "Country", "Armor")
armor_melt$Country <- gsub("_Armor", "", armor_melt$Country)
armor_melt <- armor_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Armor - lag(Armor, default = first(Armor)))

current_armor <- ggplot(armor_melt, aes(Date, Armor, colour=Country, shape=Country)) +
geom_col(data=armor_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Armor Lost") +
ggtitle(paste0("Armor support lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_armor.jpg", current_armor, device="jpg", width=6, height=5)

####Logistics
logistics_melt <- melt(equipment_losses[,c("Date", "Russia_Logistics", "Ukraine_Logistics")], id.var="Date")
logistics_melt$Date <- as.Date(logistics_melt$Date, format="%m/%d/%Y")
colnames(logistics_melt) <- c("Date", "Country", "Logistics")
logistics_melt$Country <- gsub("_Logistics", "", logistics_melt$Country)
logistics_melt <- logistics_melt %>%
    group_by(Country) %>%
    arrange(Date) %>%
    mutate(Daily = Logistics - lag(Logistics, default = first(Logistics)))

current_logistics <- ggplot(logistics_melt, aes(Date, Logistics, colour=Country, shape=Country)) +
geom_col(data=logistics_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
geom_point() +
stat_smooth(method="gam") +
scale_x_date(date_labels = "%m/%d") +
scale_y_continuous("Logistics Systems Lost") +
ggtitle(paste0("Logistics systems lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_logistics.jpg", current_logistics, device="jpg", width=6, height=5)


###Analysis
empty_columns <- colSums(is.na(equipment_losses) | equipment_losses == "") == nrow(equipment_losses)
equipment_totals <- equipment_losses[,!empty_columns]
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
    MANPAD=equipment_totals[,"Russia_MANPAD"]/equipment_totals[,"Ukraine_MANPAD"],
    SAM=equipment_totals[,"Russia_SAM"]/equipment_totals[,"Ukraine_SAM"],
    SPAAG=equipment_totals[,"Russia_SPAAG"]/equipment_totals[,"Ukraine_SPAAG"],
    Infantry=equipment_totals[,"Russia_Infantry"]/equipment_totals[,"Ukraine_Infantry"],
    Logistics=equipment_totals[,"Russia_Logistics"]/equipment_totals[,"Ukraine_Logistics"],
    Armor=equipment_totals[,"Russia_Armor"]/equipment_totals[,"Ukraine_Armor"],
    Antiair=equipment_totals[,"Russia_Antiair"]/equipment_totals[,"Ukraine_Antiair"]
)
equipment_ratios_t <- data.frame(Type=gsub("Russia_", "", names(equipment_ratios)), Ratio=t(equipment_ratios))


loss_type <- ggplot(equipment_ratios_t[equipment_ratios_t$Type %in% c("Destroyed", "Abandoned", "Captured"),], aes(Type, Ratio, colour=Type, fill=Type)) +
geom_col() +
scale_y_continuous("Ratio (Russian/Ukranian Losses)") +
ggtitle(paste0("Loss ratios lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_loss_type.jpg", loss_type, device="jpg", width=6, height=5)

unit_type <- ggplot(equipment_ratios_t[equipment_ratios_t$Type %in% c("Aircraft", "Antiair", "Infantry", "Armor", "Vehicles", "Logistics"),], aes(Type, Ratio, colour=Type, fill=Type)) +
geom_col() +
scale_y_continuous("Ratio (Russian/Ukranian Losses)") +
ggtitle(paste0("Unit type ratios lost through ", Sys.Date())) +
theme_light()
ggsave("~/Github/Russia-Ukraine/Plots/current_unit_type.jpg", unit_type, device="jpg", width=6, height=5)


###Relative Tank Losses
### Percent Loss estimate per common request
### Total Tanks sourced from https://inews.co.uk/news/world/russia-tanks-how-many-putin-armoured-forces-ukraine-nato-explained-1504470
percent_tanks <- equipment_losses  %>%
  select(Date, Russia = Russia_Tanks, Ukraine = Ukraine_Tanks, Russia_Capture=Russia_Tank_Capture, Ukraine_Capture=Ukraine_Tank_Capture) %>%
  mutate(Date = mdy(Date),
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
    
    
  current_percent_tanks <- ggplot(data=percent_tanks, mapping=aes(Date, Tanks, colour=Country, shape=Country)) +
  geom_col(data=percent_tanks, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point() +
  stat_smooth(method="gam") +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "Tank Losses [% of tanks]") +
  ggtitle(paste0("Total tank losses through ", Sys.Date())) +
  theme_light()

ggsave("~/Github/Russia-Ukraine/Plots/current_percent_tanks.jpg", current_percent_tanks, device="jpg", width=6, height=5)

###Relative AFV Losses
### Percent Loss estimate per common request
### Total Tanks sourced from https://inews.co.uk/news/world/russia-tanks-how-many-putin-armoured-forces-ukraine-nato-explained-1504470
percent_afv <- equipment_losses  %>%
  select(Date, Russia = Russia_AFV, Ukraine = Ukraine_AFV, Russia_Capture = Russia_AFV_Capture, Ukraine_Capture = Ukraine_AFV_Capture) %>%
  mutate(Date = mdy(Date),
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
    
    
  current_percent_afv <- ggplot(data=percent_afv, mapping=aes(Date, AFV, colour=Country, shape=Country)) +
  geom_col(data=percent_afv, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
  geom_point() +
  stat_smooth(method="gam") +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = percent) +
  labs(y = "AFV Losses [% of AFV]") +
  ggtitle(paste0("Total AFV losses through ", Sys.Date())) +
  theme_light()

ggsave("~/Github/Russia-Ukraine/Plots/current_percent_afv.jpg", current_percent_afv, device="jpg", width=6, height=5)
