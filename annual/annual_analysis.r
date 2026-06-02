## annual_analysis.r
##
## Year-over-year comparison of (a) Oryx-derived equipment losses and
## (b) NASA FIRMS active-fire detections in the eastern theatre.
##
## Inputs:
##   data/byType/YYYY-MM-DD.csv  — cumulative loss snapshots (Oryx)
##   data/FIRMS/YYYY-MM-DD.csv   — daily FIRMS pulls (J1 VIIRS C2, Russia-Asia 24h)
##
## Outputs (all written to annual/):
##   annual_equipment_losses.csv
##   annual_firms_summary.csv
##   plots: tanks_by_year.jpg, total_units_minus_trucks_by_year.jpg,
##          firms_by_year.jpg, firms_pokrovsk_monthly.jpg
##
## Caveats:
##   - 2022 covers Feb 24 onwards; 2026 is partial (through latest snapshot).
##     A per-day rate is reported alongside totals so partial years are comparable.
##   - FIRMS detections are an indirect proxy. Confounders: agricultural burns
##     (esp. spring/autumn), wildfires, industrial sites, weather/cloud cover,
##     satellite revisit cadence. Use the macro window and multi-year trend to
##     dampen short-term noise; do not over-read month-to-month wiggles.
##   - "Total units (minus trucks)" = "All Types" row minus the trucks row,
##     to stay consistent with recent Oryx category changes.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(data.table)
  library(lubridate)
  library(tidyr)
})

base_dir <- path.expand("~/GitHub/Russia-Ukraine")
out_dir  <- file.path(base_dir, "annual")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

country_colors <- c("Russia" = "#E4181C", "Ukraine" = "#0057B8")
ggplot2::theme_set(theme_light(base_size = 11))

today <- Sys.Date()
latest_snapshot <- file.path(base_dir, "data/byType", paste0(today, ".csv"))
if (!file.exists(latest_snapshot)) {
  # Fall back to the most recent file in the directory.
  files <- list.files(file.path(base_dir, "data/byType"), pattern = "\\.csv$", full.names = TRUE)
  dates <- as.Date(gsub("\\.csv$", "", basename(files)))
  latest_snapshot <- files[which.max(dates)]
  today <- max(dates, na.rm = TRUE)
}

## ---- 1. Equipment losses: cumulative snapshots at year boundaries ---------

snapshots <- list(
  "pre-war"   = file.path(base_dir, "data/byType/2022-02-24.csv"),
  "2022-end"  = file.path(base_dir, "data/byType/2022-12-31.csv"),
  "2023-end" = file.path(base_dir, "data/byType/2023-12-31.csv"),
  "2024-end" = file.path(base_dir, "data/byType/2024-12-31.csv"),
  "2025-end" = file.path(base_dir, "data/byType/2025-12-31.csv"),
  "current"  = latest_snapshot
)

read_snap <- function(path, label) {
  d <- data.table::fread(path)
  setnames(d, old = names(d)[1], new = "V1")  # leading row-id column
  d[, snapshot := label]
  d
}

snap_list <- mapply(read_snap, snapshots, names(snapshots), SIMPLIFY = FALSE)
snap <- rbindlist(snap_list, use.names = TRUE, fill = TRUE)

# Trucks row renamed across years; match either form.
trucks_pattern <- "^Trucks"
all_types_pattern <- "^All Types$"

extract_metric <- function(d, country_, pattern) {
  row <- d[country == country_ & grepl(pattern, equipment_type), ]
  if (nrow(row) == 0) return(NA_integer_)
  as.integer(row$type_total[1])
}

countries <- c("Russia", "Ukraine")

# Build the cumulative table.
cum <- snap[, .(snapshot, country, equipment_type, type_total)]
cum_summary <- rbindlist(lapply(names(snapshots), function(lbl) {
  d <- snap[snapshot == lbl]
  rbindlist(lapply(countries, function(co) {
    tanks    <- extract_metric(d, co, "^Tanks$")
    all_t    <- extract_metric(d, co, all_types_pattern)
    trucks   <- extract_metric(d, co, trucks_pattern)
    data.table(
      snapshot = lbl,
      country  = co,
      tanks_cum        = tanks,
      all_units_cum    = all_t,
      trucks_cum       = trucks,
      units_no_trucks_cum = all_t - trucks
    )
  }))
}))

# Annual deltas: each calendar year is (year-end) - (prior year-end).
# 2022 is treated as (2022-end) - (pre-war) since the war began Feb 24, 2022.
deltas <- cum_summary %>%
  arrange(country, match(snapshot, names(snapshots))) %>%
  group_by(country) %>%
  mutate(
    year = case_when(
      snapshot == "pre-war"  ~ NA_character_,
      snapshot == "2022-end" ~ "2022",
      snapshot == "2023-end" ~ "2023",
      snapshot == "2024-end" ~ "2024",
      snapshot == "2025-end" ~ "2025",
      snapshot == "current"  ~ format(today, "%Y")
    ),
    tanks_year     = tanks_cum         - lag(tanks_cum),
    all_units_year = all_units_cum     - lag(all_units_cum),
    trucks_year    = trucks_cum        - lag(trucks_cum),
    units_no_trucks_year = units_no_trucks_cum - lag(units_no_trucks_cum)
  ) %>%
  ungroup() %>%
  filter(!is.na(year))

# Days in each window — pre-war anchor is Feb 24, 2022.
year_starts <- c("2022" = as.Date("2022-02-24"),
                 "2023" = as.Date("2023-01-01"),
                 "2024" = as.Date("2024-01-01"),
                 "2025" = as.Date("2025-01-01"),
                 "2026" = as.Date("2026-01-01"))
year_ends   <- c("2022" = as.Date("2022-12-31"),
                 "2023" = as.Date("2023-12-31"),
                 "2024" = as.Date("2024-12-31"),
                 "2025" = as.Date("2025-12-31"),
                 "2026" = today)

deltas <- deltas %>%
  mutate(
    period_start = year_starts[year],
    period_end   = year_ends[year],
    days         = as.integer(period_end - period_start) + 1,
    tanks_per_day        = tanks_year / days,
    units_no_trucks_per_day = units_no_trucks_year / days,
    partial = year == format(today, "%Y")
  )

write.csv(deltas, file.path(out_dir, "annual_equipment_losses.csv"), row.names = FALSE)

## ---- 2. Equipment loss plots ---------------------------------------------

bar_year <- function(df, ycol, ylabel, title, subtitle, file) {
  ggplot(df, aes(year, .data[[ycol]], fill = country)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(aes(label = comma(round(.data[[ycol]]))),
              position = position_dodge(width = 0.8), vjust = -0.4, size = 3) +
    scale_fill_manual(values = country_colors) +
    scale_y_continuous(ylabel, labels = comma, expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, title = title, subtitle = subtitle, fill = NULL) +
    theme(legend.position = "top")
  ggsave(file.path(out_dir, file), width = 7, height = 5, dpi = 300)
}

partial_note <- paste0(
  format(today, "%Y"), " is partial through ", format(today, "%b %d"),
  " (", deltas$days[deltas$year == format(today, "%Y")][1], " days)."
)

bar_year(
  deltas, "tanks_year",
  "Tanks lost (visually confirmed)",
  "Tanks lost per year — Russia vs Ukraine",
  paste0("Source: Oryx (data/byType cumulative snapshots). ", partial_note),
  "tanks_by_year.jpg"
)

bar_year(
  deltas, "units_no_trucks_year",
  "Units lost, excluding trucks",
  "Total equipment lost per year (excluding trucks)",
  paste0("'All Types' minus 'Trucks/similar'. Oryx visually confirmed. ", partial_note),
  "total_units_minus_trucks_by_year.jpg"
)

# Per-day-rate variant so the partial year is honestly comparable.
deltas_long <- deltas %>%
  select(country, year, tanks_per_day, units_no_trucks_per_day) %>%
  pivot_longer(c(tanks_per_day, units_no_trucks_per_day),
               names_to = "metric", values_to = "rate") %>%
  mutate(metric = recode(metric,
                         tanks_per_day = "Tanks / day",
                         units_no_trucks_per_day = "Units (no trucks) / day"))

ggplot(deltas_long, aes(year, rate, fill = country)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = country_colors) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Losses per day",
       title = "Loss rate per day — normalised for partial years",
       subtitle = paste0("Oryx visually confirmed losses / days in window. ", partial_note),
       fill = NULL) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "loss_rate_per_day.jpg"),
       width = 7, height = 7, dpi = 300)

## ---- 3. FIRMS: load every daily pull --------------------------------------

firms_dir <- file.path(base_dir, "data/FIRMS")
firms_files <- list.files(firms_dir, pattern = "\\.csv$", full.names = TRUE)

# Schema drifts across years (brightness vs bright_ti4/5). Keep the union.
keep_cols <- c("latitude", "longitude", "acq_date", "frp")

read_firms <- function(f) {
  d <- tryCatch(fread(f, fill = TRUE), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  miss <- setdiff(keep_cols, names(d))
  for (m in miss) d[[m]] <- NA
  d <- d[, ..keep_cols]
  # Schema drift across years — coerce to a stable set of types.
  d[, latitude  := suppressWarnings(as.numeric(latitude))]
  d[, longitude := suppressWarnings(as.numeric(longitude))]
  d[, frp       := suppressWarnings(as.numeric(frp))]
  d[, acq_date  := suppressWarnings(as.character(acq_date))]
  d
}

firms_all <- rbindlist(lapply(firms_files, read_firms), use.names = TRUE, fill = TRUE)
firms_all[, acq_date := as.Date(acq_date)]
firms_all <- firms_all[!is.na(acq_date) & !is.na(latitude) & !is.na(longitude) & !is.na(frp)]
firms_all <- firms_all[acq_date >= as.Date("2022-02-24") & acq_date <= today]
# Some daily pulls overlap (24h files can repeat detections). De-dup on the
# rounded coordinate + date + frp signature to avoid double-counting.
firms_all[, dedup_key := paste(round(latitude, 4), round(longitude, 4), acq_date, round(frp, 1))]
firms_all <- unique(firms_all, by = "dedup_key")
firms_all[, dedup_key := NULL]
firms_all[, year := lubridate::year(acq_date)]

## ---- 4. Three nested windows over the eastern theatre --------------------
##
## Pokrovsk axis (tight):  lat 48.0–48.7, lon 36.6–37.9
##   — Pokrovsk, Avdiivka ruins, Kurakhove approach, Toretsk.
## Donbas operational (medium):  lat 47.3–49.6, lon 36.3–38.7
##   — Kupyansk southern half down through Pokrovsk to Velyka Novosilka,
##     plus Bakhmut/Chasiv Yar.
## Eastern theatre (wide):  lat 46.5–50.5, lon 35.0–39.5
##   — full Kharkiv-Luhansk-Donetsk-northern Zaporizhzhia operational area.

windows <- list(
  pokrovsk_axis = list(lat = c(48.0, 48.7), lon = c(36.6, 37.9)),
  donbas_oper   = list(lat = c(47.3, 49.6), lon = c(36.3, 38.7)),
  east_theatre  = list(lat = c(46.5, 50.5), lon = c(35.0, 39.5)),
  ukraine_all   = list(lat = c(44.3, 52.4), lon = c(22.1, 40.2))
)

window_label <- c(
  pokrovsk_axis = "Pokrovsk axis (tight)",
  donbas_oper   = "Donbas operational (medium)",
  east_theatre  = "Eastern theatre (wide)",
  ukraine_all   = "Ukraine (whole country)"
)

subset_window <- function(df, w) {
  df[latitude  >= w$lat[1] & latitude  <= w$lat[2] &
     longitude >= w$lon[1] & longitude <= w$lon[2]]
}

firms_year <- rbindlist(lapply(names(windows), function(nm) {
  w <- windows[[nm]]
  d <- subset_window(firms_all, w)
  d[, .(detections = .N, total_frp = sum(frp, na.rm = TRUE)),
    by = year][, window := nm]
}), use.names = TRUE)

# Per-day rates so the partial year is comparable.
year_days <- data.table(
  year = c(2022, 2023, 2024, 2025, 2026),
  days = as.integer(c(
    as.Date("2022-12-31") - as.Date("2022-02-24") + 1,
    365, 366, 365,
    as.integer(today - as.Date("2026-01-01")) + 1
  ))
)
firms_year <- merge(firms_year, year_days, by = "year", all.x = TRUE)
firms_year[, `:=`(
  detections_per_day = detections / days,
  frp_per_day = total_frp / days,
  window_label = window_label[window],
  partial = year == lubridate::year(today)
)]
setorder(firms_year, window, year)

write.csv(firms_year, file.path(out_dir, "annual_firms_summary.csv"), row.names = FALSE)

## ---- 5. FIRMS annual plots -----------------------------------------------

partial_firms_note <- paste0(
  lubridate::year(today), " is partial through ", format(today, "%b %d"),
  " — bars include a per-day rate so the year-to-date is comparable."
)

ggplot(firms_year, aes(factor(year), detections_per_day, fill = window_label)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.78) +
  geom_text(aes(label = comma(round(detections_per_day, 1))),
            position = position_dodge(width = 0.85),
            vjust = -0.4, size = 2.8) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous("FIRMS detections per day", labels = comma,
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, fill = NULL,
       title = "FIRMS detections per day, by year and window",
       subtitle = paste0("VIIRS active-fire hits in eastern Ukraine. ", partial_firms_note,
                         "\nProxy for combat tempo; confounded by agricultural burns + wildfires.")) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "firms_detections_per_day.jpg"),
       width = 8, height = 5.5, dpi = 300)

ggplot(firms_year, aes(factor(year), frp_per_day, fill = window_label)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.78) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous("Total FRP per day (MW)", labels = comma,
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, fill = NULL,
       title = "FIRMS Fire Radiative Power per day, by year and window",
       subtitle = paste0("Sum of frp / days in window. ", partial_firms_note)) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "firms_frp_per_day.jpg"),
       width = 8, height = 5.5, dpi = 300)

## ---- 6. Pokrovsk monthly trend (last 24 months) --------------------------
##
## Macro annual numbers smooth too much for the user's question about whether
## the Pokrovsk front has heated up. Add a monthly view of the operational
## (medium) window so the recent trajectory is visible.

donbas <- subset_window(firms_all, windows$donbas_oper)
donbas[, month := floor_date(acq_date, "month")]
monthly <- donbas[, .(detections = .N, total_frp = sum(frp, na.rm = TRUE)),
                  by = month][order(month)]

ggplot(monthly, aes(month, detections)) +
  geom_col(fill = "#7F1D1D", alpha = 0.85) +
  geom_smooth(se = FALSE, colour = "black", method = "loess",
              span = 0.4, linewidth = 0.6) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "FIRMS detections per month",
       title = "Donbas operational window — monthly FIRMS detections",
       subtitle = "lat 47.3–49.6, lon 36.3–38.7. Covers Kupyansk → Pokrovsk → Velyka Novosilka.")
ggsave(file.path(out_dir, "firms_donbas_monthly.jpg"),
       width = 8, height = 5, dpi = 300)

## ---- 7. Year-overlay views ------------------------------------------------
##
## Same metric, one curve per calendar year, with an opacity + colour gradient
## so the most recent year reads darkest.
##
## Equipment losses are aggregated by **month** because Oryx posts updates
## irregularly (2–3× per week, occasionally every 3 weeks), which makes weekly
## bins noisy. FIRMS is on a continuous satellite cadence, so it stays weekly.

# FIRMS week bucket. Days 365–366 fold into week 52 to keep 52 buckets/year.
to_week <- function(d) pmin(ceiling(lubridate::yday(d) / 7), 52)
month_first <- as.Date(paste0("2025-", sprintf("%02d", 1:12), "-01"))
month_ticks_weekly <- to_week(month_first)
month_labs  <- month.abb

year_alpha <- c("2022" = 0.45, "2023" = 0.55, "2024" = 0.70,
                "2025" = 0.85, "2026" = 1.00)

# Sequential palette — light/warm → dark/hot so recent years pop. Used on top
# of alpha as a redundant channel so faded years stay readable.
year_colour <- c("2022" = "#FED98E",
                 "2023" = "#FE9929",
                 "2024" = "#EC7014",
                 "2025" = "#993404",
                 "2026" = "#1A0000")

## --- 7a. Monthly equipment-loss helper ------------------------------------

byType_files <- list.files(file.path(base_dir, "data/byType"),
                           pattern = "\\.csv$", full.names = TRUE)

# Build a daily cumulative series for the given types, then diff and bucket
# into months. Drops negative daily deltas (Oryx retroactive revisions).
compute_monthly_losses <- function(files, types, label) {
  daily <- rbindlist(lapply(files, function(f) {
    d <- tryCatch(fread(f), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    rows <- d[equipment_type %in% types,
              .(cum = sum(type_total, na.rm = TRUE)), by = country]
    if (nrow(rows) == 0) return(NULL)
    rows[, src_date := as.Date(gsub("\\.csv$", "", basename(f)))]
    rows
  }), use.names = TRUE, fill = TRUE)
  daily <- unique(daily, by = c("country", "src_date"))
  setorder(daily, country, src_date)
  daily[, lost := cum - shift(cum), by = country]
  daily <- daily[!is.na(lost) & lost >= 0]
  daily[, `:=`(year = lubridate::year(src_date),
               month = lubridate::month(src_date))]
  monthly <- daily[, .(losses = sum(lost)), by = .(country, year, month)]
  monthly[, year_fct := factor(year)]
  monthly <- monthly[as.character(year) %in% names(year_alpha)]
  monthly[, category := label]
  monthly
}

plot_monthly <- function(d, ylab, title, subtitle, file) {
  ggplot(d, aes(month, losses, colour = year_fct,
                alpha = year_fct, group = year_fct)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.6) +
    facet_wrap(~ country, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = year_colour, name = "Year") +
    scale_alpha_manual(values = year_alpha, name = "Year") +
    scale_x_continuous(breaks = 1:12, labels = month_labs,
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(ylab, labels = comma) +
    coord_cartesian(ylim = c(0, NA)) +
    guides(colour = guide_legend(override.aes = list(linewidth = 2.5)),
           alpha = "none") +
    labs(x = NULL, title = title, subtitle = subtitle) +
    theme(legend.position = "top")
  ggsave(file.path(out_dir, file), width = 8, height = 7, dpi = 300)
}

monthly_tanks <- compute_monthly_losses(byType_files, "Tanks", "Tanks")

plot_monthly(
  monthly_tanks,
  "Tanks lost per month",
  "Monthly tank losses — year-on-year overlay",
  paste0("x = month of year. Recent years darker/hotter. Oryx visually confirmed. ",
         "Monthly bins absorb Oryx's irregular posting cadence."),
  "monthly_tanks_yearcompare.jpg"
)

## --- 7b. Weekly FIRMS — eastern half of Ukraine (eastern-theatre window) --

east_w <- windows$east_theatre
firms_east <- subset_window(firms_all, east_w)
firms_east[, year := lubridate::year(acq_date)]
firms_east[, week := to_week(acq_date)]

weekly_firms_east <- firms_east[
  , .(detections = .N, frp = sum(frp, na.rm = TRUE)),
  by = .(year, week)
]
weekly_firms_east[, year_fct := factor(year)]
weekly_firms_east <- weekly_firms_east[as.character(year) %in% names(year_alpha)]

ggplot(weekly_firms_east, aes(week, detections, colour = year_fct,
                              alpha = year_fct, group = year_fct)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = year_colour, name = "Year") +
  scale_alpha_manual(values = year_alpha, name = "Year") +
  scale_x_continuous(breaks = month_ticks_weekly, labels = month_labs,
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous("FIRMS detections per week", labels = comma) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2.5)),
         alpha = "none") +
  labs(x = NULL,
       title = "Weekly FIRMS detections — eastern half of Ukraine",
       subtitle = paste0(
         "Eastern theatre window (lat 46.5–50.5, lon 35.0–39.5). ",
         "Recent years darker/hotter. Spring/autumn ag-burn spikes are real seasonal noise.")) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "weekly_firms_east_yearcompare.jpg"),
       width = 9, height = 5.5, dpi = 300)

# Also: a smoothed version for the macro trend without the ag-burn spike noise.
ggplot(weekly_firms_east, aes(week, detections, colour = year_fct,
                              alpha = year_fct, group = year_fct)) +
  geom_smooth(se = FALSE, method = "loess",
              span = 0.35, linewidth = 1.3) +
  scale_colour_manual(values = year_colour, name = "Year") +
  scale_alpha_manual(values = year_alpha, name = "Year") +
  scale_x_continuous(breaks = month_ticks_weekly, labels = month_labs,
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous("FIRMS detections per week (loess smooth)", labels = comma) +
  coord_cartesian(ylim = c(0, NA)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2.5)),
         alpha = "none") +
  labs(x = NULL,
       title = "Weekly FIRMS detections (smoothed) — eastern half of Ukraine",
       subtitle = "Loess span 0.35 to dampen ag-burn spikes; shape of the seasonal arc is the signal.") +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "weekly_firms_east_yearcompare_smoothed.jpg"),
       width = 9, height = 5.5, dpi = 300)

## --- 7b. Monthly artillery + infantry-carrier losses ----------------------

artillery_types <- c("Towed Artillery", "Self-Propelled Artillery",
                     "Multiple Rocket Launchers", "Rocket and Missile Artillery")
infantry_carrier_types <- c("Infantry Fighting Vehicles",
                            "Armoured Personnel Carriers",
                            "Infantry Mobility Vehicles")

monthly_artillery <- compute_monthly_losses(byType_files, artillery_types, "Artillery")
monthly_infantry  <- compute_monthly_losses(byType_files, infantry_carrier_types,
                                            "Infantry carriers (IFV+APC+IMV)")

plot_monthly(
  monthly_artillery,
  "Artillery pieces lost per month",
  "Monthly artillery losses — year-on-year overlay",
  "Towed + self-propelled + rocket/missile artillery. Monthly bins absorb Oryx posting cadence.",
  "monthly_artillery_yearcompare.jpg"
)

plot_monthly(
  monthly_infantry,
  "Infantry carriers lost per month",
  "Monthly infantry-carrier losses — year-on-year overlay",
  "IFV + APC + IMV (excludes MRAP). Monthly bins absorb Oryx posting cadence.",
  "monthly_infantry_carriers_yearcompare.jpg"
)

write.csv(monthly_tanks, file.path(out_dir, "monthly_tanks.csv"), row.names = FALSE)
write.csv(monthly_artillery, file.path(out_dir, "monthly_artillery.csv"), row.names = FALSE)
write.csv(monthly_infantry, file.path(out_dir, "monthly_infantry_carriers.csv"), row.names = FALSE)
write.csv(weekly_firms_east, file.path(out_dir, "weekly_firms_east.csv"), row.names = FALSE)

## ---- 8. Console summary --------------------------------------------------

cat("\n=== Annual equipment losses ===\n")
print(deltas %>%
        select(country, year, tanks_year, units_no_trucks_year,
               tanks_per_day, units_no_trucks_per_day, days, partial) %>%
        arrange(year, country))

cat("\n=== Annual FIRMS by window ===\n")
print(firms_year[, .(window_label, year, detections, total_frp,
                     detections_per_day, frp_per_day, days, partial)])

cat("\nWrote outputs to: ", out_dir, "\n", sep = "")
