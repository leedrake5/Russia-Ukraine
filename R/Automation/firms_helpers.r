## FIRMS helpers shared by firms.r, firms_october_01_2023.r and annual/annual_analysis.r.

# Consecutive daily files overlap (each pull spans ~27h, and a day's file keeps every pull made that day),
# so the same detection can sit in two or three files. Count it once.
firms_dedupe <- function(df) {
    df[!duplicated(paste(round(df$latitude, 4), round(df$longitude, 4), df$acq_date, df$acq_time)),]
}

# Time span a daily file covers, from its first and last detection, in UTC minutes since 1970.
# Files under 1000 rows are truncated downloads (normal files hold 4000+) and count as no coverage.
firms_file_window <- function(df) {
    if(is.null(df) || nrow(df) < 1000) return(NULL)
    m <- as.numeric(as.Date(df$acq_date)) * 1440 + (df$acq_time %/% 100) * 60 + df$acq_time %% 100
    data.frame(start=min(m, na.rm=TRUE), end=max(m, na.rm=TRUE))
}

# Dates whose NOAA-20 passes over Ukraine (~21:30-01:30 and ~09:30-13:30 UTC) aren't covered by any
# saved file -- empty, missing or truncated files, or a day whose last pull came before the night pass.
# FRP on these dates is an undercount, so callers show a gap instead of a false low. Uncovered stretches
# shorter than one orbit (~100 min) are just fire-free edges of a file's window and count as covered.
firms_incomplete_dates <- function(windows, dates, min_coverage=0.9) {
    bin <- 10
    t0 <- (as.numeric(min(dates)) - 1) * 1440
    nb <- ((as.numeric(max(dates)) + 2) * 1440 - t0) %/% bin
    covered <- rep(FALSE, nb)
    for(k in seq_len(nrow(windows))) {
        s <- max((windows$start[k] - t0) %/% bin + 1, 1)
        e <- min((windows$end[k] - t0) %/% bin + 1, nb)
        if(s <= e) covered[s:e] <- TRUE
    }
    runs <- rle(covered)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1
    for(k in which(!runs$values & runs$lengths * bin <= 100 & starts > 1 & ends < nb)) covered[starts[k]:ends[k]] <- TRUE
    pass_bins <- c(0:8, 57:80, 129:143)  # 10-min bins of the UTC day: 00:00-01:30, 09:30-13:30, 21:30-24:00
    day_start <- (as.numeric(dates) * 1440 - t0) %/% bin
    coverage <- sapply(day_start, function(d) mean(covered[d + pass_bins + 1]))
    dates[coverage < min_coverage]
}
