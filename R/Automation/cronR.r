library(cronR)
fichero <- "~/GitHub/Russia-Ukraine/R/Automation/automation.r"
cmd <- cron_rscript(fichero)

cron_add(command = cmd, frequency = 'monthly', id = 'lvta', description = 'LTA automation', tags = c('lab', 'xyz'))


cmd <- cron_rscript(
  rscript = "~/GitHub/Russia-Ukraine/R/Automation/daily_update.r"
)

library(cronR)

# Remove existing job
cron_rm("Russia-Ukraine Daily Update")

# Ensure log directory exists
#dir.create("/Users/lee/GitHub/Russia-Ukraine/R/Automation", recursive = TRUE, showWarnings = FALSE)

# Build command
cmd <- cron_rscript(
  rscript = "/Users/lee/GitHub/Russia-Ukraine/R/Automation/daily_update.r"
)

# Add TZ + log redirection (cron-safe)
cmd_logged <- paste(
  'TZ="America/Los_Angeles"',
  cmd,
  ">> /Users/lee/GitHub/Russia-Ukraine/R/Automation/cron_daily_update.log 2>&1"
)

# Re-add at 19:30
cron_add(
  command = cmd_logged,
  frequency = "daily",
  at = "19:30",
  id = "Russia-Ukraine Daily Update"
)

# Verify
cron_ls()
