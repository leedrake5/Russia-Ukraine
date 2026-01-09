library(cronR)
fichero <- "~/GitHub/Russia-Ukraine/R/Automation/automation.r"
cmd <- cron_rscript(fichero)

cron_add(command = cmd, frequency = 'monthly', id = 'lvta', description = 'LTA automation', tags = c('lab', 'xyz'))

