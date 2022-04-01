library(cronR)
fichero <- "~/GitHub/Russia-Ukraine/R/Automation/automation.r"
cmd <- cron_rscript(fichero)

cron_add(command = cmd, frequency = 'hourly', id = 'prueba', description = 'Descripción del proceso', tags = c('lab', 'xyz'))

