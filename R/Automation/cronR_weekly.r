library(cronR)
fichero_weekly <- "~/GitHub/Russia-Ukraine/R/Automation/automation_weekly.r"
cmd <- cron_rscript(fichero_weekly)

cron_add(command = cmd, frequency = 'daily', id = 'prueba_weekly', description = 'Descripción del proceso', tags = c('lab', 'xyz'), days_of_week = 6, at = '04:00')

