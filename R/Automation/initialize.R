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

list.of.packages <- c("ggplot2", "RCurl", "reshape2", "data.table", "gsheet", "dplyr", "tidyverse", "lubridate", "scales", "rvest", "do")
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
library(dplyr)
library(tidyverse)
library(lubridate)
library(scales)
library(rvest)
library(do)

source("~/GitHub/scrape_oryx/R/functions.R")

source("~/GitHub/Russia-Ukraine/R/Automation/back.r")

googlesheets4::gs4_auth_configure(api_key=api_key)

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

###Map
ggmap::register_google(key = maps_key)


