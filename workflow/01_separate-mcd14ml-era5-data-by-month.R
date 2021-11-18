# split data into monthly .csv files and get them analysis ready

library(data.table)
library(lubridate)
library(pbapply)

dir.create(file.path("data", "out", "mcd14ml_c006_v03_era5_analysis-ready"), showWarnings = FALSE)
# Steps to make these data analysis ready:
# 1) remove .geo column
# 2) calculate wind speed and direction from u and v vector components of wind
# 3) break into individual months
# 4) determine end day of month (considering leap years) for file naming

files <- list.files("data/out/ee", full.names = TRUE, pattern = ".csv")
# drop the .csv where I kept track of processing time
files <- files[files != "data/out/ee/ee-processing-time.csv"]

# Calculating wind speed/direction from u- and v- components
# https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398
# https://www.eol.ucar.edu/content/wind-direction-quick-reference (R uses the programming language
# convention for the atan2() function because atan2(1,-1) = 2.36)
wind_speed <- function(u, v) {
  return(sqrt(u^2 + v^2))
}

wind_direction <- function(u, v) {
  (pi + atan2(y = u, x = v)) %% (2 * pi)
}

pblapply(1:length(files), FUN = function(i) {
  this_year <- substr(files[i], start = 35, stop = 38)
  f <- data.table::fread(files[i], colClasses = c("system:index" = "character", "acq_time" = "character"))
  
  f[, .geo := NULL]
  f[, month := data.table::month(acq_date)]
  f[, wind_speed := wind_speed(u = u_component_of_wind_10m, v = v_component_of_wind_10m)]
  f[, wind_dir_r := wind_direction(u = u_component_of_wind_10m, v = v_component_of_wind_10m)]
  
  l <- split(x = f, f = f$month)
  
  names(l) <- sprintf("%02d", as.numeric(names(l)))
  
  for(j in 1:length(l)) {
    start_day <- paste(this_year, names(l)[j], "01", sep = "-")
    end_day <- lubridate::days_in_month(start_day)
    end_day_char <- paste(this_year, names(l)[j], end_day, sep = "-")
    
    new_basename <- paste0(substr(x = basename(files[i]), start = 1, stop = 22), 
                           "analysis-ready_",
                           start_day, "_", 
                           end_day_char, 
                           ".csv")
    
    new_filename <- file.path("data", "out", "mcd14ml_c006_v03_era5_analysis-ready", new_basename)
    
    l[[j]][, month:= NULL]
    
    data.table::fwrite(x = l[[j]], file = new_filename)
    
    print(paste0(new_basename, " successfully written..."))
  }
  
})


# ar_files <- list.files("data/out/mcd14ml_c006_v03_era5_analysis-ready/", full.names = TRUE)
# n_records <- sapply(ar_files, FUN = function(x) nrow(data.table::fread(x)))
# sum(n_records)
# 89937808 (89.9 million)
