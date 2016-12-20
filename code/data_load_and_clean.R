## This code download, load up, and clean a number of different global warming relevant
## data sets.  These cleaned, prepped sets will be saved as csv files in sub directories
## off of the main data directory.  This script should be run from the directory it 
## is sitting in as all directory traversing is relative to the code directory.
## The user can either choose to download and re-clean all of the data sets, or to just
## load the csv files that have already been created by tweaking the download variable
## below

## Zach Wilson - 20161205

## load libraries
library(tidyverse)
library(RCurl)
library(stringr)
library(tidyr)

## download data (1) or processed locally saved files (0)
download = 1
# download = 0

## download se ice area data
# set working directory to data/ice_area directory
setwd("../data/ice_area")

# loop to change directory and filenames and grab the data files
if (download == 1) {
  # define elements needed to build directory and filenames
  month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_num <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  url <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135"

  for (i in 1:12) {
    filenames <- c(paste("N_", month_num[i], "_area_v2.txt", sep = ""),
                   paste("S_", month_num[i], "_area_v2.txt", sep = ""))
    for (fn in filenames) {
      full_url <- paste(url, month[i], fn, sep = "/")
      download.file(full_url, fn, method = "libcurl");
    }
  }

  # load all files for the arctic and consolidate into a single data set
  ice_area <- c()
  for (i in 1:12) {
    file_name <- paste("N_", month_num[i], "_area_v2.txt", sep = "")
    month_data <- read.table(file_name, skip = 1, nrows = 38)
    ice_area <- rbind(ice_area, month_data)
  }
  
  # rename the data columns and make the data frame a tibble
  names(ice_area) <- c("year", "month", "data_type", "region", "extent", "area")
  ice_area <- as_tibble(ice_area)
  
  # arrange the data by year and month
  ice_area <- arrange(ice_area, year, month)

  # replace all -9999 columns in extent and area with NA
  ice_area$extent[ice_area$extent == -9999] = NA
  ice_area$area[ice_area$area == -9999] = NA

  # create a fractional year column
  ice_area <- mutate(ice_area, fractional_year = year + (month-1)/12)

  # save ice area tibble
  write_csv(ice_area, "ice_area.csv")
} else {
  # load the ice area data
  ice_area <- read_csv("ice_area.csv")
}


## load and clean up carbon dioxide data from 1958 to present
# set working directory to data/c02 directory
setwd("../co2")

if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "co2_mm_mlo.txt"
  url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file skipping comments at the begining
  co2_recent <- read.table(file_name, skip = 71)
  
  # rename the data columns
  names(co2_recent) <- c("year", "month", "decimal_date", "average", "interpolated",
                         "season_corr", "num_days")
  
  # transform the data frame to a tibble
  co2_recent <- as_tibble(co2_recent)
  
  # remove the -99.99s and replace them with NAs
  co2_recent$average[co2_recent$average == -99.99] <- NA
  
  # save recent co2 data
  write_csv(co2_recent, "co2_recent.csv")
} else {
  # load the CO2 data from 1958 to 2016
  co2_recent <- read_csv("co2_recent.csv")
}

## load and clean up really old co2 data
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "edc-co2-2008.txt"
  url <- "https://www1.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file skipping comments at the begining
  co2_old <- read.table(file_name, skip = 774)
  
  # rename the data columns
  names(co2_old) <- c("age", "co2_ppm")
  
  # make the data frame a tibble
  co2_old <- as_tibble(co2_old)
  
  # save old co2 data
  write_csv(co2_old, "co2_old.csv")
} else {
  # load the ice core CO2 data set
  co2_old <- read_csv("co2_old.csv")
}

## download arctic surface temperature data
# set working directory to data/surface_temp directory
setwd("../surface_temp")

# loop to change directory and filenames and grab the data files
if (download == 1) {
  # define elements needed to build directory and filenames
  url <- "ftp://sidads.colorado.edu/pub/DATASETS/surface_obs/data/arctic/043900/"
  
  # get a directory listing of this location
  download.file(url, "dir_list.txt", method = "libcurl")
  
  # load directory listing of ftp directory and create list of filenames to grab
  rm(filenames, file_name)
  dl <- read_table("dir_list.txt", skip = 2)
  names(dl) <- c("perm", "num_links", "owner", "group", "size", "month", "day", "year", "file_name")
  filenames <- dl$file_name

  # loop through each of the files on the list and download the data file
  for (file_name in filenames) {
    rm(full_url)
    full_url <- paste(url, file_name, sep = "")
    download.file(full_url, file_name, method = "libcurl");
  }

  # load all files for the arctic and consolidate into a single data set
  arctic_surface_temp <- c()
  for (file_name in filenames) {
    # read in the current data file
    year_data <- read_table(file_name, skip = 4, col_names = c("year", "month", "day", "hr", "min",
                                                               "dir", "dqc", "wtype", "wsp", "sqc",
                                                               "cig", "hqc", "visby", "dqc2", "vv",
                                                               "vqc", "dbt", "bqc", "dpt", "pqc",
                                                               "slp", "sqc2"))
    
    # append the current file onto the end of the arctic temperature data set
    arctic_surface_temp <- rbind(arctic_surface_temp, year_data)
  }
  
  # arrange the data in time order
  arctic_surface_temp <- arrange(arctic_surface_temp, year, month, day, hr, min)
  
  # add a decimal date column, use aaverage number of days in a month
  arctic_surface_temp <- mutate(arctic_surface_temp, decimal_date = year + ((month-1)/12) + (day/365) + 
                               (hr/8760) + (min/525600))
  
  # scale the dry bulb temperature data by 10
  arctic_surface_temp$dbt <- arctic_surface_temp$dbt / 10
  
  # save recent arctic surface temp data
  write_csv(arctic_surface_temp, "arctic_surface_temp.csv")
} else {
  # load the recent arctic surface temperature data
  arctic_surface_temp <- read_csv("arctic_surface_temp.csv")
}

## download antarctic surface temperature data
# loop to change directory and filenames and grab the data files
if (download == 1) {
  # define elements needed to build directory and filenames
  url <- "ftp://sidads.colorado.edu/pub/DATASETS/surface_obs/data/antarctic/896110/"
  
  # get a directory listing of this location
  download.file(url, "dir_list.txt", method = "libcurl")
  
  # load directory listing of ftp directory and create list of filenames to grab
  rm(filenames, file_name)
  dl <- read_table("dir_list.txt", skip = 2)
  names(dl) <- c("perm", "num_links", "owner", "group", "size", "month", "day", "year", "file_name")
  filenames <- dl$file_name

  # loop to download each data file on the filenames list
  for (file_name in filenames) {
    rm(full_url)
    full_url <- paste(url, file_name, sep = "")
    download.file(full_url, file_name, method = "libcurl");
  }

  # load all files for the antarctic and consolidate into a single data set
  antarctic_surface_temp <- c()
  for (file_name in filenames) {
    # load the current data file
    year_data <- read_table(file_name, skip = 4, col_names = c("year", "month", "day", "hr", "min",
                                                               "dir", "dqc", "wtype", "wsp", "sqc",
                                                               "cig", "hqc", "visby", "dqc2", "vv",
                                                               "vqc", "dbt", "bqc", "dpt", "pqc",
                                                               "slp", "sqc2"))
    
    # append the data from the current file onto the end of the antarctic temperature data frame
    antarctic_surface_temp <- rbind(antarctic_surface_temp, year_data)
  }
  # arrange the data in time order
  antarctic_surface_temp <- arrange(antarctic_surface_temp, year, month, day, hr, min)
  
  # add a decimal date colum to antarctic_surface_temp
  antarctic_surface_temp <- mutate(antarctic_surface_temp, decimal_date = year + ((month-1)/12) + (day/365) + 
                               (hr/8760) + (min/525600))
  
  # get rid of the 9999s and some oddly high temperature data
  antarctic_surface_temp$dbt[antarctic_surface_temp$dbt >= 750] = NA
  
  # scale the dry bulb temperature data by 10
  antarctic_surface_temp$dbt <- antarctic_surface_temp$dbt / 10
  
  # save antarctic temperature data
  write_csv(antarctic_surface_temp, "antarctic_surface_temp.csv")
} else {
  # load antarctic surface temperature data
  antarctic_surface_temp <- read_csv("antarctic_surface_temp.csv")
}

## load up the hadley centre monthly max temperature data set
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "cetmaxmly1878on_urbadj4.dat"
  url <- "http://www.metoffice.gov.uk/hadobs/hadcet/cetmaxmly1878on_urbadj4.dat"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  hadley_temp <- read.table(file_name)
  
  # rename data columns
  names(hadley_temp) <- c("year", "jan", "feb", "mar", "apr", "may", "jun", 
                          "jul", "aug", "sep", "oct", "nov", "dec", "year_avg")
  
  # create a tibble
  hadley_temp <- as_tibble(hadley_temp)
  
  # replace -99.9 and -99.99 with NAs
  hadley_temp$dec[hadley_temp$dec == -99.9] = NA
  hadley_temp$year_avg[hadley_temp$year_avg == -99.99] = NA
  
  # perform a gather on all the month columns and the year_avg column
  hadley_temp <- gather(hadley_temp, "month", "temp", jan:year_avg)
  
  # define variables needed to change month abbreviation to integer
  month_abb <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep",
                 "oct", "nov", "dec", "year_avg")
  months <- 1:13
  names(months) <- month_abb
  hadley_temp$month <- match(hadley_temp$month, month_abb)
  
  # add a decimal year column
  hadley_temp <- mutate(hadley_temp, dec_year = year + ((month - 1) / 12)) 
  
  # find all yearly average values (month = 13) and fix there new date
  hadley_temp$dec_year[hadley_temp$month == 13] <- hadley_temp$dec_year[hadley_temp$month == 13] - 1
  
  # arrange by decimal year to order by time
  hadley_temp <- arrange(hadley_temp, dec_year)
  
  # save hadley england temperature data
  write_csv(hadley_temp, "hadley_temp.csv")
} else {
  # load hadley england temperature data
  hadley_temp <- read_csv("hadley_temp.csv")
}

## load up the average temperature of the contiguous 48 states in august
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "1895-2016.csv"
  url <- "https://www.ncdc.noaa.gov/cag/time-series/us/110/00/tavg/1/08/1895-2016.csv?base_prd=true&begbaseyear=1901&endbaseyear=2016"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  contig_48_temp <- read_csv(file_name, skip = 4)
  
  # fix column names
  names(contig_48_temp) <- c("year", "temp", "anomaly")
  
  # break up the year_month column and toss the month
  contig_48_temp$year <- as.character(contig_48_temp$year)
  contig_48_temp$year <- str_replace(contig_48_temp$year, "[0][8]$", "")
  contig_48_temp$year <- as.integer(contig_48_temp$year)
  
  # save united states average temperature in august data
  write_csv(contig_48_temp, "contig_48_avg_temp_aug.csv")
} else {
  # load united states average temperature in august data
  contig_48_temp <- read_csv("contig_48_avg_temp_aug.csv")
}


## load up the average global sea surface temperature
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "sea-surface-temp_fig-1.csv"
  url <- "https://www.epa.gov/sites/production/files/2016-08/sea-surface-temp_fig-1.csv"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  sea_surface_temp <- read_csv(file_name, skip = 6)
  
  # change the names of the columns
  names(sea_surface_temp) <- c("year", "annual_anomaly", "lower_95_ci", "upper_95_ci")
  
  # save sea surface temp data
  write_csv(sea_surface_temp, "sea_surface_temp.csv")
} else {
  # load sea surface temp data
  sea_surface_temp <- read_csv("sea_surface_temp.csv")
}

## download sea level data
# set working directory to data/sea_level directory
setwd("../sea_level")

## load up the global average sea level change data
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "sea-level_fig-1.csv"
  url <- "https://www.epa.gov/sites/production/files/2016-08/sea-level_fig-1.csv"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  sea_level <- read_csv(file_name, skip = 6)
  
  # modify column names
  names(sea_level) <- c("year", "adj_sea_level", "lower_error_bound", "upper_error_bound")
  
  # save sea surface temp data
  write_csv(sea_level, "sea_level.csv")
} else {
  # load sea surface temp data
  sea_level <- read_csv("sea_level.csv")
}

## download ice data
# set working directory to data/ice directory
setwd("../ice")

## load up the arctic sea ice data
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "arctic-sea-ice_fig-1.csv"
  url <- "https://www.epa.gov/sites/production/files/2016-11/arctic-sea-ice_fig-1.csv"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  arctic_sea_ice <- read_csv(file_name, skip = 6)
  
  # change column names
  names(arctic_sea_ice) <- c("year", "september", "march")
  
  # save arctic sea ice data
  write_csv(arctic_sea_ice, "arctic_sea_ice.csv")
} else {
  # load arctic sea ice data
  arctic_sea_ice <- read_csv("arctic_sea_ice.csv")
}

## load up the antarctic sea ice data
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "antarctic-sea-ice_fig-1.csv"
  url <- "https://www.epa.gov/sites/production/files/2016-08/antarctic-sea-ice_fig-1.csv"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  antarctic_sea_ice <- read_csv(file_name, skip = 6)
  
  # rename columns
  names(antarctic_sea_ice) <- c("year", "february", "september")
  
  # save antarctic sea ice data
  write_csv(antarctic_sea_ice, "antarctic_sea_ice.csv")
} else {
  # load antarctic sea ice data
  antarctic_sea_ice <- read_csv("antarctic_sea_ice.csv")
}

## load up the glacier ice data
if (download == 1) {
  # define filename and url
  rm(file_name)
  file_name <- "glaciers_fig-1.csv"
  url <- "https://www.epa.gov/sites/production/files/2016-08/glaciers_fig-1.csv"

  # download file
  download.file(url, file_name, method = "libcurl")
  
  # load file
  glacier_ice <- read_csv(file_name, skip = 6)
  
  # rename columns
  names(glacier_ice) <- c("year", "mass_balance", "num_of_obs")
  
  # save glacier ice data
  write_csv(glacier_ice, "glacier_ice.csv")
} else {
  # load glacier ice data
  glacier_ice <- read_csv("glacier_ice.csv")
}

setwd("../../code")
