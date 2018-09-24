## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      results='hide')

working.dir <- ''


## ----install_packages----------------------------------------------------
# install CRAN packages
list.of.packages.CRAN = c("tidyr", "dplyr", "ggplot2", "ggthemes", "curl", "zoo")

new.packages.CRAN = list.of.packages.CRAN[!(list.of.packages.CRAN %in% 
                                              installed.packages()[,"Package"])]
if(length(new.packages.CRAN)) install.packages(new.packages.CRAN)


# install aWhere code packages
list.of.packages.Github = c("aWhereAPI", "aWhereCharts")

new.packages.Github = list.of.packages.Github[!(list.of.packages.Github %in% 
                                                  installed.packages()[,"Package"])]
if(length(new.packages.Github) > 0) {
  for (x in 1:length(new.packages.Github)) {
    if (new.packages.Github[x] == 'aWhereAPI') {
      devtools::install_github("aWhereAPI/aWhere-R-Library")
    } else if (new.packages.Github[x] == 'aWhereCharts') {
      devtools::install_github("aWhereAPI/aWhere-R-Charts")
    }
  }
} 

## ----load_packages-------------------------------------------------------
# load packages 
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(curl)
library(zoo)
library(knitr)
library(aWhereAPI)
library(aWhereCharts)

## ----set_up_working_dir, eval = FALSE------------------------------------
#  #working directory - where input files are located and outputs will be saved.
#  working.dir <- c('~/aWhere/')
#  
#  # set the working directory
#  setwd(working.dir)

## ----createDirectoryStructure--------------------------------------------
# create necessary output directories
dir.create(path = 'outputCSVs/',showWarnings = FALSE, recursive = TRUE)
dir.create(path = 'figures/',showWarnings = FALSE, recursive = TRUE)

## ----load_local_functions------------------------------------------------
# load external R functions in local file
source(paste0(working.dir, "0-supporting_functions.R"))

## ----awhere_credentials--------------------------------------------------
# filename containing your aWhere credientials (key and secret),
credentials.file <- paste0(working.dir,'credentials.txt')

# load the aWhere API credentials file 
aWhereAPI::load_credentials(credentials.file) 

## ------------------------------------------------------------------------
# latitude, longitude, and name of location 
lat <- 8.45
lon <- 80.4
location.name <- paste("Ana") 

## ----data_timespan-------------------------------------------------------
# starting and ending years and days for the time series. each is a vector with 
# the starting value in position 1 and the ending value in position 2. 
years <- c(2010, 2017)

#starting and ending days with format "YYYY-MM-DD"
day.start <- "2018-08-01"

#day.end <- "2018-03-08" # specific date
day.end <- as.character(Sys.Date() + 7) # today plus n
day.end <- as.character(Sys.Date() + 6) # today plus n

# combine the days into a single vector 
days <- c(day.start, day.end)

## ----additional_chart_parameters-----------------------------------------
# effective precip amount for consistency across plots
eP <- 30    

# size of rolling average window for consistency across plots
roll.avg <- 30

## ----roll_avg_start_date-------------------------------------------------
# adjust the start date to be "roll.avg" days earlier
day.start.extended <- as.character(as.Date(day.start) - roll.avg)

# substitute this earlier date into the "days" vector for pulling data
days[1] <- day.start.extended

## ----check_duration------------------------------------------------------
# check if time range is more than 365 days
duration <- as.Date(days[2]) - as.Date(days[1])

if(duration > 365) { 
  print ("Period too long")
}

## ----pull_weather_data---------------------------------------------------
# pull the datasets for the specified location and time period   
weather.df <- aWhereCharts::generateaWhereDataset(lat = lat, lon = lon, 
                                                  day_start = days[1], 
                                                  day_end = days[2], 
                                                  year_start = years[1], 
                                                  year_end = years[2])

## ----check_weather_data--------------------------------------------------
# select the first five columns of the data frame using [,1:5]
# and show the first 10 rows using the "n" argument
utils::head(weather.df[,1:5], n = 10)

## ----results=TRUE--------------------------------------------------------
# reorder the columns in the data frame
weather.df <- weather.df %>% 
              dplyr::select(day, date, latitude, longitude, everything())

# look at the last few columns of the weather data frame. 
utils::head(weather.df[,(ncol(weather.df)-2):(ncol(weather.df))], n = 6)

## ----write_csv-----------------------------------------------------------
# write forecast to .csv file 
utils::write.csv(weather.df, 
                 file = paste0('outputCSVs/',
                              paste(location.name, 
                              paste(days, collapse="_"),
                              paste(years, collapse="_"),
                              ".csv", sep="_")))

# since the rolling averages have already been calculated and the 
# weather data extending out to an earlier start date (30 days in this case)
# has been saved to file, let's filter our weather dataframe down to span
# the original date range for chart creation. 
weather.df.extended <- weather.df

weather.df <- weather.df %>% 
  dplyr::filter(as.Date(date) >= day.start)


## ----chart_title---------------------------------------------------------
# create a variable containing the lat and lon, for plot titles
lat.lon <- paste0("(", lat, ", ", lon, ")") 

# construct title
max.temp.1.title <- paste0(location.name, "_Maximum Temp w StdDev")

## ----chart_max_temp------------------------------------------------------
# generate the plot 
max.temp.1 <- aWhereCharts::generateaWhereStdDevChart(data = weather.df, 
                                              variable = "maxTemp", 
                                              title = paste0(max.temp.1.title,
                                                              lat.lon))

## ----display_max_temp_chart, eval=FALSE----------------------------------
#  # display the plot
#  max.temp.1

## ----chart_min_temp, results=TRUE----------------------------------------
min.temp.1.title <- paste0(location.name, "_Minimum Temp w StdDev")

min.temp.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                              "minTemp", 
                                              title = paste0(min.temp.1.title,
                                                              lat.lon))

## ----chart_pet_stddev----------------------------------------------------
pet.1.title <- paste0(location.name, "_PET w StdDev")

pet.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                 "pet", 
                                                 title = paste0(pet.1.title,
                                                                lat.lon))

## ----chart_precip_stddev-------------------------------------------------
precip.1.title <- paste0(location.name, "_Daily Precipitation w StdDev")

precip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                              "precipitation",  
                                              title = paste0(precip.1.title,
                                                              lat.lon))

## ----chart_precip--------------------------------------------------------
precip.2.title <- paste0(location.name, "_Daily Precipitation")

precip.2 <- aWhereCharts::generateaWhereChart(weather.df,
                                              "precipitation",    
                                              title = paste0(precip.2.title,
                                                             lat.lon))

## ----chart_acc_precip_stddev---------------------------------------------
no.eprecip.1.title <- paste0(location.name, 
                             "_Accumulated Precipitation w StdDev")

no.eprecip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                            "accumulatedPrecipitation",
                                            title = paste0(no.eprecip.1.title,
                                                            lat.lon))

## ----chart_acc_precip_effprecip------------------------------------------
eprecip.1.title <- paste0(location.name, "_Precipitation and",
                          " Effective Precipitation, Accumulated w Std Dev")

eprecip.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                              "accumulatedPrecipitation",
                                              title = paste0(eprecip.1.title,
                                                            lat.lon),
                                              e_precip = TRUE, 
                                              e_threshold = eP)

## ----chart_acc_precip----------------------------------------------------
acc.precip.2.title <- paste0(location.name, "_Accumulated Precipitation")

acc.precip.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                            "accumulatedPrecipitation", 
                                            title = paste0(acc.precip.2.title,
                                                            lat.lon))

## ----chart_acc_pet-------------------------------------------------------
acc.pet.1.title <- paste0(location.name, "_Accumulated PET w StdDev")
 
acc.pet.1 <- aWhereCharts::generateaWhereStdDevChart(weather.df, 
                                                 "accumulatedPet",
                                                 title = paste0(acc.pet.1.title,
                                                               lat.lon))

## ----chart_ppet----------------------------------------------------------
# ppet rarely is interpretable on a daily chart 
ppet.2.title <- paste0(location.name,"_PPET ")
 
ppet.2 <- aWhereCharts::generateaWhereChart(weather.df, 
                                             "ppet", 
                                             title = paste0(ppet.2.title,
                                                            lat.lon))

## ----chart_roll_avg_ppet_eppet-------------------------------------------

# no eprecip/PET shows up if all rainfall events are less than the e_threshold
rolling.avg.ppet.2.title <- paste0(location.name,
                                   "_30 day rolling avg eP PET and P PET")

# use the "extended" weather data, since it contains dates 30 days prior
# to the original start date and will allow for rolling average values to
# be calculated from the original start date. 
rolling.avg.ppet.2 <- aWhereCharts::generateaWhereChart(weather.df.extended, 
                                      "rollingavgppet",
                                      title = paste0(rolling.avg.ppet.2.title,
                                                     lat.lon),
                                      e_precip = TRUE, 
                                      e_threshold = eP, 
                                      rolling_window = roll.avg) + 
                        xlim(as.Date(day.start), as.Date(day.end))

## ----chart_multiplot-----------------------------------------------------
# set the graphics device parameters to write a .JPEG
jpeg(paste0('figures/',location.name,"_4chart.jpeg"), 
     width = 12, height = 6, 
     units = 'in', res = 500)

# generate the multiplot & write to JPEG
aWhereCharts::generateMultiplot(acc.precip.2, 
                                rolling.avg.ppet.2, 
                                max.temp.1, 
                                pet.1, 
                                cols = 2, fontsize = 10, 
                                title = paste0("Current vs LTN at ", 
                                               location.name," (", 
                                               lat, ", ", lon, ")", 
                                               "   eP = ",eP,"mm"))
# close the current plot object
invisible(dev.off())

## ----display_multiplot, results=TRUE, out.width = '100%', fig.cap="Multiplot displaying four charts together. The dashed line marks the current date, beyond which the weather data is forecasted."----
knitr::include_graphics(paste0('figures/',location.name,"_4chart.jpeg"))

## ----charts_to_file------------------------------------------------------
# Maximum temperature
#max.temp.1 

# write the plot to file using the WriteJpeg function, an external R function
# in the "supporting_functions.R" file.
WriteJpeg(plt = max.temp.1, plt.title = paste0('figures/',max.temp.1.title))


# Minimum temperature with standard deviation
#min.temp.1 
WriteJpeg(plt = min.temp.1, plt.title = paste0('figures/',min.temp.1.title))


# Potential evapotranspiration (PET) with standard deviation 
#pet.1 
WriteJpeg(plt = pet.1, plt.title = paste0('figures/',pet.1.title))


# Daily precipitation with standard deviation  
#precip.1 
WriteJpeg(plt = precip.1 , plt.title = paste0('figures/',precip.1.title))


# Daily precipitation without standard deviation  
#precip.2 
WriteJpeg(plt = precip.2, plt.title = paste0('figures/',precip.2.title))


# Accumulated Precipitation with StdDev but no Effective Precipitation
#no.eprecip.1
WriteJpeg(plt = no.eprecip.1, plt.title = paste0('figures/',no.eprecip.1.title))


# Precipitation and Effective Precipitation, Accumulated 
#eprecip.1
WriteJpeg(plt = eprecip.1, plt.title = paste0('figures/',eprecip.1.title))


# Accumulated Precipitation 
#acc.precip.2
WriteJpeg(plt = acc.precip.2, plt.title = paste0('figures/',acc.precip.2.title))

# Accumulated PET 
#acc.pet.1
WriteJpeg(plt = acc.pet.1, plt.title = paste0('figures/',acc.pet.1.title))


# P/PET 
#ppet.2
WriteJpeg(plt = ppet.2, plt.title = paste0('figures/',ppet.2.title))


# 30-day rolling average eP/PET and P/PET 
#rolling.avg.ppet.2
WriteJpeg(plt = rolling.avg.ppet.2, 
          plt.title = paste0('figures/',rolling.avg.ppet.2.title))


