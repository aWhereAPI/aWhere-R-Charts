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

## ----add_years-----------------------------------------------------------
# add additional selected year(s) to charts. 
# If you do not want to add additional years, set add.year = NA
#add.years <- NA
add.years <- c(2016, 2017)

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

## ----wide_long_example, echo=FALSE---------------------------------------
# create example data with wide format
data.wide <- data.frame(individual = c(1, 2, 3),
                        feature_A = c('red', 'green', 'blue'),
                        feature_B = c(24, 27, 31),
                        feature_C = c(2004, 2008, 2011))

# reshape the dataset to have long format.
# keep "individual" in the first column, use the "feature"
# number as the key (new variable) and fill in the values
# for each feature for 
data.long <- tidyr::gather(data = data.wide, 
                           key = feature, 
                           value = value, 
                           feature_A:feature_C)

## ----wide_table, echo=FALSE, results='asis'------------------------------
# create nicely formatted example of "wide" data
knitr::kable(data.wide, 
             align=c(rep('c', times=4)),
             caption = "Example dataset with wide format")

## ----long_table, echo=FALSE, results='asis'------------------------------
# nicely formatted example of "long" data
knitr::kable(data.long, 
             align=c(rep('c', times=3)),
             caption = "Same example dataset with long format")

## ----wide_long_weather_example, echo=FALSE-------------------------------
# create a wide dataset
data.wide <- data.frame(date = c("2018-05-01", "2018-05-02","2018-05-03"),
                        precip_2016 = c(4.56, 24.28, 11.25),
                        precip_2017 = c(5.39,  8.36,  2.03),
                        PET_2016 = c(5.04, 5.08, 5.02),
                        PET_2017 = c(7.58, 6.80, 6.17))

# create a long dataset 
data.long <- data.frame(date = c("2018-05-01", "2018-05-02","2018-05-03",
                                 "2018-05-01", "2018-05-02","2018-05-03", 
                                 "2018-05-01", "2018-05-02","2018-05-03",
                                 "2018-05-01", "2018-05-02","2018-05-03"),
                        data = c(4.56, 24.28, 11.25,
                                 5.39,  8.36,  2.03,
                                 5.04, 5.08, 5.02,
                                 7.58, 6.80, 6.17),
                        variable = c("precip", "precip", "precip",
                                "precip", "precip", "precip",
                                "PET", "PET", "PET",
                                "PET", "PET", "PET"),
                        year = c(2016, 2016, 2016,
                                 2017, 2017, 2017,
                                 2016, 2016, 2016,
                                 2017, 2017, 2017))

## ----wide_long_weather_tables, echo=FALSE, results='asis'----------------
# example of wide data format
knitr::kable(data.wide,
             caption="Example weather dataset with wide format")

# example of long data format 
knitr::kable(data.long,
             caption="Same example weather dataset with wide format")

## ----add_years_long_data-------------------------------------------------
# create a data frame to store additional selected year data.
# the "long" format of this data frame is ideal for plotting with ggplot.
add.years.df <- data.frame(date = character(),
                           data = double(),
                           var = character(),
                           year = double())

## ----pull_data_add_years-------------------------------------------------
# pull the data sets for additional selected year(s)
for (year in add.years){
  
  # print a status message to the console 
  print(paste("Pulling weather data for selected year:", year, sep = " "))
  
  # if there are no additional selected years to add, break out of loop
  if(is.na(year)){
    break
  }
  
  # for each additional selected year, modify the start/end day values.
  # combine the selected year with the month-day values.
  day.start.mod <- paste(year,
                        paste(strsplit(x = days[1], split = "-")[[1]][2:3],
                                    collapse='-'),
                         sep="-")

  ending.year <- as.numeric(strsplit(x = days[2], split = "-")[[1]][1])
  starting.year <- as.numeric(strsplit(x = days[1], split = "-")[[1]][1])
  
  # if the date range spans multiple years, make sure the end date 
  # includes the selected year + 1.  
  if(ending.year == (starting.year + 1)){
    day.end.mod <- paste(year+1,
                         paste(strsplit(x = days[2], split = "-")[[1]][2:3],
                               collapse='-'),
                         sep="-")
  } else {
    day.end.mod <- paste(year,
                         paste(strsplit(x = days[2], split = "-")[[1]][2:3],
                               collapse='-'),
                         sep="-")
  }
  
  # pull data for selected year with modified start/end days
  weather.df.mod <- aWhereCharts::generateaWhereDataset(lat = lat, lon = lon, 
                                                      day_start = day.start.mod, 
                                                      day_end = day.end.mod, 
                                                      year_start = years[1], 
                                                      year_end = years[2])
  
  # save the accumulated precip data needed for charts in "long" format
  temp.data <- data.frame(date = weather.df$date, 
                          data = weather.df.mod$accumulatedPrecipitation.amount,
                          var = 'accumulatedPrecipitation.amount',
                          year = year) 
  
  # combine the accumulated precip data with other additional year data
  add.years.df <- rbind(add.years.df, temp.data)                       
  
  # calculate P/PET rolling average values for chart in "long" format
  temp.data <- data.frame(date = weather.df$date, 
                          data = weather.df.mod$ppet.amount,
                          var = 'ppet.amount.rollAvg',
                          year = year) 
  temp.data$data <- zoo::rollapply(temp.data$data
                                           ,width = roll.avg
                                           ,align = "right"
                                           ,FUN = mean
                                           ,na.rm = TRUE
                                           ,fill = NA)
  # combine the rolling average P/PET with other additional year data
  add.years.df <- rbind(add.years.df, temp.data) 
  
  # calculate eP/PET rolling average. first, use the ClipValues function to
  # calculate effective precipitation, then divide eP by PET for eP/PET. 
  weather.df.mod$ePPET <- ClipValues(weather.df.mod$precipitation.amount, 
                                     max.thresh = eP) /
                          weather.df.mod$pet.amount
  temp.data$date <- weather.df$date
  temp.data$data <- weather.df.mod$ePPET
  temp.data$data <- zoo::rollapply(temp.data$data
                                   ,width = roll.avg
                                   ,align = "right"
                                   ,FUN = mean
                                   ,na.rm = TRUE
                                   ,fill = NA)
  temp.data$var <- 'eppet.amount.rollAvg'
  temp.data$year <- year
  
  # combine the rolling average eP/PET with other additional year data
  add.years.df <- rbind(add.years.df, temp.data) 
  
  
  # add the current selected year's accumulated precip and PPET columns to the
  # main weather dataframe to write to .csv
  
  # Accumulated Precipitation amount
  weather.df$accumulatedPrecipitation.amount.year <- 
                              weather.df.mod$accumulatedPrecipitation.amount

  # P/PET 
  weather.df$ppet.amount.year <- weather.df.mod$ppet.amount
  weather.df$precipitation.amount.year <- weather.df.mod$precipitation.amount
  weather.df$pet.amount.year <- weather.df.mod$pet.amount
  
  # rename the columns to have the current year value appended on the end,
  # such as precipitation.amount.year.2016
  
  # Accumulated Precipitation 
  colnames(weather.df)[colnames(weather.df) == 
                         "accumulatedPrecipitation.amount.year"] <- 
                        paste0("accumulatedPrecipitation.amount.year", year)

  # P/PET
  colnames(weather.df)[colnames(weather.df) ==
                        "ppet.amount.year"] <- paste0("ppet.amount.year", year)
  colnames(weather.df)[colnames(weather.df) ==
      "precipitation.amount.year"] <- paste0("precipitation.amount.year", year)
  colnames(weather.df)[colnames(weather.df) ==
                        "pet.amount.year"] <- paste0("pet.amount.year", year)
}

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

# copy the data frame with extended dates for rolling averages
add.years.df.extended <- add.years.df

# filter the additional selected year data according to date
add.years.df <- add.years.df %>% 
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

## ----acc_precip_addyears-------------------------------------------------
# filter the add.years data frame for the accumulated precip data.
add.years.accPrecip <- add.years.df %>% 
  dplyr::filter(var == "accumulatedPrecipitation.amount")

# generate color scale to use including the colors of the original chart:
# list of original colors for current and LTN lines 
colors.orig <- c("#1F83B4", "#FF810E")

# list of unique colors for additional lines added 
colors.additional <- c("black", "red", "yellow", "purple")

# just take the number of line colors that are needed for the final chart
colors.final <- c(colors.additional[1:length(add.years)], colors.orig)

acc.precip.2.addyears.title <- paste0(location.name, 
                                      "_Accumulated Precipitation ",
                                      "with additional selected years")

# add the additional selected year lines to the acc precip chart
acc.precip.2.addyears <- acc.precip.2 + 
  geom_line(aes(x = as.Date(add.years.accPrecip$date),
                y = add.years.accPrecip[,'data'],
                color = as.factor(add.years.accPrecip[,'year'])),
            size = 1.5) +
  ggtitle(acc.precip.2.addyears.title) + 
  scale_color_manual(values = colors.final)

## ----chart_acc_precip_addyears, results=TRUE, fig.cap="Accumulated precipitation across the date range specified for the  current year compared to the long term-normal (LTN) and additional selected years. The dashed line marks the current date, beyond which the weather data is forecasted."----
acc.precip.2.addyears

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

## ----chart_roll_avg_ppet_addyears, results=TRUE, fig.cap="30-day rolling average P/PET across the date range specified for the current year compared to the long term-normal (LTN) and additional selected years. The dashed line marks the current date, beyond which the weather data is forecasted."----

rolling.avg.ppet.2.addyears.title <- paste0(location.name,
                                 "_30 day rolling avg P PET ",
                                 "with additional selected years")

# get a chart of P/PET without effective precip (set e_precip to FALSE).
# as for the previous chart, use the weather date with "extended" dates
# for this rolling average calculation. 
rolling.avg.ppet <- aWhereCharts::generateaWhereChart(weather.df.extended, 
                                                        "rollingavgppet",
                                  title = paste0(rolling.avg.ppet.2.addyears.title,
                                                 lat.lon),
                                  e_precip = FALSE, 
                                  e_threshold = eP, 
                                  rolling_window = roll.avg)
# filter the add.years data frame for just the rolling average P/PET data
add.years.rollAvgPPet <- add.years.df %>% dplyr::filter(var == "ppet.amount.rollAvg")

# add P/PET lines to the chart for additional selected years.
# set the x-axis limits to reflect the specified start and end dates.
rolling.avg.ppet.2.addyears <- rolling.avg.ppet + 
  geom_line(aes(x = as.Date(add.years.rollAvgPPet$date),
                y = add.years.rollAvgPPet[,'data'],
                color = as.factor(add.years.rollAvgPPet[,'year'])),
            size = 1.5) +
  ggtitle(rolling.avg.ppet.2.addyears.title) + 
  scale_color_manual(values = colors.final) +
  xlim(as.Date(day.start), as.Date(day.end))

# display the chart. 
rolling.avg.ppet.2.addyears

## ----chart_roll_avg_eppet_addyears, results=TRUE, fig.cap="30-day rolling average eP/PET across the date range specified for the current year compared to the long term-normal (LTN) and additional selected years. The dashed line marks the current date, beyond which the weather data is forecasted."----

rolling.avg.eppet.2.addyears.title <- paste0(location.name,
                                          "_30 day rolling avg eP PET ",
                                          "with additional selected years")

# get a chart of eP/PET. Since there is no way to isolate the eP/PET curve 
# within the generateaWhereChart function, let's cap the precipitation using 
# the eP threshold and then supply this to the function to chart eP/PET for 
# the current year and LTN. 
weather.df$precipitation.amount <- ClipValues(weather.df$precipitation.amount,
                                              max.thresh = eP)

rolling.avg.eppet <- aWhereCharts::generateaWhereChart(weather.df.extended, 
                          "rollingavgppet",
                          title = paste0(rolling.avg.eppet.2.addyears.title,
                                         lat.lon),
                          e_precip = FALSE, 
                          e_threshold = eP, 
                          rolling_window = roll.avg) + 
                     xlim(as.Date(day.start), as.Date(day.end))

# filter the add.years data frame for just the rolling average P/PET data
add.years.rollAvgePPet <- add.years.df %>% 
  dplyr::filter(var == "eppet.amount.rollAvg")

# add P/PET lines to the chart for additional selected years 
rolling.avg.eppet.2.addyears <- rolling.avg.eppet + 
  geom_line(aes(x = as.Date(add.years.rollAvgePPet$date),
                y = add.years.rollAvgePPet[,'data'],
                color = as.factor(add.years.rollAvgePPet[,'year'])),
            size = 1.5) +
  ggtitle(rolling.avg.eppet.2.addyears.title) + 
  scale_color_manual(values = colors.final) + 
  xlim(as.Date(day.start), as.Date(day.end))

# display the chart. 
rolling.avg.eppet.2.addyears

## ----chart_multiplot-----------------------------------------------------
# set the graphics device parameters to write a .JPEG
jpeg(paste0('figures/',location.name,"_4chart.jpeg"), 
     width = 12, height = 6, 
     units = 'in', res = 500)

# generate the multiplot & write to JPEG
aWhereCharts::generateMultiplot(acc.precip.2.addyears, 
                                rolling.avg.ppet.2.addyears, 
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


# Accumulated Precipitation with additional selected years
#acc.precip.2.addyears
WriteJpeg(plt = acc.precip.2.addyears, 
          plt.title = paste0('figures/',acc.precip.2.addyears.title))


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


# 30-day rolling average P/PET with additional selected years
#rolling.avg.ppet.2.addyears
WriteJpeg(plt = rolling.avg.ppet.2.addyears, 
          plt.title = paste0('figures/',rolling.avg.ppet.2.addyears.title))


# 30-day rolling average eP/PET with additional selected years
#rolling.avg.eppet.2.addyears
WriteJpeg(plt = rolling.avg.eppet.2.addyears, 
          plt.title = paste0('figures/',rolling.avg.eppet.2.addyears.title))

