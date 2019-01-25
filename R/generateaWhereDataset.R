#' @title generateaWhereDataset
#'
#' @description
#' \code{generateaWhereDataset} Generate a basic aWhere weather dataset for a given point and timespan.
#'
#' @details
#' This function returns an organized dataset from the aWhere API with a pre-set list
#' of variables for the given coordinates and spanning the entire input datespan, which
#' can include forecasted dates. Forecast data will be returned in 24-hour/1-day blocks,
#' in order to be harmonious with the past observed data. The pre-set variables include
#' temperature, rainfall, PET, P/PET, GDDs, and variable accumulations. The pre-set list
#' is not customizable. Users who wish to investigate other variables returned by the
#' aWhere API - like humidity, solar radiation, and more - should use the base functions
#' provided in the aWhereAPI package. This function is intended to get a user started
#' with retrieving data and analyzing it quickly, without the need for making multiple
#' function calls or merging the results.
#'
#' @references http://developer.awhere.com/api/reference/
#'
#' @param - lat: numeric latitude point in decimal degrees (required)
#' @param - lon: numeric longitude point in decimal degrees (required)
#' @param - day_start: character string in YYYY-MM-DD format,
#'             denoting the first day for which you want weather data retrieved  (required)
#' @param - day_end: character string in YYYY-MM-DD format,
#'             denoting the last day for which you want weather data retrieved  (required)
#' @param - year_start: numeric in YYYY format, denoting the first year
#'             of data which should be included in the norms calculation.  (required)
#' @param - year_end: numeric in YYYY format, denoting the last year
#'             of data which should be included in the norms calculation (required)
#'
#' @import dplyr
#' @import aWhereAPI
#' @import data.table
#' @import lubridate
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{generateaWhereDataset(lat = 30.685
#'                               ,lon = 72.928
#'                               ,day_start = "2018-09-01"
#'                               ,day_end = "2019-01-20"
#'                               ,year_start = 2008
#'                               ,year_end = 2018)}

#' @export

generateaWhereDataset <- function(lat
                                  ,lon
                                  ,day_start
                                  ,day_end
                                  ,year_start
                                  ,year_end) {

  if (exists('awhereEnv75247') == FALSE) {
    stop('Please load credentials for aWhereAPI before continuing')
  }
  
  #set month-day combos to pull LTN for. If more than one year of data is requested, then
  #Jan 1-December 31 is set. Otherwise, the span is set based on the date span of the query.
  if((as.numeric(as.Date(day_end)) - as.numeric(as.Date(day_start))) < 365) {
    monthday_start <- substr(day_start, 6, 10)
    monthday_end <- substr(day_end, 6, 10)
  } else {
    monthday_start <- "01-01"
    monthday_end <- "12-31"
  }


  #if forecast data is requested, set an interim day_end for obs/ag queries 
  #Because of time zone differences between where the call is being made from 
  #and for, simply using the user's current date and subtracting one is not 
  #sufficient.  What we will do instead and is test empirically for the last
  #date that makes a valid return from the historical API and then use that
  #going forward.  Subsequent forecast calls will use this date + 1 as their starting point
  
  if((day_start >= (Sys.Date()+2)) == TRUE) {
    onlyForecastRequested <- TRUE
  } else {
    dateToTest <- Sys.Date()  - 2 #this is one extra day of testing than should be necessary
    
    repeatQuery <- TRUE
    while(repeatQuery == TRUE) {
      repeatQuery <- FALSE
      
      temp <- tryCatch({
        
        aWhereAPI::forecasts_latlng(lat
                                    ,lon
                                    ,day_start = as.character(dateToTest)
                                    ,day_end = as.character(dateToTest)
                                    ,block_size = 24)
        
      }, error = function(e) {
        repeatQuery <- TRUE
        dateToTest <- dateToTest+1
        return(list(repeatQuery,dateToTest))
      })
      #Because there is no explicit error object in the return, we will instead
      #test if the fxn returned a data.frame indicating the query worked
      if(is.data.frame(temp) == FALSE) {
        repeatQuery <- temp[[1]][1]
        dateToTest <- temp[[2]][1]
        
        rm(temp)
      }
    }
    
    if (dateToTest <= lubridate::ymd(day_start)) {
      onlyForecastRequested <- TRUE
    } else {
      onlyForecastRequested <- FALSE
    }
  }
  
  
  #No matter where you are on the planet, another spot can be no more than 24
  #hours difference in time.  Therefore, if we check if the end_date used in the
  #call is more than 2 days later than the current date, we know we need to hit
  #the historical endpoint
  if((day_end <= (Sys.Date()-2)) == TRUE) {
    interim_day_end <- day_end
    
  } else {
    dateToTest <- Sys.Date() + 2 #this is one extra day of testing than should be necessary
    
    repeatQuery <- TRUE
    while(repeatQuery == TRUE) {
      repeatQuery <- FALSE

      temp <- tryCatch({
                aWhereAPI::daily_observed_latlng(lat
                                              ,lon
                                              ,day_start = as.character(dateToTest)
                                              ,day_end = as.character(dateToTest))
              }, error = function(e) {
                repeatQuery <- TRUE
                dateToTest <- dateToTest-1
                return(list(repeatQuery,dateToTest))
              })
      #Because there is no explicit error object in the return, we will instead
      #test if the fxn returned a data.frame indicating the query worked
      if(is.data.frame(temp) == FALSE) {
        repeatQuery <- temp[[1]][1]
        dateToTest <- temp[[2]][1]
        
        rm(temp)
      }
    }
    
    interim_day_end <- dateToTest
  }

  #we are going to add prefix to the various columns to indicate where they came
  #from to make the logic of joining easier to understand
  colsNoChange <- c('latitude'
                    ,'longitude'
                    ,'date'
                    ,'day')

  
###############################################################################
#Get observed Weather data
##############################################################################
  #Because the observed weather endpoint does not return forecast data, we need 
  #to have logic so that the obs data structure is present regardless of whether
  # we will actually have historical data for later joins to work
  if (onlyForecastRequested == FALSE) {
    obs <- suppressWarnings(aWhereAPI::daily_observed_latlng(lat
                                                             ,lon
                                                             ,day_start
                                                             ,day_end = as.character(interim_day_end))) %>%
      data.table::as.data.table(.) %>%
      .[,date := lubridate::ymd(date)] %>%
      .[,day := gsub(pattern = '20\\d\\d-',replacement = '',x = date)]
      
  
    obs.names <- colnames(obs)
  
    #add prefix to indicate this is historical data
    setnames(obs
             ,setdiff(obs.names,colsNoChange)
             ,paste0('obs.',setdiff(obs.names,colsNoChange)))
    
    
  } else {
    #based on the code below we need to have a data.frame with this name that
    #has at least the columns date and day for the merge to be successful.  The
    #merge is doing an outer join
    obs <- data.frame(latitude = 0
                      ,longitude = 0
                      ,date = Sys.Date()
                      ,day = 0
                      ,obs.temperatures.max = 0
                      ,obs.temperatures.min = 0
                      ,obs.precipitation.amount = 0
                      ,obs.solar.amount = 0
                      ,obs.relativeHumidity.max = 0
                      ,obs.relativeHumidity.min = 0
                      ,obs.wind.morningMax = 0
                      ,obs.wind.dayMax = 0
                      ,obs.wind.average = 0)
    obs <- obs[0,]
  }
###############################################################################
#Get observed Agronomics data
##############################################################################

  ##This works becayse the Ag endpoint includes forecast data
  ag <- suppressWarnings(aWhereAPI::agronomic_values_latlng(latitude = lat
                                                            ,longitude = lon
                                                            ,day_start = day_start
                                                            ,day_end = day_end)) %>%
    data.table::as.data.table(.) %>%
    .[,date := lubridate::ymd(date)] %>%
    .[,day := gsub(pattern = '20\\d\\d-',replacement = '',x = date)]
  
  ag.names <- colnames(ag)
  
  #add prefix to indicate this is historical data
  setnames(ag
           ,setdiff(ag.names,colsNoChange)
           ,paste0('obs.',setdiff(ag.names,colsNoChange)))

###############################################################################
#Get LTN Weather data
##############################################################################
    
  obs_ltn <- suppressWarnings(aWhereAPI::weather_norms_latlng(latitude = lat
                                                              ,longitude = lon
                                                              ,monthday_start = monthday_start
                                                              ,monthday_end = monthday_end
                                                              ,year_start = year_start
                                                              ,year_end = year_end
                                                              ,includeFeb29thData = FALSE)) %>%
    data.table::as.data.table(.)

  obs_ltn.names <- colnames(obs_ltn)
  
  #add prefix to indicate this is historical data
  setnames(obs_ltn
           ,setdiff(obs_ltn.names,colsNoChange)
           ,paste0('ltn.',setdiff(obs_ltn.names,colsNoChange)))
  
###############################################################################
#Get LTN Agronomics data
##############################################################################
  
  ag_ltn <- suppressWarnings(aWhereAPI::agronomic_norms_latlng(latitude = lat
                                                               ,longitude = lon
                                                               ,month_day_start = monthday_start
                                                               ,month_day_end = monthday_end
                                                               ,year_start = year_start
                                                               ,year_end = year_end
                                                               ,includeFeb29thData = FALSE)) %>%
    data.table::as.data.table(.)
  
  ag_ltn.names <- colnames(ag_ltn)
  
  #add prefix to indicate this is historical data
  setnames(ag_ltn
           ,setdiff(ag_ltn.names,colsNoChange)
           ,paste0('ltn.',setdiff(ag_ltn.names,colsNoChange)))
###############################################################################
#Get forecast data
##############################################################################
  #pull forecasted data for determined time period We are using what is
  #determined above to be the last valid day of historical data at this
  #location at the time the API call is made and incrementing by one day to
  #get the date range fo rthis call
  #
  
  if (interim_day_end != day_end) {
    forecast <- aWhereAPI::forecasts_latlng(lat
                                            ,lon
                                            ,day_start = as.character(interim_day_end + 1)
                                            ,day_end = day_end
                                            ,block_size = 24) %>%
      data.table::as.data.table(.) %>%
      .[,date := lubridate::ymd(tstrsplit(x = startTime
                                          ,split = 'T'
                                          ,fixed = TRUE
                                          ,keep = 1)[[1]])]  %>%
      .[,day := gsub(pattern = '20\\d\\d-',replacement = '',x = date)]
    
  
    forecast <- forecast[,-c('startTime'
                             ,'endTime'
                             ,'conditionsCode'
                             ,'conditionsText'
                             ,'precipitation.chance'
                             ,'sky.cloudCover'
                             ,'sky.sunshine'
                             ,'dewPoint.amount'),with = FALSE]
    
    forecast.names <- colnames(forecast)
    
    #add prefix to indicate this is historical data
    setnames(forecast
             ,setdiff(forecast.names,colsNoChange)
             ,paste0('forecast.',setdiff(forecast.names,colsNoChange)))
    
  } else {
    forecast <- data.frame(latitude = 0
                      ,longitude = 0
                      ,date = ymd(Sys.Date())
                      ,day = as.character(0)
                      ,forecast.temperatures.max = 0
                      ,forecast.temperatures.min = 0
                      ,forecast.precipitation.amount = 0
                      ,forecast.solar.amount = 0
                      ,forecast.relativeHumidity.average = 0
                      ,forecast.relativeHumidity.max = 0
                      ,forecast.relativeHumidity.min = 0
                      ,forecast.wind.average = 0
                      ,forecast.wind.max = 0
                      ,forecast.wind.min = 0)
    forecast <- forecast[0,]
  }

###############################################################################
#REPLACE ANY MISSING DATA WITH LTN VALUES
##############################################################################

  if(all(complete.cases(obs) == TRUE) == FALSE) {
    
    obs.names <- colnames(obs)
    
    obs <- merge(obs
                ,obs_ltn
                ,by = c('latitude','longitude','day'))
          
    
    obs[is.na(obs.temperatures.max),      obs.temperatures.max      := ltn.maxTemp.average]
    obs[is.na(obs.temperatures.min),      obs.temperatures.min      := ltn.minTemp.average]
    obs[is.na(obs.precipitation.amount),  obs.precipitation.amount  := ltn.precipitation.average]
    obs[is.na(obs.solar.amount),          obs.solar.amount          := ltn.solar.average]
    obs[is.na(obs.relativeHumidity.max),  obs.relativeHumidity.max  := ltn.maxHumidity.average]
    obs[is.na(obs.relativeHumidity.min),  obs.relativeHumidity.min  := ltn.minHumidity.average]
    obs[is.na(obs.wind.morningMax),       obs.wind.morningMax       := ltn.dailyMaxWind.average]
    obs[is.na(obs.wind.dayMax),           obs.wind.dayMax           := ltn.dailyMaxWind.average]
    obs[is.na(obs.wind.average),          obs.wind.average          := ltn.averageWind.average]
    
    obs <- obs[,obs.names,with = FALSE]
  } 
  
  if(all(complete.cases(ag) == TRUE) == FALSE) {
    
    ag.names <- colnames(ag)
    
    ag <- merge(ag
                 ,ag_ltn
                 ,by = c('latitude','longitude','day'))
    ag <- merge(ag
                ,obs
                ,by = c('latitude','longitude','day'))
    
    ag[is.na(obs.gdd),        obs.gdd        := ltn.gdd.average]
    ag[is.na(obs.ppet),       obs.ppet       := ltn.ppet.average]
    ag[is.na(obs.pet.amount), obs.pet.amount := ltn.pet.average]
    
    ag[,obs.accumulatedGdd                  := cumsum(gdd)]
    ag[,obs.accumulatedPpet                 := cumsum(ppet)]
    ag[,obs.accumulatedPrecipitation.amount := cumsum(precipitation.amount)]
    ag[,obs.accumulatedPet.amount           := cumsum(pet.amount)]
    
    ag <- ag[,ag.names,with = FALSE]
  } 
  
###############################################################################
#Assemble final dataset
##############################################################################

  weather_full <- merge(obs,          ag,      by = c("date", "day", "latitude", "longitude"), all = TRUE)
  weather_full <- merge(weather_full, obs_ltn, by = c("day", "latitude", "longitude"))
  weather_full <- merge(weather_full, ag_ltn,  by = c("day", "latitude", "longitude"))
  
  setkey(weather_full,date)
  
  weather_full.names <- colnames(weather_full)
  
  weather_full <- merge(weather_full
                        ,forecast
                        ,by = c("date", "day", "latitude", "longitude")
                        ,all = TRUE)
  
  #This is meant to add the forecast data to the observed columns
  
  weather_full[is.na(obs.temperatures.max),      obs.temperatures.max      := forecast.temperatures.max]
  weather_full[is.na(obs.temperatures.min),      obs.temperatures.min      := forecast.temperatures.min]
  weather_full[is.na(obs.precipitation.amount),  obs.precipitation.amount  := forecast.precipitation.amount]
  weather_full[is.na(obs.solar.amount),          obs.solar.amount          := forecast.solar.amount]
  weather_full[is.na(obs.relativeHumidity.max),  obs.relativeHumidity.max  := forecast.relativeHumidity.max]
  weather_full[is.na(obs.relativeHumidity.min),  obs.relativeHumidity.min  := forecast.relativeHumidity.min]
  weather_full[is.na(obs.wind.morningMax),       obs.wind.morningMax       := forecast.wind.max]
  weather_full[is.na(obs.wind.dayMax),           obs.wind.dayMax           := forecast.wind.max]
  weather_full[is.na(obs.wind.average),          obs.wind.average          := forecast.wind.average]
  
  weather_full[,obs.accumulatedPrecipitation.amount := cumsum(obs.precipitation.amount)]


  weather_full <- weather_full[,weather_full.names,with = FALSE]
  
  #make names of columns match between obs and ltn
  setnames(weather_full,c('ltn.meanTemp.average'
                          ,'ltn.meanTemp.stdDev'
                          ,'ltn.maxTemp.average'
                          ,'ltn.maxTemp.stdDev'
                          ,'ltn.minTemp.average'
                          ,'ltn.minTemp.stdDev'
                          ,'ltn.minHumidity.average'
                          ,'ltn.minHumidity.stdDev'
                          ,'ltn.maxHumidity.average'
                          ,'ltn.maxHumidity.stdDev'
                          ,'ltn.dailyMaxWind.average'
                          ,'ltn.dailyMaxWind.stdDev'
                          ,'ltn.averageWind.average'
                          ,'ltn.averageWind.stdDev'),c('ltn.temperatures.mean.average'
                                                   ,'ltn.temperatures.mean.stdDev'
                                                   ,'ltn.temperatures.max.average'
                                                   ,'ltn.temperatures.max.stdDev'
                                                   ,'ltn.temperatures.min.average'
                                                   ,'ltn.temperatures.min.stddev'
                                                   ,'ltn.relativeHumidity.min.average'
                                                   ,'ltn.relativeHumidity.min.stdDev'
                                                   ,'ltn.relativeHumidity.max.average'
                                                   ,'ltn.relativeHumidity.max.stdDev'
                                                   ,'ltn.wind.dayMax.average'
                                                   ,'ltn.wind.dayMax.stdDev'
                                                   ,'ltn.wind.average.average'
                                                   ,'ltn.wind.average.stdDev'))
  
  #add amount to end of relevant columns it is currently missing from
  columnsToChange <- c('obs.temperatures.max'
                       ,'obs.temperatures.min'
                       ,'obs.relativeHumidity.max'
                       ,'obs.relativeHumidity.min'
                       ,'obs.wind.morningMax'
                       ,'obs.wind.dayMax'
                       ,'obs.wind.average'
                       ,'obs.gdd'
                       ,'obs.ppet'
                       ,'obs.accumulatedGdd'
                       ,'obs.accumulatedPpet')

  setnames(weather_full
           ,columnsToChange
           ,paste0(columnsToChange,'.amount'))
  

  #remove the prefix's on the column names
  setnames(weather_full,gsub(pattern = 'ltn.'
                             ,replacement = ''
                             ,x = gsub(pattern = 'obs.'
                                       ,replacement = ''
                                       ,x = colnames(weather_full)
                                       ,fixed = TRUE)
                             ,fixed = TRUE))
  
  #arrange columns appropriately
  weather_full.names <- colnames(weather_full)
  
  leadingColumns <- c('latitude'
                      ,'longitude'
                      ,'date'
                      ,'day')
  
  weather_full.names <- sort(setdiff(weather_full.names,leadingColumns))
  
  setcolorder(weather_full,c(leadingColumns,weather_full.names))
  
  #remove variables that aren't in both the Obs and LTN datasets
  weather_full[,c('temperatures.mean.average'
                  ,'temperatures.mean.stdDev'
                  ,'wind.morningMax.amount') := NULL]
  
  #ADD LOGIC TO ENSURE THAT NOTHING IS IMPOSSIBLE VALUES
  

  return(weather_full)
}
