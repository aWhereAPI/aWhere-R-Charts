#' @title generateaWhereDataset
#'
#' @description \code{generateaWhereDataset} Generate a basic aWhere weather
#'   dataset for a given point and timespan.
#'
#' @details This function returns an organized dataset from the aWhere API with
#'   a pre-set list of variables for the given coordinates and spanning the
#'   entire input datespan, which can include forecasted dates. Forecast data
#'   will be returned in 24-hour/1-day blocks, in order to be harmonious with
#'   the past observed data. The pre-set variables include temperature,
#'   rainfall, PET, P/PET, GDDs, and variable accumulations. The pre-set list is
#'   not customizable. Users who wish to investigate other variables returned by
#'   the aWhere API - like humidity, solar radiation, and more - should use the
#'   base functions provided in the aWhereAPI package. This function is intended
#'   to get a user started with retrieving data and analyzing it quickly,
#'   without the need for making multiple function calls or merging the results.
#'
#' @references http://developer.awhere.com/api/reference/
#'
#' @param lat numeric latitude point in decimal degrees (required)
#' @param lon numeric longitude point in decimal degrees (required)
#' @param day_start character string in YYYY-MM-DD format, denoting the first
#'   day for which you want weather data retrieved  (required)
#' @param day_end character string in YYYY-MM-DD format, denoting the last day
#'   for which you want weather data retrieved  (required)
#' @param year_start numeric in YYYY format, denoting the first year of data
#'   which should be included in the norms calculation.  (required)
#' @param year_end numeric in YYYY format, denoting the last year of data which
#'   should be included in the norms calculation (required)
#' @param verbose print additional diagnostic messages during runtime (optional)
#' @param appendPrevDataPull disable behavior for R to request and append only
#'   novel data for the current location but instead to create data object fresh
#'   at all times (optional)
#' @param removeFeb29Data automatically remove Feb 29th data (optional)   
#'
#' @import dplyr
#' @import aWhereAPI
#' @import data.table
#' @import lubridate
#' @import purrr
#' @import pryr
#'
#' @return dataframe with the following schema: latitude, longitude, date, day, 
#'         accumulatedGdd.amount, accumulatedGdd.average, accumulatedGdd.stdDev, 
#'         accumulatedPet.amount, accumulatedPet.average, accumulatedPet.stdDev, 
#'         accumulatedPpet.amount, accumulatedPpet.average, accumulatedPpet.stdDev, 
#'         accumulatedPrecipitation.amount, accumulatedPrecipitation.average, 
#'         accumulatedPrecipitation.stdDev, gdd.amount, gdd.average, gdd.stdDev, 
#'         pet.amount, pet.average, pet.stdDev, ppet.amount, ppet.average, ppet.stdDev, 
#'         precipitation.amount, precipitation.average, precipitation.stdDev, 
#'         relativeHumidity.max.amount, relativeHumidity.max.average, relativeHumidity.max.stdDev, 
#'         relativeHumidity.min.amount, relativeHumidity.min.average, relativeHumidity.min.stdDev, 
#'         solar.amount, solar.average, solar.stdDev, temperatures.max.amount, temperatures.max.average,
#'         temperatures.max.stdDev, temperatures.min.amount, temperatures.min.average, 
#'         temperatures.min.stdDev, wind.average.amount, wind.average.average, wind.average.stdDev, 
#'         wind.dayMax.amount, wind.dayMax.average, wind.dayMax.stdDev
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
                                  ,year_end
                                  ,verbose = TRUE
                                  ,appendPrevDataPull = TRUE
                                  ,removeFeb29Data = FALSE) {
  
  if (exists('awhereEnv75247') == FALSE) {
    stop('Please load credentials for aWhereAPI before continuing')
  } else if (appendPrevDataPull == TRUE) {
    #Check to see if there is an object in memory from previous data pull.  This
    #works because if the object is modified at all the mem reference will
    #change
    if (exists('memRef_generateaWhereDataset',where = awhereEnv75247,inherits = FALSE) == TRUE) {
      
      data.prevPull <-loadVarMemRef_aWhereEnv(memAddress = awhereEnv75247$memRef_generateaWhereDataset)
      LTNyears.prevPull <- awhereEnv75247$generateaWhereDataset.LTN_years
      
      if (is.null(data.prevPull) == TRUE) {
        prevDataPullInMemory <- FALSE
      } else {
        prevDataPullInMemory <- TRUE
      }
    } else {
      prevDataPullInMemory <- FALSE
    }
  } else {
    prevDataPullInMemory <- FALSE
  }
  
  #If there is a prev data pull in memoory, alter query to only fetch needed new data
  if (prevDataPullInMemory == TRUE) {
    #check to make sure they are pulling data for the same location
    lat.prev <- unique(data.prevPull[,latitude])
    lon.prev <- unique(data.prevPull[,longitude])
    
    
    if (length(lat.prev) == 1 & length(lon.prev) == 1 & 
        lat.prev == lat & lon.prev == lon &
        LTNyears.prevPull[1] == year_start & LTNyears.prevPull[2] == year_end) {
    
      dates.prev <- unique(data.prevPull[,date])
      dates.current <- seq.Date(as.Date(day_start),as.Date(day_end),by = 'days')
      
      dates.current <- as.Date(setdiff(dates.current,dates.prev),origin = '1970-01-01')
      
      if (length(dates.current) == 0) {
        #Save Memory Reference of Object to the aWhereEnv
        if (exists('memRef_generateaWhereDataset',where = awhereEnv75247,inherits = FALSE) == TRUE) {
          if (bindingIsLocked('memRef_generateaWhereDataset',awhereEnv75247) == TRUE) {
            unlockBinding('memRef_generateaWhereDataset',awhereEnv75247)
          }
        }
        
        #save the memory refrence
        awhereEnv75247$memRef_generateaWhereDataset <-  
          purrr::map_chr('data.prevPull', ~ do.call(pryr::address,list(rlang::sym(.x)))) 
        
        lockBinding('memRef_generateaWhereDataset'
                    ,awhereEnv75247)
        
        return(data.prevPull)
      } else {
        
        #check to make sure the user didn't just request a longer time period on
        #the front and back end.  If so just request the new data as its more
        #complicated to do the logic and it won't save API calls
        
        if (day_start < min(dates.prev) & day_end > max(dates.prev)) {
          cat(paste0('You have requested data that extends both earlier and later in time than previous data 
                      you have requested for the same location.  It would be more effecient to run this function twice,
                      once for the earlier dates and again for the later.'))
          makeAPICalls <- readline("Do you wish to proceed with the current request? Type yes to begin API calls: ")
          
          if (tolower(makeAPICalls) != 'yes') {
            stop('User Input indicated they did not want to proceed with making API Calls \n')
          }
        } else {
          if (verbose == TRUE) {
            cat(paste0('Appending newly requested data to previously pulled dataset for the same location.\n'))
          }
        }
      
        day_start <- dates.current[1]
        day_end <- dates.current[length(dates.current)]
      }
    }
  }
  
  day_start <- as.character(day_start)
  day_end <- as.character(day_end)
  
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
  
  if (verbose == TRUE) {
    cat(paste0('    Adjusting Query for differences in time zone between user and location being requested \n'))
  }
  
  
  if((day_start >= (Sys.Date()+2)) == TRUE) {
    onlyForecastRequested <- TRUE
  } else {
    dateToTest <- Sys.Date()  - 2 #this is one extra day of testing than should be necessary
    
    numTries <- 0
    repeatQuery <- TRUE
    while(repeatQuery == TRUE) {
  
      repeatQuery <- FALSE
      numTries <- numTries + 1
      
      temp <- tryCatch({
        
        aWhereAPI::forecasts_latlng(latitude = lat
                                    ,longitude = lon
                                    ,day_start = as.character(dateToTest)
                                    ,day_end = as.character(dateToTest)
                                    ,block_size = 24,)
        
      }, error = function(e) {
        
        if (grepl(pattern = 'Please use the observations API for past data.'
                  ,x = e
                  ,ignore.case = TRUE)) {
          repeatQuery <- TRUE
          dateToTest <- dateToTest+1
          return(list(repeatQuery,dateToTest))
        } else {
          stop(e) #print the error message and stop code
        }
      })
      #Because there is no explicit error object in the return, we will instead
      #test if the fxn returned a data.frame indicating the query worked
      if(is.data.frame(temp) == FALSE) {
        repeatQuery <- temp[[1]][1]
        dateToTest <- temp[[2]][1]
        
       # rm(temp)
      }
      
      if (numTries > 5) {
        stop('There is a problem with determining how to account for timezone differences\n
             between the user and aWhere\'s API.  Please review your settings to make sure nothing is wrong')
      }
    }
    
    firstForecastDay <- dateToTest
    
    if (firstForecastDay <= lubridate::ymd(day_start)) {
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
    
    numTries <- 0
    repeatQuery <- TRUE
    while(repeatQuery == TRUE) {
      repeatQuery <- FALSE
      numTries <- numTries + 1
      
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
      if (numTries > 5) {
        stop('There is a problem with determining how to account for timezone differences\n
             between the user and aWhere\'s API.  Please review your settings to make sure nothing is wrong')
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
    if (verbose == TRUE) {
      cat(paste0('    Requesting observed weather data \n'))
    }
    
    
    obs <- suppressWarnings(aWhereAPI::daily_observed_latlng(lat
                                                             ,lon
                                                             ,day_start = day_start
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
                      ,day = '0'
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
  
  setkey(obs,date)
###############################################################################
#Get observed Agronomics data
##############################################################################

  if (verbose == TRUE) {
    cat(paste0('    Requesting observed agronomics data \n'))
  }
  
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
  
  setkey(ag,date)

###############################################################################
#Get LTN Weather data
##############################################################################
  
  #Allow the request of LTN data to be bypassed
  if (is.null(year_start) == FALSE & is.null(year_end) == FALSE) {
    
    if (verbose == TRUE) {
      cat(paste0('    Requesting LTN weather data \n'))
    }
    
    obs_ltn <- suppressWarnings(aWhereAPI::weather_norms_latlng(latitude = lat
                                                                ,longitude = lon
                                                                ,monthday_start = monthday_start
                                                                ,monthday_end = monthday_end
                                                                ,year_start = year_start
                                                                ,year_end = year_end
                                                                ,includeFeb29thData = TRUE)) %>%
      data.table::as.data.table(.)
  
    obs_ltn.names <- colnames(obs_ltn)
    
    #add prefix to indicate this is historical data
    setnames(obs_ltn
             ,setdiff(obs_ltn.names,colsNoChange)
             ,paste0('ltn.',setdiff(obs_ltn.names,colsNoChange)))
    
    setkey(obs_ltn,day)
    
  ###############################################################################
  #Get LTN Agronomics data
  ##############################################################################
    
    if (verbose == TRUE) {
      cat(paste0('    Requesting LTN agronomics data \n'))
    }
    
    ag_ltn <- suppressWarnings(aWhereAPI::agronomic_norms_latlng(latitude = lat
                                                                 ,longitude = lon
                                                                 ,month_day_start = monthday_start
                                                                 ,month_day_end = monthday_end
                                                                 ,year_start = year_start
                                                                 ,year_end = year_end
                                                                 ,includeFeb29thData = TRUE)) %>%
      data.table::as.data.table(.)
    
    ag_ltn.names <- colnames(ag_ltn)
    
    #add prefix to indicate this is historical data
    setnames(ag_ltn
             ,setdiff(ag_ltn.names,colsNoChange)
             ,paste0('ltn.',setdiff(ag_ltn.names,colsNoChange)))
    
    setkey(ag_ltn,day)
  } else {
    obs_ltn <- 
      data.frame(latitude = 0
                ,longitude = 0
                ,day = '0'
                ,ltn.meanTemp.average = 0
                ,ltn.meanTemp.stdDev = 0
                ,ltn.maxTemp.average = 0
                ,ltn.maxTemp.stdDev = 0
                ,ltn.minTemp.average = 0
                ,ltn.minTemp.stdDev = 0
                ,ltn.precipitation.average = 0
                ,ltn.precipitation.stdDev = 0
                ,ltn.solar.average = 0
                ,ltn.solar.stdDev = 0
                ,ltn.minHumidity.average = 0
                ,ltn.minHumidity.stdDev = 0
                ,ltn.maxHumidity.average = 0
                ,ltn.maxHumidity.stdDev = 0
                ,ltn.dailyMaxWind.average = 0
                ,ltn.dailyMaxWind.stdDev = 0
                ,ltn.averageWind.average = 0
                ,ltn.averageWind.stdDev = 0)
    
    obs_ltn <- obs_ltn[0,]
    
    ag_ltn <- 
      data.frame(latitude = 0
                 ,longitude = 0
                 ,day = '0'
                 ,ltn.gdd.average = 0 
                 ,ltn.gdd.stdDev = 0
                 ,ltn.pet.average = 0
                 ,ltn.pet.stdDev = 0
                 ,ltn.ppet.average = 0
                 ,ltn.ppet.stdDev = 0
                 ,ltn.accumulatedGdd.average = 0
                 ,ltn.accumulatedGdd.stdDev = 0
                 ,ltn.accumulatedPrecipitation.average = 0
                 ,ltn.accumulatedPrecipitation.stdDev = 0
                 ,ltn.accumulatedPet.average = 0
                 ,ltn.accumulatedPet.stdDev = 0
                 ,ltn.accumulatedPpet.average = 0
                 ,ltn.accumulatedPpet.stdDev = 0)  
    
    ag_ltn <- ag_ltn[0,]
  }
###############################################################################
#Get forecast data
##############################################################################
  #pull forecasted data for determined time period We are using what is
  #determined above to be the last valid day of historical data at this
  #location at the time the API call is made and incrementing by one day to
  #get the date range fo rthis call
  
  if (firstForecastDay <= day_end) {
    
    if (verbose == TRUE) {
      cat(paste0('    Requested forecast weather data \n'))
    }
    
    forecast <- aWhereAPI::forecasts_latlng(lat
                                            ,lon
                                            ,day_start = as.character(firstForecastDay)
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
                      ,date = lubridate::ymd(Sys.Date())
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
                ,by = c('latitude','longitude','day')
                ,all.x = TRUE)
    
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
                ,by = c('latitude','longitude','day')
                ,all.x = TRUE)
    
    ag <- merge(ag
                ,obs
                ,by = c('latitude','longitude','day','date')
                ,all.x = TRUE)
    
    ag[is.na(obs.gdd),        obs.gdd        := ltn.gdd.average]
    ag[is.na(obs.ppet),       obs.ppet       := ltn.ppet.average]
    ag[is.na(obs.pet.amount), obs.pet.amount := ltn.pet.average]
    
    ag[,obs.accumulatedGdd                  := cumsum(obs.gdd)]
    ag[,obs.accumulatedPpet                 := cumsum(obs.ppet)]
    ag[,obs.accumulatedPrecipitation.amount := cumsum(obs.precipitation.amount)]
    ag[,obs.accumulatedPet.amount           := cumsum(obs.pet.amount)]
    
    ag <- ag[,ag.names,with = FALSE]
    
    setkey(ag,date)
  } 
  
###############################################################################
#Assemble final dataset
##############################################################################

  
  if (verbose == TRUE) {
    cat(paste0('    Combining together data\n'))
  }
  
  weather_full <- merge(obs,          ag,       by = c("date", "day", "latitude", "longitude"), all = TRUE)
  weather_full <- merge(weather_full, forecast, by = c("date", "day", "latitude", "longitude"), all = TRUE)
  weather_full <- merge(weather_full, obs_ltn,  by = c("day", "latitude", "longitude"), all.x = TRUE)
  weather_full <- merge(weather_full, ag_ltn,   by = c("day", "latitude", "longitude"), all.x = TRUE)
  
  setkey(weather_full,date)
  
  weather_full.names <- unique(c(colnames(obs), colnames(ag), colnames(obs_ltn), colnames(ag_ltn)))
  
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
                                                   ,'ltn.temperatures.min.stdDev'
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
  
  #APPEND IN THE DATA FROM THE PREVIOUS PULL
  if (prevDataPullInMemory == TRUE) {
    weather_full <- rbind(weather_full
                          ,data.prevPull
                          ,use.names = TRUE)
    
    setkey(weather_full,date)
    weather_full <- unique(weather_full, by = 'date')
    
    #These calcs need to be redone because the data could have changed
    weather_full[,accumulatedGdd.amount           := cumsum(gdd.amount)]
    weather_full[,accumulatedPpet.amount          := cumsum(ppet.amount)]
    weather_full[,accumulatedPrecipitation.amount := cumsum(precipitation.amount)]
    weather_full[,accumulatedPet.amount           := cumsum(pet.amount)]
  }
  
  #For queries that go for more than a year the only way to get accumulated
  #amounts correct is to join back to the original datasets because the API
  #can't return the correct data
  weather_full[,accumulatedPrecipitation.average := cumsum(precipitation.average)]
  weather_full[,accumulatedGdd.average := cumsum(gdd.average)]
  weather_full[,accumulatedPpet.average := cumsum(ppet.average)]
  weather_full[,accumulatedPet.average := cumsum(pet.average)]
  
  #we need to do this calculation manually to get reasonable stdDev values
  weather_full[,daily.accumulatedPrecipitation.stdDev := accumulatedPrecipitation.stdDev - shift(accumulatedPrecipitation.stdDev,n = 1,fill = 0)]
  weather_full[,daily.accumulatedPet.stdDev := accumulatedPet.stdDev - shift(accumulatedPet.stdDev,n = 1, fill = 0)]
  weather_full[,daily.accumulatedPpet.stdDev := accumulatedPpet.stdDev- shift(accumulatedPpet.stdDev,n = 1, fill = 0)]
  weather_full[,daily.accumulatedGdd.stdDev := accumulatedGdd.stdDev- shift(accumulatedGdd.stdDev,n = 1,fill = 0)]
  
  weather_full[daily.accumulatedPrecipitation.stdDev < 0, daily.accumulatedPrecipitation.stdDev := 0]
  weather_full[daily.accumulatedPet.stdDev < 0, daily.accumulatedPet.stdDev := 0]
  weather_full[daily.accumulatedPpet.stdDev < 0, daily.accumulatedPpet.stdDev := 0]
  weather_full[daily.accumulatedGdd.stdDev < 0, daily.accumulatedGdd.stdDev := 0]
  
  weather_full[,accumulatedPrecipitation.stdDev := cumsum(daily.accumulatedPrecipitation.stdDev)]
  weather_full[,accumulatedPet.stdDev := cumsum(daily.accumulatedPet.stdDev)]
  weather_full[,accumulatedPpet.stdDev := cumsum(daily.accumulatedPpet.stdDev)]
  weather_full[,accumulatedGdd.stdDev:= cumsum(daily.accumulatedGdd.stdDev)]

  weather_full[,c('daily.accumulatedPrecipitation.stdDev'
                  ,'daily.accumulatedPet.stdDev'
                  ,'daily.accumulatedPpet.stdDev'
                  ,'daily.accumulatedGdd.stdDev') := NULL]
  
  if (removeFeb29Data == TRUE) {
    weather_full <- weather_full[day != '02-29',]
  }
  
  
  if (verbose == TRUE) {
    cat(paste0('    Process Complete \n\n'))
  }
  
  #Save Memory Reference of Object to the aWhereEnv
  if (exists('memRef_generateaWhereDataset',where = awhereEnv75247,inherits = FALSE) == TRUE) {
    if (bindingIsLocked('memRef_generateaWhereDataset',awhereEnv75247) == TRUE) {
      unlockBinding('memRef_generateaWhereDataset',awhereEnv75247)
    }
  }
  
  if (exists('generateaWhereDataset.LTN_years',where = awhereEnv75247,inherits = FALSE) == TRUE) {
    if (bindingIsLocked('generateaWhereDataset.LTN_years',awhereEnv75247) == TRUE) {
      unlockBinding('generateaWhereDataset.LTN_years',awhereEnv75247)
    }
  }
  
  awhereEnv75247$memRef_generateaWhereDataset <-  
    purrr::map_chr('weather_full', ~ do.call(pryr::address,list(rlang::sym(.x)))) 
  
  awhereEnv75247$generateaWhereDataset.LTN_years <- c(year_start,year_end)  
  
  lockBinding('memRef_generateaWhereDataset',awhereEnv75247)
  lockBinding('generateaWhereDataset.LTN_years',awhereEnv75247)
  
  if (is.null(year_start) == FALSE & is.null(year_end) == FALSE) {
    return(weather_full)
  } else {
    return(weather_full[,c(colsNoChange,grep(pattern = '.amount',x = colnames(weather_full),value = TRUE)),with = FALSE])
  }
}
