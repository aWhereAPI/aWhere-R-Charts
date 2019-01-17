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
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{generateaWhereDataset(lat = 30.685
#'                               ,lon = 72.928
#'                               ,day_start = "2018-09-01"
#'                               ,day_end = "2019-01-17"
#'                               ,year_start = 2008
#'                               ,year_end = 2018
#'                               )
#'         }

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


  #pull daily weather data for determined time period
  if (onlyForecastRequested == FALSE) {
    obs <- suppressWarnings(aWhereAPI::daily_observed_latlng(lat
                                                             ,lon
                                                             ,day_start
                                                             ,day_end = as.character(interim_day_end))) %>%
      cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>%
      mutate(day = paste0(X2, "-", X3))%>%
      dplyr::select(-c(X1,X2,X3)) %>%
      data.table::as.data.table(.)
  
    #simplify column names
    data.table::setnames(obs,c('temperatures.max'
                               ,'temperatures.min'
                               ,'relativeHumidity.max'
                               ,'relativeHumidity.min'
                               ,'wind.morningMax'
                               ,'wind.dayMax'
                               ,'wind.average'), c('maxTemp'
                                                   ,'minTemp'
                                                   ,'maxRH'
                                                   ,'minRH'
                                                   ,'maxMorningWind'
                                                   ,'maxWind'
                                                   ,'averageWind'))
    
    
    
  } else {
    #based on the code below we need to have a data.frame with this name that
    #has at least the columns date and day for the merge to be successful.  The
    #merge is doing an outer join
    obs <- data.frame(date = Sys.Date()
                      ,day = 0
                      ,maxTemp = 0
                      ,minTemp = 0
                      ,precipitation.amount = 0
                      ,solar.amount = 0
                      ,maxRH = 0
                      ,minRH = 0
                      ,maxMorningWind = 0
                      ,maxWind = 0
                      ,averageWind = 0)
    obs <- obs[0,]
  }

  #pull agronomic data for time period
  ##This works becayse the Ag endpoint includes forecast data
  ag <- suppressWarnings(aWhereAPI::agronomic_values_latlng(latitude = lat
                                                            ,longitude = lon
                                                            ,day_start = day_start
                                                            ,day_end = day_end)) %>%
    cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>%
    mutate(day = paste0(X2, "-", X3)) %>%
    dplyr::select(-c(X1,X2,X3)) %>%
    data.table::as.data.table(.)
  
  
  #pull LTN observed weather
  obs_ltn <- suppressWarnings(aWhereAPI::weather_norms_latlng(latitude = lat
                                                              ,longitude = lon
                                                              ,monthday_start = monthday_start
                                                              ,monthday_end = monthday_end
                                                              ,year_start = year_start
                                                              ,year_end = year_end
                                                              ,includeFeb29thData = FALSE)) %>%
    data.table::as.data.table(.)

  #pull LTN agronomic
  ag_ltn <- suppressWarnings(aWhereAPI::agronomic_norms_latlng(latitude = lat
                                                               ,longitude = lon
                                                               ,month_day_start = monthday_start
                                                               ,month_day_end = monthday_end
                                                               ,year_start = year_start
                                                               ,year_end = year_end
                                                               ,includeFeb29thData = FALSE)) %>%
    data.table::as.data.table(.)

  #REPLACE ANY MISSING DATA WITH LTN VALUES
  if(all(complete.cases(obs) == TRUE) == FALSE) {
    
    obs <- merge(obs
                ,obs_ltn[,list(latitude
                              ,longitude
                              ,day
                              ,maxTemp.average
                              ,minTemp.average
                              ,precipitation.average
                              ,solar.average
                              ,minHumidity.average
                              ,maxHumidity.average
                              ,dailyMaxWind.average
                              ,averageWind.average
                              )
                         ]
                ,by = c('latitude','longitude','day'))
          
    
    obs[is.na(maxTemp), maxTemp := maxTemp.average]
    obs[is.na(minTemp), minTemp := minTemp.average]
    obs[is.na(precipitation.amount), precipitation.amount := precipitation.average]
    obs[is.na(solar.amount), solar.amount := solar.average]
    obs[is.na(maxRH), maxRH := maxHumidity.average]
    obs[is.na(minRH), minRH := minHumidity.average]
    obs[is.na(maxMorningWind), maxMorningWind := dailyMaxWind.average]
    obs[is.na(maxWind), maxWind := dailyMaxWind.average]
    obs[is.na(averageWind), averageWind := averageWind.average]
    
    obs <- subset(obs
                 ,select = -c(maxTemp.average
                               ,minTemp.average
                               ,precipitation.average
                               ,solar.average
                               ,minHumidity.average
                               ,maxHumidity.average
                               ,dailyMaxWind.average
                               ,averageWind.average)
                 )
  } 
  
  if(all(complete.cases(ag) == TRUE) == FALSE) {
    
    ag <- merge(ag
                 ,ag_ltn[, list(latitude
                               ,longitude
                               ,day
                               ,gdd.average
                               ,ppet.average
                               ,pet.average)]
                 ,by = c('latitude','longitude','day'))
    ag <- merge(ag
                ,obs[,list(latitude
                           ,longitude
                           ,day
                           ,precipitation.amount)])
    
    ag[is.na(gdd), gdd := gdd.average]
    ag[is.na(ppet), ppet := ppet.average]
    ag[is.na(pet.amount), pet.amount := pet.average]
    
    ag[,accumulatedGdd := cumsum(gdd)]
    ag[,accumulatedPpet := cumsum(ppet)]
    ag[,accumulatedPrecipitation.amount := cumsum(precipitation.amount)]
    ag[,accumulatedPet.amount := cumsum(pet.amount)]
    
    
    
    ag <- subset(ag
                ,select = -c(gdd.average
                             ,ppet.average
                             ,pet.average
                             ,precipitation.amount))
  } 
  
  
  #If no forecast data is needed
  if((day_end <= (Sys.Date()-1)) == TRUE) {

    #create final dataset
      weather_full <- merge(obs, ag, by = c("date", "day","latitude", "longitude"), all = TRUE) %>%
        merge(., obs_ltn,            by = c("day","latitude", "longitude")) %>%
        merge(., ag_ltn,             by = c("day","latitude", "longitude"))   %>%
        .[order(.$date),] %>%
        dplyr::mutate(accumulatedPrecipitation.average = cumsum(precipitation.average),
                      accumulatedPet.average = cumsum(pet.average),
                      accumulatedPpet.average = cumsum(ppet.average))


  } else {

    #pull forecasted data for determined time period We are using what is
    #determined above to be the last valid day of historical data at this
    #location at the time the API call is made and incrementing by one day to
    #get the date range fo rthis call
    forecast <- aWhereAPI::forecasts_latlng(lat
                                            ,lon
                                            ,day_start = as.character(interim_day_end + 1)
                                            ,day_end = day_end
                                            ,block_size = 24) %>%
      mutate(date = substr(.$startTime, 1, 10)) %>%
      cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>%
      #create month-day combo column
      mutate(day = paste0(X2, "-", X3))%>%
      dplyr::select(-c(X1,X2,X3)) %>%
      as.data.table(.)

    #set precipitation name to delineate from observed
    setnames(forecast,c('precipitation.amount'
                        ,'solar.amount'), c('precipitation.forecast'
                                            ,'solar.forecast'))
    

    #create final dataset

    weather_full <- merge(obs, ag, by = c("date", "day", "latitude", "longitude"), all = TRUE) %>%
      merge(., forecast, by = c("date", "day", "latitude", "longitude"), all = TRUE) %>%
      merge(., obs_ltn, by = c("day", "latitude", "longitude"))  %>%
      merge(., ag_ltn,  by = c("day", "latitude", "longitude")) %>%
      .[order(.$date),] %>%
      dplyr::mutate(maxTemp.amount        = ifelse(!is.na(maxTemp),              maxTemp, temperatures.max)
                    ,minTemp.amount       = ifelse(!is.na(minTemp),              minTemp, temperatures.min)
                    ,precipitation.amount = ifelse(!is.na(precipitation.amount), precipitation.amount, precipitation.forecast)
                    ,solar.amount         = ifelse(!is.na(solar.amount),         solar.amount, solar.forecast)
                    ,maxRH                = ifelse(!is.na(maxRH), maxRH,         relativeHumidity.max)
                    ,minRH                = ifelse(!is.na(minRH), minRH,         relativeHumidity.min)
                    ,maxMorningWind       = ifelse(!is.na(maxMorningWind),       maxMorningWind,wind.max)
                    ,maxWind              = ifelse(!is.na(maxWind), maxWind,     wind.max)
                    ,averageWind          = ifelse(!is.na(averageWind),          averageWind, wind.average)
                    ,accumulatedPrecipitation.amount = cumsum(precipitation.amount)
                    ,accumulatedPrecipitation.average = cumsum(precipitation.average)
                    ,accumulatedPet.average = cumsum(pet.average)
                    ,accumulatedPpet.average = cumsum(ppet.average)) %>%
      dplyr::select(-startTime
                    ,-endTime
                    ,-conditionsCode
                    ,-conditionsText
                    ,-temperatures.max
                    ,-temperatures.min
                    ,-precipitation.chance
                    ,-precipitation.forecast
                    ,-sky.cloudCover
                    ,-sky.sunshine
                    ,-solar.forecast
                    ,-relativeHumidity.average
                    ,-relativeHumidity.max
                    ,-relativeHumidity.min
                    ,-wind.average
                    ,-wind.max
                    ,-wind.min
                    ,-dewPoint.amount) %>%
      as.data.table(.)


  }

  weather_full <- weather_full %>%
    dplyr::select(day
                  ,date
                  ,maxTemp.amount = maxTemp
                  ,maxTemp.average
                  ,maxTemp.stdDev
                  ,minTemp.amount = minTemp
                  ,minTemp.average
                  ,minTemp.stdDev
                  ,precipitation.amount
                  ,precipitation.average
                  ,precipitation.stdDev
                  ,accumulatedPrecipitation.amount
                  ,accumulatedPrecipitation.average
                  ,accumulatedPrecipitation.stdDev
                  ,gdd.amount = gdd
                  ,gdd.average
                  ,gdd.stdDev
                  ,pet.amount
                  ,pet.average
                  ,pet.stdDev
                  ,accumulatedPet.amount
                  ,accumulatedPet.average
                  ,accumulatedPet.stdDev
                  ,ppet.amount = ppet
                  ,ppet.average
                  ,ppet.stdDev
                  ,accumulatedPpet.amount = accumulatedPpet
                  ,accumulatedPpet.average
                  ,accumulatedPpet.stdDev) %>%
    dplyr::mutate(latitude = lat,
                  longitude = lon)

  return(weather_full)
}
