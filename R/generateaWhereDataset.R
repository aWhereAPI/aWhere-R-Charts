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
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{generateaWhereDataset(lat = 30.685
#'                               ,lon = 72.928
#'                               ,day_start = "2017-01-01"
#'                               ,day_end = "2017-06-01"
#'                               ,year_start = 2008
#'                               ,year_end = 2017
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
      dplyr::select(-c(X1,X2,X3))
  
    #simplify column names
    names(obs)[grep("temperatures.max", names(obs))] <- "maxTemp"
    names(obs)[grep("temperatures.min", names(obs))] <- "minTemp"
    
    
  } else {
    #based on the code below we need to have a data.frame with this name that
    #has at least the columns date and day for the merge to be successful.  The
    #merge is doing an outer join
    obs <- data.frame(date = Sys.Date(),day = 0,maxTemp = 0, minTemp = 0, precipitation.amount = 0)
    obs <- obs[0,]
  }

  #pull agronomic data for time period
  ##This works becayse the Ag endpoint includes forecast data
  ag <- suppressWarnings(aWhereAPI::agronomic_values_latlng(lat, lon, day_start, day_end)) %>%
    cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>%
    mutate(day = paste0(X2, "-", X3)) %>%
    dplyr::select(-c(X1,X2,X3))
  
  
  #pull LTN observed weather
  obs_ltn <- aWhereAPI::weather_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end,includeFeb29thData = FALSE)

  #pull LTN agronomic
  ag_ltn <- aWhereAPI::agronomic_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end,includeFeb29thData = FALSE)

  #REPLACE ANY MISSING DATA WITH LTN VALUES
  if(all(complete.cases(obs) == TRUE) == FALSE) {
    
    obs <- merge(obs
                ,subset(obs_ltn
                       ,select = c('latitude'
                                  ,'longitude'
                                  ,'day'
                                  ,'maxTemp.average'
                                  ,'minTemp.average'
                                  ,'precipitation.average'
                                  ,'solar.average'
                                  ,'minHumidity.average'
                                  ,'maxHumidity.average'
                                  ,'dailyMaxWind.average'
                                  ,'averageWind.average'
                                  )
                       )
                ,by = c('latitude','longitude','day'))
          
    
    obs$maxTemp[is.na(obs$maxTemp)] <- obs$maxTemp.average[is.na(obs$maxTemp)]
    obs$minTemp[is.na(obs$minTemp)] <- obs$minTemp.average[is.na(obs$minTemp)]
    obs$precipitation.amount[is.na(obs$precipitation.amount)] <- obs$precipitation.average[is.na(obs$precipitation.amount)]
    obs$solar.amount[is.na(obs$solar.amount)] <- obs$solar.average[is.na(obs$solar.amount)]
    obs$relativeHumidity.max[is.na(obs$relativeHumidity.max)] <- obs$maxHumidity.average[is.na(obs$relativeHumidity.max)]
    obs$relativeHumidity.min[is.na(obs$relativeHumidity.min)] <- obs$minHumidity.average[is.na(obs$relativeHumidity.min)]
    obs$wind.morningMax[is.na(obs$wind.morningMax)] <- obs$dailyMaxWind.average[is.na(obs$wind.morningMax)]
    obs$wind.dayMax[is.na(obs$wind.dayMax)] <- obs$dailyMaxWind.average[is.na(obs$wind.dayMax)]
    obs$wind.average[is.na(obs$wind.average)] <- obs$averageWind.average[is.na(obs$wind.average)]
    
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
                 ,subset(ag_ltn
                         ,select = c('latitude'
                                     ,'longitude'
                                     ,'day'
                                     ,'gdd.average'
                                     ,'ppet.average'
                                     ,'pet.average'
                                     )
                         )
                 ,by = c('latitude','longitude','day'))
    
    
    ag$gdd[is.na(ag$gdd)]   <- ag$gdd.average[is.na(ag$gdd)]
    ag$ppet[is.na(ag$ppet)] <- ag$ppet.average[is.na(ag$ppet)]
    ag$pet[is.na(ag$pet.amount)]   <- ag$pet.average[is.na(ag$pet.amount)]

    ag$accumulatedGdd <- cumsum(ag$gdd)
    ag$accumulatedPpet <- cumsum(ag$ppet)
    ag$accumulatedPet.amount <- cumsum(ag$pet.amount)
    
    ag <- subset(ag
                ,select = -c(gdd.average
                             ,ppet.average
                             ,pet.average
                             )
                )
  } 
  
  

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
      dplyr::select(-c(X1,X2,X3))

    #set precipitation name to delineate from observed
    names(forecast)[grep("precipitation.amount", names(forecast))] <- "precipitation.forecast"

    #create final dataset

    weather_full <- merge(obs, ag, by = c("date", "day", "latitude", "longitude"), all = TRUE) %>%
      merge(., forecast, by = c("date", "day", "latitude", "longitude"), all = TRUE) %>%
      merge(., obs_ltn, by = c("day", "latitude", "longitude"))  %>%
      merge(., ag_ltn,  by = c("day", "latitude", "longitude")) %>%
      .[order(.$date),] %>%
      dplyr::mutate(maxTemp = ifelse(!is.na(maxTemp), maxTemp, temperatures.max),
                    minTemp = ifelse(!is.na(minTemp), minTemp, temperatures.min),
                    precipitation.amount = ifelse(!is.na(precipitation.amount), precipitation.amount, precipitation.forecast),
                    accumulatedPrecipitation.amount = cumsum(precipitation.amount),
                    accumulatedPrecipitation.average = cumsum(precipitation.average),
                    accumulatedPet.average = cumsum(pet.average),
                    accumulatedPpet.average = cumsum(ppet.average))


  }

  weather_full <- weather_full %>%
    dplyr::select(day, date,
                  maxTemp.amount = maxTemp, maxTemp.average, maxTemp.stdDev,
                  minTemp.amount = minTemp, minTemp.average, minTemp.stdDev,
                  precipitation.amount, precipitation.average, precipitation.stdDev,
                  accumulatedPrecipitation.amount, accumulatedPrecipitation.average, accumulatedPrecipitation.stdDev,
                  gdd.amount = gdd, gdd.average, gdd.stdDev,
                  pet.amount, pet.average, pet.stdDev,
                  accumulatedPet.amount, accumulatedPet.average, accumulatedPet.stdDev,
                  ppet.amount = ppet, ppet.average, ppet.stdDev,
                  accumulatedPpet.amount = accumulatedPpet, accumulatedPpet.average, accumulatedPpet.stdDev) %>%
    dplyr::mutate(latitude = lat,
                  longitude = lon)

  return(weather_full)
}
