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
#' \dontrun{generateaWhereDataset(lat = 30.685, lon = 72.928, day_start = "2017-01-01",
#'                                day_end = "2017-06-01", year_start = 2008, year_end = 2017)}

#' @export

generateaWhereDataset <- function(lat
                                  ,lon
                                  ,day_start
                                  ,day_end
                                  ,year_start
                                  ,year_end) {

  #Determine if only forecast data is requested
  if (day_start >= Sys.Date()) {
    onlyForecastRequested <- TRUE
  } else {
    onlyForecastRequested <- FALSE
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
  if((day_end <= (Sys.Date()-1)) == TRUE) {
    interim_day_end <- day_end
  } else {
    interim_day_end <- as.character((Sys.Date()-1))
  }

  #pull daily weather data for determined time period
  
  if (onlyForecastRequested == FALSE) {
    obs <- aWhereAPI::daily_observed_latlng(lat, lon, day_start, day_end = interim_day_end) %>%
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
  ag <- aWhereAPI::agronomic_values_latlng(lat, lon, day_start, day_end) %>%
    cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>%
    mutate(day = paste0(X2, "-", X3)) %>%
    dplyr::select(-c(X1,X2,X3))
  
  #pull LTN observed weather
  obs_ltn <- aWhereAPI::weather_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end,includeFeb29thData = FALSE)

  #pull LTN agronomic
  ag_ltn <- aWhereAPI::agronomic_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end,includeFeb29thData = FALSE)


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

    #pull forecasted data for determined time period
    forecast <- aWhereAPI::forecasts_latlng(lat, lon, day_end = day_end, block_size = 24) %>%
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
