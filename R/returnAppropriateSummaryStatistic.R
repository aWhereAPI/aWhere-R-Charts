#' @title returnAppropriateSummaryStatistic
#'
#' @description \code{returnAppropriateSummaryStatistic} Internal function to return
#' appropriate summary statistic to use with each index
#'
#' @details Internal function to return
#' appropriate summary statistic to use with each index
#'
#' @param variable character string denoting the variable to chart. Acceptable
#'   values are maxLenDrySpell, maxLenWetSpell, numFrostDays, numSummerDays, numIcingDays,
#'   numTropicalNights, minOfMaxTemp, maxOfMaxTemp, minOfMinTemp, maxOfMinTemp,
#'   dailyTempRange, maxSingleDayPrecip, max5ConsDayPrecip, simplePrecipIntensityIndex,
#'   precipSumExceedPercentile, warmSpellDurIndex, coldSpellDurIndex,
#'   countDaysPrecipExceedAmount, percentDaysMinTempBelowQuantile, percentDaysMaxTempBelowQuantile,
#'   percentDaysMinTempAboveQuantile, percentDaysMaxTempAboveQuantile, maxOfAccumulatedGdd,
#'   maxOfAccumulatedPet, sumOfGDD, sumOfPET, sumOfPrecip, sumOfPOverPET, sumOfSolar, averageMaxTemp, 
#'   averageMinTemp, averageMaxRH, averageMinRH, averageWind, and maxWindGusts
#'  
#' @return string
#'

returnAppropriateSummaryStatistic <- function(variable) {
  
  if (grepl(pattern = 'maxLenDrySpell|Maximum Length of Dry Spell|maxLenWetSpell|Maximum Length of Wet Spell'
            ,x = variable
            ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'numFrostDays|Number of Frost Days'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'numSummerDays|Number of Summer Days'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'numIcingDays|Number of Icing Days'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'numTropicalNights|Number of Tropical Nights'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'minOfMaxTemp|Minimum of Maximum Temperature'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'min'
    
  } else if (grepl(pattern = 'maxOfMaxTemp|Maximum of Maximum Temperature'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'minOfMinTemp|Minimum of Minimum Temperature'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'min'
    
  } else if (grepl(pattern = 'maxOfMinTemp|Maximum of Minimum Temperature'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'dailyTempRange|Daily Temperature Range'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'maxSingleDayPrecip|Maximum of Single Day Precipitation'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'max5ConsDayPrecip|Maximum of Five Consecutive Day Precipitation'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'seasonTotalPrecip|Seasonal Total Precipitation'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'simplePrecipIntensityIndex|Simple Precipitation Intensity Index'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'warmSpellDurIndex|Warm Spell Duration Index|coldSpellDurIndex|Cold Spell Duration Index'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'precipSumExceedPercentile|Sum of Precipitation when Precipitation Exceeding Quantile'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'countDaysPrecipExceedAmount|Count of Days When Precipitation Exceeding Thresshold'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'percentDaysMinTempBelowQuantile|Percentage of Days Minimum Temperature Below Quantile|percentDaysMaxTempBelowQuantile|Percentage of Days Maximum Temperature Below Quantile|percentDaysMinTempAboveQuantile|Percentage of Days Minimum Temperature Above Quantile|percentDaysMaxTempAboveQuantile|Percentage of Days Maximum Temperature Above Quantile'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'sumOf|maxWindGust'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'averageMax|averageMin|averageWind|wind.average'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'mean'
    
  } else if (grepl(pattern = 'maxOfAccumulated'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  } else if (grepl(pattern = 'gdd|pet|ppet|precipitation'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'sum' 
    
  } else if (grepl(pattern = 'relativeHumidity.max|solar|temperatures.max|wind.dayMax'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max' 
      
  } else if (grepl(pattern = 'relativeHumidity.min|temperatures.min'
                  ,x = variable
                  ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'min'
      
  } else if (grepl(pattern = 'accumulated'
                  ,x = variable
                  ,ignore.case = TRUE) == TRUE)  {
    
    summaryStatistic.use <- 'max'
    
  }else {
    stop('Need to define methods for summarizing this index\n')
  }
  
  return(summaryStatistic.use)
}