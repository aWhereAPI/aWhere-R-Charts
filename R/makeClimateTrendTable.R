#' @title makeClimateTrendTable
#'
#' @description \code{makeClimateTrendTable} Generate a table across multiple growing seasons
#' using aWhere weather data with standardized formatting of a prefedined set of climate indices
#'
#' @details This function extends the aWhere charts package to make a table with the ETCCDI climate
#' change indices (reference below).  Most indices are implemented with the default behavior 
#' described int he publication.  Default values of the indices can be overriden using the 
#' "indexSpecificValue" parameter. Applies a summary statistic over each year/season of data
#' 
#' Also has helper functions including to automatically (with user permission) download missing
#' but needed data and store in the aWhereEnv to persist it throughout a user R session.
#'
#' @references http://etccdi.pacificclimate.org/list_27_indices.shtml
#'
#' @param data data frame in which variables are named according to the schema
#'   output by generateaWhereDataset.R (required)
#' @param filename filename for output image (required)   
#' @param variable character string denoting the variable to chart. Acceptable
#'   values are maxLenDrySpell, maxLenWetSpell, numFrostDays, numSummerDays, numIcingDays,
#'   numTropicalNights, minOfMaxTemp, maxOfMaxTemp, minOfMinTemp, maxOfMinTemp,
#'   dailyTempRange, maxSingleDayPrecip, max5ConsDayPrecip, simplePrecipIntensityIndex,
#'   precipSumExceedPercentile, warmSpellDurIndex, coldSpellDurIndex,
#'   countDaysPrecipExceedAmount, percentDaysMinTempBelowQuantile, percentDaysMaxTempBelowQuantile,
#'   percentDaysMinTempAboveQuantile, percentDaysMaxTempAboveQuantileaccumulatedGdd as well as 
#'   accumulatedPet, accumulatedPpet,accumulatedPrecipitation, gdd, pet, precipitation, maxRH, minRH,
#'   solar,averageWind,dayMaxWind, rollingavgppet, maxTemp, minTemp, dayMaxWind, averageWind
#' @param season.monthDay_start Specify the start month-day combination of the "season"
#'   you want analyzed (optional)
#' @param season.monthDay_end Specify the end month-day combination of the "season"
#'   you want analyzed (optional)
#' @param years.LTN specify the years over which you want the LTN calculated.  Defaults
#'   to 2006-2020 (optional)   
#' @param title character string of title to assign to the plot. (optional)
#' @param e_precip logical, if set to TRUE, effective precipitation will be
#'   calculated and charted based on e_threshold. Default is set to FALSE.
#'   (optional)
#' @param e_threshold numeric value (in milimeters) for the daily maximum used
#'   to calculate effective precipitation if e_precip is set to TRUE. (optional)
#' @param doRoll apply a rolling average to the calculation.
#' @param rolling_window numeric value for the number of days to use in rolling
#'   average calculations.  Default value is 30. (optional)
#' @param indexSpecificValue For the Climate Indices this tool can plot the user
#'   can override the default value of the index using this parameter (optional)
#'
#' @import dplyr
#' @import data.table
#' @import kableExtra
#' @import webshot
#'
#' @return plot object
#'
#' @examples
#' \dontrun{makeClimateTrendTable(data = weather_df
#'                                ,variable = "seasonTotalPrecip"
#'                                ,startYearOfSeasonToPlot = 2020
#'                                ,season.monthDay_start = '09-01'
#'                                ,season.monthDay_end = '11-30'
#'                                ,years.LTN = seq(2010,2019,1)
#'                                ,e_precip = TRUE
#'                                ,e_threshold = 10
#'                                ,doRoll = TRUE)}
#'
#' @export

makeClimateTrendTable <- function(data
                                  ,filename
                                  ,variable
                                  ,season.monthDay_start = '01-01'
                                  ,season.monthDay_end = '12-31'
                                  ,years.LTN = seq(2006,2020,1)
                                  ,title = NULL
                                  ,e_precip = FALSE 
                                  ,e_threshold = 35 
                                  ,indexSpecificValue = NULL) {
  
  if (webshot::is_phantomjs_installed() == FALSE) {
    cat('PhantomJS is not installed.  This must be installed before the tables can be output\n')
    webshot::install_phantomjs()
  }

  #because we are going to change the datastructure and it is a data.table we
  #will explicitly copy what is passed in so it doesn't violate user's scoping
  #expectations 
  dataToUse <- data.table::as.data.table(copy(data))
  
  out.list <- 
    processClimateIndices(dataToUse = dataToUse
                          ,variable = variable
                          ,variable_rightAxis = NULL
                          ,season.monthDay_start = season.monthDay_start
                          ,season.monthDay_end = season.monthDay_end
                          ,years.LTN = years.LTN
                          ,title = title
                          ,e_precip = e_precip 
                          ,e_threshold = e_threshold
                          ,indexSpecificValue = indexSpecificValue)
  
  dataToUse <- out.list[[1]]
  variable.all <- out.list[[2]]
  title <- out.list[[3]]
  year.increment <- out.list[[4]]
  
  rm(out.list)
  gc()
  
  #Decide on which summary statistic to apply
  summaryStatistic.use <- returnAppropriateSummaryStatistic(variable.all)
  
  #NEED TO IMPLEMENT THE RIGHT AXIS OPTION
  
  eval(parse(text = paste0('tempData <- dataToUse[,',summaryStatistic.use,'(',variable.all[1],'.amount,na.rm = TRUE),by = c(\'seasonNumber\',\'seasonNumber_startYear\')]')))
  setnames(tempData,c('V1','seasonNumber_startYear'),c(paste0(variable.all[1]),'Year'),skip_absent = TRUE)
  
  tempData[,seasonNumber := NULL]
  
  title <- gsub(pattern = 'Current Year is \\d{4}\n',replacement = '',title)
  title <- gsub(pattern = 'LTN calculated between \\d{4} and \\d{4}', replacement = 'Data Summarized by Season Start Year',title)
  
  
  tempData %>% 
    kableExtra::kbl(caption = title, booktabs = F) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover","condensed"), full_width = F) %>%
    kableExtra::save_kable(file = filename)
   #            density = 600, zoom = 2)
  
  return(out)
}