#' @title plotClimateTrendChart
#'
#' @description \code{plotClimateTrendChart} Generate a trendline chart across multiple growing seasons
#' using aWhere weather data with standardized formatting of a prefedined set of climate indices
#'
#' @details This function extends the aWhere charts package to plot the ETCCDI climate
#' change indices (reference below).  Most indices are implement3e with the default behavior 
#' described int he publication.  Default values of the indices can be overriden using the 
#' "indexSpecificValue" parameter. Applies a summary statistic over each year/season of data and then fits
#' trend line to that data across all LTN years
#' 
#' Also has helper functions including to automatically (with user permission) download missing
#' but needed data and store in the aWhereEnv to persist it throughout a user R session.
#'
#' @references http://etccdi.pacificclimate.org/list_27_indices.shtml
#'
#' @param data data frame in which variables are named according to the schema
#'   output by generateaWhereDataset.R (required)
#' @param variable character string denoting the variable to chart. Acceptable
#'   values are maxLenDrySpell, maxLenWetSpell, numFrostDays, numSummerDays, numIcingDays,
#'   numTropicalNights, minOfMaxTemp, maxOfMaxTemp, minOfMinTemp, maxOfMinTemp,
#'   dailyTempRange, maxSingleDayPrecip, max5ConsDayPrecip, simplePrecipIntensityIndex,
#'   precipSumExceedPercentile, warmSpellDurIndex, coldSpellDurIndex,
#'   countDaysPrecipExceedAmount, percentDaysMinTempBelowQuantile, percentDaysMaxTempBelowQuantile,
#'   percentDaysMinTempAboveQuantile, percentDaysMaxTempAboveQuantile, sumOfGdd, sumOfPET,
#'   sumOfPrecip, sumOfSolar, averageMaxTemp, averageMinTemp, averageMaxRH, averageMinRH,
#'   averageWind, and maxWindGust
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
#' @param yAxisLimits Used to set the limits of the y axis explicitly.  If used,
#'   must be a two element vector of the form c(minValue, maxValue) (optional)
#' @param size_font_main_title Font size of main title of graph (optional)
#' @param size_font_axis_titles Font size of axes on graph (optional)
#' @param size_font_axis_labels Font size of labels on axes on graph (optional)
#' @param size_font_legend_entries Font size of entries in lengend on graph (optional)
#' @param line_width Font size for line geometries on charts (optional)
#' @param annotationsWhichSide Whether to plot annotations on left or right side of figure (optional)
#' @param indexSpecificValue For the Climate Indices this tool can plot the user
#'   can override the default value of the index using this parameter (optional)
#'
#' @import dplyr
#' @import data.table
#'
#' @return plot object
#'
#' @examples
#' \dontrun{generateAndPlotClimateIndex(data = weather_df
#'                                      ,variable = "seasonTotalPrecip"
#'                                      ,season.monthDay_start = '09-01'
#'                                      ,season.monthDay_end = '11-30'
#'                                      ,years.LTN = seq(2010,2019,1)
#'                                      ,e_precip = TRUE
#'                                      ,e_threshold = 10
#'                                      ,doRoll = TRUE)}
#'
#' @export

plotClimateTrendChart <- function(data
                                  ,variable
                                  ,season.monthDay_start = '01-01'
                                  ,season.monthDay_end = '12-31'
                                  ,years.LTN = seq(2006,2020,1)
                                  ,title = NULL
                                  ,e_precip = FALSE 
                                  ,e_threshold = 35 
                                  ,yAxisLimits = NA
                                  ,size_font_main_title = 16
                                  ,size_font_axis_titles = 14
                                  ,size_font_axis_labels = 12
                                  ,size_font_legend_entries = 12
                                  ,line_width = 1
                                  ,annotationsWhichSide = 'left'
                                  ,indexSpecificValue = NULL) {

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
  setnames(tempData,c('V1','seasonNumber_startYear'),c(paste0(variable.all[1],'.amount'),'date'),skip_absent = TRUE)
  
  eval(parse(text = paste0('tempData[,',variable.all[1],'.average := NA]')))
  eval(parse(text = paste0('tempData[,',variable.all[1],'.stdDev := NA]')))
  
  title <- gsub(pattern = 'Current Year is \\d{4}\n',replacement = '',title)
  title <- gsub(pattern = 'LTN calculated between \\d{4} and \\d{4}', replacement = 'Data Summarized by Season Start Year',title)
  
  out <-
    generateaWhereChart(data = tempData
                        ,variable = variable.all[1]
                        ,variable_rightAxis = NA
                        ,title = title
                        ,e_precip = FALSE # already done above
                        ,doRoll = FALSE
                        ,rolling_window = 30
                        ,size_font_main_title = size_font_main_title
                        ,size_font_axis_titles = size_font_axis_titles 
                        ,size_font_axis_labels = size_font_axis_labels
                        ,size_font_legend_entries = size_font_legend_entries
                        ,line_width = line_width
                        ,yAxisLimits = yAxisLimits
                        ,annotationsWhichSide = annotationsWhichSide)
  
  return(out)
}
