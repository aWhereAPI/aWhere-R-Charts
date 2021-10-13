#' @title generateAndPlotClimateIndex
#'
#' @description \code{generateAndPlotClimateIndex} Generate a plot using aWhere weather
#' data with standardized formatting.
#'
#' @details This function extends the aWhere charts package to plot the ETCCDI climate
#' change indices (reference below).  Most indices are implement with the default behavior 
#' described int he publication.  Default values of the indices can be overriden using the 
#' "indexSpecificValue" parameter.  Can also plot all attributes from aWhereCharts functiona
#' and create plots combinging both sets of information.
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
#'   percentDaysMinTempAboveQuantile, percentDaysMaxTempAboveQuantileaccumulatedGdd as well as 
#'   accumulatedPet, accumulatedPpet,accumulatedPrecipitation, gdd, pet, precipitation, maxRH, minRH,
#'   solar,averageWind,dayMaxWind, rollingavgppet, maxTemp, minTemp, dayMaxWind, averageWind
#' @param variable_rightAxis  What variable to plot over the primary variable.
#'   The right y-axis of the plot will be used to present its range. Note that
#'   it will always be plotted as a line chart. Same valid values as the
#'   variable param.  (optional)
#' @param startYearOfSeasonToPlot Specify the start year of the "season" you want
#'   plotted for the current time period.  For example if you specify 2020 here and
#'   season.monthDay_start is '08-01' and season.monthDay_end is '12-15' the current
#'   time period will begin on '2020-01-01' and end on '2020-12-15'.  Dates will be
#'   adjusted properly if the season goes over Jan 1st.
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
#' @param includeSTD whether to plot the standard deviation as a ribbon around
#'   the LTN value of the main variable. (optional)
#' @param maingraphType Which type of graph to make for the main plot.  Valid
#'   values are "line" and "bar" (optional)
#' @param daysToAggregateOver Used to temporally aggregate data.  Unit is in
#'   days. This is done based on the startdate of the dataset, not a calendar
#'   week (otpional)
#' @param yAxisLimits Used to set the limits of the y axis explicitly.  If used,
#'   must be a two element vector of the form c(minValue, maxValue) (optional)
#' @param size_font_main_title Font size of main title of graph (optional)
#' @param size_font_axis_titles Font size of axes on graph (optional)
#' @param size_font_axis_labels Font size of labels on axes on graph (optional)
#' @param size_font_legend_entries Font size of entries in lengend on graph (optional)
#' @param line_width Font size for line geometries on charts (optional)
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
#'                                      ,startYearOfSeasonToPlot = 2020
#'                                      ,season.monthDay_start = '09-01'
#'                                      ,season.monthDay_end = '11-30'
#'                                      ,years.LTN = seq(2010,2019,1)
#'                                      ,e_precip = TRUE
#'                                      ,e_threshold = 10
#'                                      ,doRoll = TRUE)}
#'
#' @export

generateAndPlotClimateIndex <- function(data
                                      ,variable
                                      ,variable_rightAxis = NULL
                                      ,startYearOfSeasonToPlot
                                      ,season.monthDay_start = '01-01'
                                      ,season.monthDay_end = '12-31'
                                      ,years.LTN = seq(2006,2020,1)
                                      ,title = NULL
                                      ,e_precip = FALSE 
                                      ,e_threshold = 35 
                                      ,doRoll = FALSE
                                      ,rolling_window = 30
                                      ,includeSTD = FALSE
                                      ,mainGraphType = 'line'
                                      ,daysToAggregateOver = NULL
                                      ,yAxisLimits = NA
                                      ,size_font_main_title = 16
                                      ,size_font_axis_titles = 14
                                      ,size_font_axis_labels = 12
                                      ,size_font_legend_entries = 12
                                      ,line_width = 1
                                      ,indexSpecificValue = NULL) {
  
  #because we are going to change the datastructure and it is a data.table we
  #will explicitly copy what is passed in so it doesn't violate user's scoping
  #expectations 
  dataToUse <- data.table::as.data.table(copy(data))
  
  out.list <- 
    processClimateIndices(dataToUse = dataToUse
                          ,variable = variable
                          ,variable_rightAxis = variable_rightAxis
                          ,startYearOfSeasonToPlot = startYearOfSeasonToPlot
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
  
  out <-
    generateaWhereChart(data = dataToUse
                        ,variable = variable.all[1]
                        ,variable_rightAxis = variable.all[2]
                        ,day_start = paste0(startYearOfSeasonToPlot,'-',season.monthDay_start)
                        ,day_end = paste0(startYearOfSeasonToPlot + year.increment,'-',season.monthDay_end)
                        ,title = title
                        ,e_precip = FALSE # already done above
                        ,e_threshold = e_threshold
                        ,doRoll = doRoll
                        ,rolling_window = rolling_window
                        ,includeSTD = includeSTD
                        ,mainGraphType = mainGraphType
                        ,daysToAggregateOver = daysToAggregateOver
                        ,yAxisLimits = yAxisLimits
                        ,size_font_main_title = size_font_main_title
                        ,size_font_axis_titles = size_font_axis_titles 
                        ,size_font_axis_labels = size_font_axis_labels
                        ,size_font_legend_entries = size_font_legend_entries
                        ,line_width = line_width
                        ,indexSpecificValue = indexSpecificValue)

  return(out)
}