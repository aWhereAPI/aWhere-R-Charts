#' @title generateaWhereStdDevChart
#'
#' @description
#' \code{generateaWhereStdDevChart} Generate a plot using aWhere weather data with standardized 
#'                                   formatting and standard deviations included.
#'
#' @details
#' This function makes basic line plots using the same structure, formatting and inputs utilized 
#' in the generateaWhereChart function, but adds one standard deviation's shading above and below 
#' the long-term normal line. This function is meant to help users understand whether the current
#' weather conditions are significantly outside the norm. 
#'  
#' Refer to the documentation for generateaWhereChart for more details on the parameter inputs and 
#' definitions. The input variables allowed are the same as those allowed for the generateaWhereChart
#' function, but it should be noted that as the datespan increases, the standard deviation for variables 
#' which are accumulations is likely to become increasingly wide.
#'
#' @references http://developer.awhere.com/api/reference/
#'
#' @param data data frame in which variables are named according to the aWhere API conventions (required)
#' @param variable character string denoting the variable to chart. Acceptable values 
#'             are accumulatedGdd, accumulatedPet, accumulatedPpet, accumulatedPrecipitation,
#'             gdd, pet, precipitation, maxRH, minRH, solar,averageWind,dayMaxWind
#'             or rollingavgppet. (required)
#' @param title character string of title to assign to the plot. (required)
#' @param e_precip logical, if set to TRUE, effective precipitation will 
#'             be calculated and charted based on e_threshold. Default is set to FALSE. (optional)
#' @param e_threshold numeric value (in milimeters) for the daily maximum used to calculate 
#'             effective precipitation if e_precip is set to TRUE. (optional)
#' @param doRoll Boolean for whether a rolling calculation should be done to smooth the data (optional)
#' @param rolling_window numeric value for the number of days to use in rolling 
#'             average calculations, only applicable if the variable parameter is set to
#'             "rollingavgppet". Default value is 30. (optional)
#' @param yAxisLimits Used to set the limits of the y axis explicitly.  If used, must be a two element vector of the form 
#'                       c(minValue, maxValue) (optional)
#' @param size_font_main_title Font size of main title of graph (optional)
#' @param size_font_axis_titles Font size of axes on graph (optional)
#' @param size_font_axis_labels Font size of labels on axes on graph (optional)
#' @param size_font_legend_entries Font size of entries in lengend on graph (optional)
#' @param line_width Font size for line geometries on charts (optional)
#' 
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import zoo
#'
#' @return plot object
#'
#' @examples
#' \dontrun{generateaWhereStdDevChart(data = weather_df
#'                                    ,variable = "accumulatedPrecipitation" 
#'                                    ,title = "Accumulated Precipitation values at Location from Xdate to Ydate"
#'                                    ,e_precip = TRUE
#'                                    ,e_threshold = 20)}

#' @export

generateaWhereStdDevChart <- function(data
                                      ,variable
                                      ,title = NULL
                                      ,e_precip = FALSE
                                      ,e_threshold = 35
                                      ,doRoll = FALSE
                                      ,rolling_window = 30
                                      ,yAxisLimits = NA
                                      ,size_font_main_title = 16
                                      ,size_font_axis_titles = 14
                                      ,size_font_axis_labels = 12
                                      ,size_font_legend_entries = 12
                                      ,line_width = 1) {
  
  return(generateaWhereChart(data
                             ,variable = variable
                             ,title = title
                             ,e_precip = e_precip 
                             ,e_threshold = e_threshold
                             ,doRoll = doRoll
                             ,rolling_window = rolling_window
                             ,includeSTD = TRUE
                             ,yAxisLimits = yAxisLimits
                             ,size_font_main_title = size_font_main_title
                             ,size_font_axis_titles = size_font_axis_titles 
                             ,size_font_axis_labels = size_font_axis_labels
                             ,size_font_legend_entries = size_font_legend_entries
                             ,line_width = line_width))
  
}
