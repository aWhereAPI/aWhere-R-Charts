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
#' @param - data: data frame in which variables are named according to the aWhere API conventions (required)
#' @param - variable: character string denoting the variable to chart. Acceptable values 
#'             are precipitation, accumulatedPrecipitation, maxTemp, minTemp, pet, accumulatedPet, ppet,
#'             or rollingavgppet. (required)
#' @param - title: character string of title to assign to the plot. (required)
#' @param - e_precip: logical, if set to TRUE, effective precipitation will 
#'             be calculated and charted based on e_threshold. Default is set to FALSE. (optional)
#' @param - e_threshold: numeric value (in milimeters) for the daily maximum used to calculate 
#'             effective precipitation if e_precip is set to TRUE. (optional)
#' @param - rolling_window: numeric value for the number of days to use in rolling 
#'             average calculations, only applicable if the variable parameter is set to
#'             "rollingavgppet". Default value is 30. (optional)
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
#' \dontrun{generateaWhereStdDevChart(data = weather_df, variable = "accumulatedPrecipitation", 
#'                              title = "Accumulated Precipitation values at Location from Xdate to Ydate",
#'                              e_precip = TRUE, e_threshold = 20)}

#' @export

generateaWhereStdDevChart <- function(data
                                      ,variable
                                      ,title = NULL
                                      ,e_precip = FALSE
                                      ,e_threshold = 35 
                                      ,rolling_window = 30) {
  
  ##determine vars to chart
  if(variable %in% c("precipitation", "accumulatedPrecipitation",
                     "maxTemp", "minTemp", "pet", "accumulatedPet",
                     "ppet")) {
    
    varsToChart <- c(paste0(variable,'.amount'), paste0(variable,'.average'), paste0(variable,'.stdDev'))    
    
  } else if(variable == "rollingavgppet") {
    
    varsToChart <- c("ppet.amount", "ppet.average", "ppet.stdDev")    
    
  } else {
    
    stop("Input Variable is not from allowed list. Please use precipitation, accumulatedPrecipitation, 
           maxTemp, minTemp, pet, accumulatedPet, ppet, or rollingavgppet.")
    
  }
  
  ##set ylabel
  if(variable %in% c("precipitation", "accumulatedPrecipitation",
                     "pet", "accumulatedPet")) {
    
    ylabel = "mm"
    
  } else if(variable %in% c("maxTemp", "minTemp")) {
    
    ylabel <- "Celsius"  
    
  } else {
    
    ylabel = "Index"
    
  }
  
  #filter out relevant data
  chart_data <- data[, c("date", varsToChart)]
  
  #set common names of columns
  chart_data <- setNames(chart_data, c("date", "Current", "LTN", "LTNstddev"))
  
  #separate out stdDev data into a separate dataframe to calculate ymax and ymin
  chart_data_temp <- chart_data[, c("date", "LTN", "LTNstddev")] %>% 
    dplyr::mutate(ymax = LTN + LTNstddev,
           ymin = LTN - LTNstddev,
           Variable = "LTN") %>% 
    dplyr::select(-LTN, -LTNstddev)
  
  #ensure ymin in precip or accprecip never goes below 0
  if(variable == "accprecip" | variable == "precip") {
    chart_data_temp$ymin[chart_data_temp$ymin < 0] <- 0
  }
  
  chart_data <- dplyr::select(chart_data, -LTNstddev)
  
  
  #if e_precip is set to true, bring in daily precip data and calculate accumulated
  #daily precipitation using either default or user-defined threshold
  
  if(variable == "accprecip" & e_precip == TRUE) {
    temp <- data[, c("date", "precip")]
    chart_data <- merge(chart_data, temp, by = "date")
    chart_data$precip[chart_data$precip > e_threshold ] <- e_threshold
    chart_data <- dplyr::mutate(chart_data,
                                EffectiveCurrent = cumsum(chart_data$precip))
    chart_data <- dplyr::select(chart_data, -precip)
  }
  
  #if variable is set to "rollingavgppet", bring in daily precip and pet data 
  #and calculate rolling averages. If e_precip = TRUE, add effective precip
  
  if(variable == "rollingavgppet") {
    if(e_precip == TRUE) {
      temp <- data[, c("date", "precip", "pet.amount")]
      chart_data <- merge(chart_data, temp, by = "date")
      chart_data$precip[chart_data$precip > e_threshold ] <- e_threshold
      chart_data <- dplyr::mutate(chart_data,
                                  EffectiveCurrent = precip/pet.amount)
      chart_data <- dplyr::select(chart_data, -precip, -pet.amount)
      
      chart_data$EffectiveCurrent <- zoo::rollapply(chart_data$EffectiveCurrent, 
                                                    width = rolling_window, 
                                                    align = "right",
                                                    FUN = mean, 
                                                    fill = NA)
    } 
    
    chart_data$Current <- zoo::rollapply(chart_data$Current 
                                         ,width = rolling_window
                                         ,align = "right"
                                         ,FUN = mean 
                                         ,na.rm = TRUE
                                         ,fill = NA)
    chart_data$LTN <-     zoo::rollapply(chart_data$LTN
                                         ,width = rolling_window
                                         ,align = "right"
                                         ,FUN = mean
                                         ,na.rm = TRUE
                                         ,fill = NA)
    chart_data_temp$ymax <-     zoo::rollapply(chart_data_temp$ymax
                                         ,width = rolling_window
                                         ,align = "right"
                                         ,FUN = mean
                                         ,na.rm = TRUE
                                         ,fill = NA)
    chart_data_temp$ymin <-     zoo::rollapply(chart_data_temp$ymin
                                         ,width = rolling_window
                                         ,align = "right"
                                         ,FUN = mean
                                         ,na.rm = TRUE
                                         ,fill = NA)
  }
  
  #change data format from wide to long
  chart_data <- tidyr::gather(chart_data, 
                              key = Variable, 
                              value = measure, 
                              2:ncol(chart_data))
  
  #merge in ymax/ymin data frame to master chart_data
  chart_data <- left_join(chart_data, chart_data_temp, by = c("date", "Variable"))
  
  #convert character date column to Date
  chart_data$date <- as.Date(chart_data$date)
  
  #if title is not given by user, set it to date range + variable
  if (is.null(title)) {
    title <- paste0(variable, " from ", min(data$date), " to ", max(data$date))
  }
  
  
  #set color scale based on # of vars to chart
  if(length(unique(chart_data$Variable)) == 2) {
    colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#FF810E")) 
  } else {
    colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
  } 
  
  #make chart
  chart <- ggplot(data = chart_data, 
                  aes(x = date, 
                      y = measure, 
                      ymax = ymax,
                      ymin = ymin,
                      group = Variable,
                      color = Variable,
                      fill = Variable),
                  na.rm = TRUE) + 
    theme_igray() + 
    colorScaleToUse +
    colorFillToUse +
    geom_line(size = 1.5) +
    geom_ribbon(alpha = 0.3, linetype = "blank") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="bottom", legend.direction="horizontal",
          legend.title = element_blank()) +
    labs(x="Date", y = ylabel) +
    #the next two lines may be commented out if the vertical current date line is not desired
    geom_vline(xintercept = as.numeric(Sys.Date()),
               linetype = "dashed") +
    ggtitle(title)
  
  return(chart)
  }
