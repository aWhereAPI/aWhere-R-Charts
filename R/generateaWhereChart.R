#' @title generateaWhereChart
#'
#' @description
#' \code{generateaWhereChart} Generate a plot using aWhere weather data with standardized formatting.
#'
#' @details
#' This function makes basic line plots using data returned from the aWhereAPI and formatted using
#' the generateaWhereDataset function. The function requires variables to be named according to the
#' conventions of that function, or the basic functions of the aWhereAPI package. Users must pass the dataset object
#' to the function as well as one of the acceptable list of variables to chart. Users may also
#' use the optional parameters to customize a plot title, turn on the "effective precipitation"
#' option, or customize the rolling window. The returned plot will be a line chart comparing
#' the actual conditions within the dataset's datespan (labeled Current in the plot) to the 
#' long-term normals for that same datespan.
#' 
#' If users turn on effective precipitation (e_precip = TRUE), the function will add a line to 
#' the plot in which all daily precipitation values (the Current line) are capped at the maximum 
#' allowable amount (defined by the parameter e_threshold). This serves as a proxy for runoff
#' in times of high volumes of rain. The default value of e_threshold is 35 mm.
#' 
#' The standard plot formatting is generated using ggplot2's gray theme.
#'
#' @references http://developer.awhere.com/api/reference/
#'
#' @param - data: data frame in which variables are named according to the schema output by generateaWhereChart.R (required)
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
#' \dontrun{generateaWhereChart(data = weather_df, variable = "accumulatedPrecipitation", 
#'                              title = "Accumulated Precipitation values at Location from Xdate to Ydate",
#'                              e_precip = TRUE, e_threshold = 20)}

#' @export


generateaWhereChart <- function(data
                                ,variable 
                                ,title 
                                ,e_precip = FALSE 
                                ,e_threshold = 35 
                                ,rolling_window = 30) {

    ##determine vars to chart
    if(variable %in% c("precipitation", "accumulatedPrecipitation",
                       "maxTemp", "minTemp", "pet", "accumulatedPet",
                       "ppet")) {
      
      varsToChart <- c(paste0(variable,'.amount'), paste0(variable,'.average'))    
      
    } else if(variable == "rollingavgppet") {
      
      varsToChart <- c("ppet.amount", "ppet.average")    
      
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

    #if title is not given by user, set it to date range + variable
    if (is.null(title)) {
      title <- paste0(variable, " from ", max(data$date), " to ", min(data$date))
    }
    
    #filter out relevant data
    chart_data <- data[, c("date", varsToChart)]
  
    #set common names of columns
    chart_data <- setNames(chart_data, c("date", "Current", "LTN"))

    #if e_precip is set to true, bring in daily precip data and calculate accumulated
    #daily precipitation using either default or user-defined threshold
    
    if(variable == "accumulatedPrecipitation" & e_precip == TRUE) {
      temp <- data[, c("date", "precipitation.amount")]
      chart_data <- merge(chart_data, temp, by = "date")
      chart_data$precipitation.amount[chart_data$precipitation.amount > e_threshold ] <- e_threshold
      chart_data <- dplyr::mutate(chart_data,
                                  EffectiveCurrent = cumsum(chart_data$precipitation.amount))
      chart_data <- dplyr::select(chart_data, -precipitation.amount)
    }
    
    #if variable is set to "rollingavgppet", bring in daily precip and pet data 
    #and calculate rolling averages. If e_precip = TRUE, add effective precip
    
    if(variable == "rollingavgppet") {
      if(e_precip == TRUE) {
        temp <- data[, c("date", "precipitation.amount", "pet.amount")]
        chart_data <- merge(chart_data, temp, by = "date")
        chart_data$precipitation.amount[chart_data$precipitation.amount > e_threshold ] <- e_threshold
        chart_data <- dplyr::mutate(chart_data,
                                    EffectiveCurrent = precipitation.amount/pet.amount)
        chart_data <- dplyr::select(chart_data, -precipitation.amount, -pet.amount)
        
        chart_data$EffectiveCurrent <- zoo::rollapply(chart_data$EffectiveCurrent 
                                        ,width = rolling_window 
                                        ,align = "right"
                                        ,FUN = mean
                                        ,na.rm = TRUE
                                        ,fill = NA)
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
    }
    
    #convert character date column to Date
    chart_data$date <- as.Date(chart_data$date)
    
    #change data format from wide to long
    chart_data <- tidyr::gather(chart_data, 
                                key = Variable, 
                                value = measure, 
                                2:ncol(chart_data))
    
    #set color scale based on # of vars to chart
    if(length(unique(chart_data$Variable)) == 2) {
      colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#FF810E")) 
    } else {
      colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
    } 
    
    #make chart
    chart <- ggplot() + theme_igray() + colorScaleToUse +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5,
                na.rm = TRUE) +
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
