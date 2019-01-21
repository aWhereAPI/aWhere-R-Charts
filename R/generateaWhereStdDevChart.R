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
#'             are accumulatedGdd, accumulatedPet, accumulatedPpet, accumulatedPrecipitation,
#'             gdd, pet, precipitation, maxRH, minRH, solar,averageWind,dayMaxWind
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
                                      ,doRoll = TRUE
                                      ,rolling_window = 30) {
  
  variable.orig <- copy(variable)
  
  #rename variable to match appropriate column in data
  if (variable == 'maxTemp') {
    variable <- 'temperatures.max'
  } else if (variable == 'minTemp') {
    variable <- 'temperatures.min'
  } else if (variable == 'maxRH') {
    variable <- 'relativeHumidity.max'
  } else if (variable == 'minRH') {
    variable <- 'relativeHumidity.min'
  } else if (variable == 'averageWind') {
    variable <- 'wind.average'
  } else if (variable == 'dayMaxWind') {
    variable <- 'wind.dayMax'
  } else if (variable == 'rollingavgppet') {
    variable <- 'ppet'
    doRoll <- TRUE
  }
  
  
  #Confirm chosen variable is in the data structure
  if (any(grepl(pattern = variable,x = colnames(data),fixed = TRUE)) == FALSE) {
    stop("Input Variable is not from allowed list. Please use ccumulatedGdd, 
         accumulatedPet, accumulatedPpet, accumulatedPrecipitation,
         gdd, pet, precipitation, maxRH, minRH, solar,averageWind,dayMaxWind
         or rollingavgppet.")
  } else {
    varsToChart <- c(paste0(variable,'.amount'), paste0(variable,'.average'), paste0(variable,'.stdDev')) 
  }
  
  #because we are going to change the datastructure and it is a data.table we will
  #explicitly copy what is passed in so it doesn't violate user's scoping expectations
  if (e_precip == FALSE) {
    dataToUse <- data
    
  } else if ((e_precip == TRUE & (grepl(pattern = 'precipitation'
                                        ,x = variable
                                        ,ignore.case = TRUE) | 
                                  grepl(pattern = 'Ppet'
                                        ,x = variable
                                        ,ignore.case = TRUE))) == TRUE) {
    dataToUse <- copy(data)
    #if e_precip is set to true, bring in daily precip data and calculate accumulated
    #daily precipitation using either default or user-defined threshold
    
    
    dataToUse[,precipitation.amount.effective := precipitation.amount]
    
    dataToUse[precipitation.amount > e_threshold, precipitation.amount.effective := e_threshold]
    dataToUse[,ppet.amount.effective := precipitation.amount.effective / pet.amount]
    
    dataToUse[,accumulatedPrecipitation.amount.effective := cumsum(precipitation.amount.effective)]
    dataToUse[,accumulatedPpet.amount.effective := cumsum(ppet.amount.effective)]
    
    varsToChart <- c(varsToChart,paste0(variable,'.amount.effective'))
  }
  
  
  ##set ylabel
  if (grepl(pattern = 'Gdd'
            ,x = variable
            ,ignore.case = TRUE) == TRUE) {
    ylabel = 'GDDs'
  } else if (grepl(pattern = 'PPet'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = 'Index'
  } else if (grepl(pattern = 'Pet'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = 'mm'
  } else if (grepl(pattern = 'precipitation'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = 'mm'
  } else if (grepl(pattern = 'relativeHumidity'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = '%'
  } else if (grepl(pattern = 'solar'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = 'Wh/m^2'
  } else if (grepl(pattern = 'temperatures'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = 'Celcius'
  } else if (grepl(pattern = 'wind'
                   ,x = variable
                   ,ignore.case = TRUE) == TRUE) {
    ylabel = 'm/s'
  }
  
  #if title is not given by user, set it to date range + variable
  if (is.null(title)) {
    title <- paste0(variable.orig, " from ", min(dataToUse$date), " to ", max(dataToUse$date))
  }
  
  #filter out relevant data
  chart_data <- dataToUse[, c("date", varsToChart),with = FALSE]
  
  #set common names of columns
  chart_data <- setNames(chart_data, c("date"
                                       ,"Current"
                                       ,"LTN"
                                       ,"LTNstddev"
                                       ,ifelse(e_precip == TRUE,'EffectiveCurrent','')))

  
  #separate out stdDev data into a separate dataframe to calculate ymax and ymin
  chart_data_temp <- chart_data[,list(date,LTN + LTNstddev,LTN - LTNstddev,'LTN')]
  setnames(chart_data_temp, c('date','ymax','ymin','Variable'))
  

  chart_data[,LTNstddev := NULL]
  
  
  
  #if variable is set to "rollingavgppet", bring in daily precip and pet data 
  #and calculate rolling averages. If e_precip = TRUE, add effective precip
  
  #if variable is set to "rollingavgppet", bring in daily precip and pet data 
  #and calculate rolling averages. If e_precip = TRUE, add effective precip
  
  if(doRoll == TRUE) {
    if(e_precip == TRUE) {
      
      chart_data[,EffectiveCurrent := zoo::rollapply(EffectiveCurrent 
                                                     ,width = rolling_window 
                                                     ,align = "right"
                                                     ,FUN = mean
                                                     ,na.rm = TRUE
                                                     ,fill = NA)]
    }
    
    chart_data[,Current := zoo::rollapply(Current 
                                          ,width = rolling_window 
                                          ,align = "right"
                                          ,FUN = mean
                                          ,na.rm = TRUE
                                          ,fill = NA)]
    
    chart_data[,LTN := zoo::rollapply(LTN
                                      ,width = rolling_window
                                      ,align = "right"
                                      ,FUN = mean
                                      ,na.rm = TRUE
                                      ,fill = NA)]
  
    chart_data_temp[,ymax := zoo::rollapply(ymax
                                           ,width = rolling_window
                                           ,align = "right"
                                           ,FUN = mean
                                           ,na.rm = TRUE
                                           ,fill = NA)]
    chart_data_temp[,ymin := zoo::rollapply(ymin
                                           ,width = rolling_window
                                           ,align = "right"
                                           ,FUN = mean
                                           ,na.rm = TRUE
                                           ,fill = NA)]
  }
  
  #convert character date column to Date
  chart_data[,date :=as.Date(date)]
  
  #change data format from wide to long
  chart_data <- tidyr::gather(chart_data, 
                              key = Variable, 
                              value = measure, 
                              2:ncol(chart_data))
  
  #merge in ymax/ymin data frame to master chart_data
  chart_data <- left_join(chart_data, chart_data_temp, by = c("date", "Variable"))
  
  
  #set color scale based on # of vars to chart
  if(length(unique(chart_data$Variable)) == 2) {
    colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#FF810E")) 
  } else {
    colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
  } 
  
  #make chart
  chart <- ggplot(data = chart_data
                  ,aes(x = date 
                      ,y = measure 
                      ,ymax = ymax
                      ,ymin = ymin
                      ,group = Variable
                      ,color = Variable
                      ,fill = Variable)
                      ,na.rm = TRUE) + 
    theme_igray() + 
    colorScaleToUse +
    colorFillToUse +
    geom_line(size = 1.5) +
    geom_ribbon(alpha = 0.3
                ,linetype = "blank") +
    theme(axis.text.x = element_text(angle = 45
                                     ,hjust = 1)) +
    theme(legend.position="bottom"
          ,legend.direction="horizontal"
          ,legend.title = element_blank()) +
    labs(x="Date"
         ,y = ylabel) +
    #the next two lines may be commented out if the vertical current date line is not desired
    geom_vline(xintercept = as.numeric(Sys.Date())
               ,linetype = "dashed") +
    ggtitle(title)
  
  
  return(chart)
  }
