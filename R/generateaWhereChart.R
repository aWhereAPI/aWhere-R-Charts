#' @title generateaWhereChart
#'
#' @description \code{generateaWhereChart} Generate a plot using aWhere weather
#' data with standardized formatting.
#'
#' @details This function makes basic line plots using data returned from the
#' aWhereAPI and formatted using the generateaWhereDataset function. The
#' function requires variables to be named according to the conventions of that
#' function, or the basic functions of the aWhereAPI package. Users must pass
#' the dataset object to the function as well as one of the acceptable list of
#' variables to chart. Users may also use the optional parameters to customize a
#' plot title, turn on the "effective precipitation" option, or customize the
#' rolling window. The returned plot will be a line chart comparing the actual
#' conditions within the dataset's datespan (labeled Current in the plot) to the
#' long-term normals for that same datespan.
#'
#' If users turn on effective precipitation (e_precip = TRUE), the function will
#' add a line to the plot in which all daily precipitation values (the Current
#' line) are capped at the maximum allowable amount (defined by the parameter
#' e_threshold). This serves as a proxy for runoff in times of high volumes of
#' rain. The default value of e_threshold is 35 mm.
#'
#' The standard plot formatting is generated using ggplot2's gray theme.
#'
#' @references http://developer.awhere.com/api/reference/
#'
#' @param data data frame in which variables are named according to the schema
#'   output by generateaWhereDataset.R (required)
#' @param variable character string denoting the variable to chart. Acceptable
#'   values are accumulatedGdd, accumulatedPet, accumulatedPpet,
#'   accumulatedPrecipitation, gdd, pet, precipitation, maxRH, minRH,
#'   solar,averageWind,dayMaxWind, rollingavgppet. (required)
#' @param variable_rightAxis  What variable to plot over the primary variable.
#'   The right y-axis of the plot will be used to present its range. Note that
#'   it will always be plotted as a line chart. Same valid values as the
#'   variable param.  (optional)
#' @param day_start Specify the start date of the daily data you want plotted in
#'   the figure (optional)
#' @param day_end Specify the end date of the daily data you want plotted in the
#'   figure (optional)
#' @param title character string of title to assign to the plot. (required)
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
#'
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import zoo
#' @import data.table
#' @import scales
#' @import ggpmisc
#' @importFrom broom glance
#'
#' @return plot object
#'
#' @examples
#' \dontrun{generateaWhereChart(data = weather_df
#'                              ,variable = "accumulatedPrecipitation"
#'                              ,e_precip = TRUE
#'                              ,e_threshold = 10
#'                              ,doRoll = TRUE)}
#'
#' @export

generateaWhereChart <- function(data
                                ,variable
                                ,variable_rightAxis = NULL
                                ,day_start = NULL
                                ,day_end = NULL
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
                                ,annotationsWhichSide = 'left') {
  
  
  #We are using a list consturct to hold all variables so we can loop over its length
  temp_variable   <- copy(variable)
  variable        <- list()
  variable.orig   <- list()
  varsToChart     <- list()
  variableNames   <- list()
  ylabel          <- list()
  chart_data      <- list()
  chart_data_long <- list()
  colorScheme     <- list()
  rangeToUse      <- list()
  
  
  #because we are going to change the datastructure and it is a data.table we
  #will explicitly copy what is passed in so it doesn't violate user's scoping
  #expectations 
  dataToUse <- data.table::as.data.table(copy(data))
  
  #Subset the data if the user specifies
  if (is.null(day_start) == FALSE) {
    dataToUse <- dataToUse[date >= as.Date(day_start)]
  }
  
  if (is.null(day_end) == FALSE) {
    dataToUse <- dataToUse[date <= as.Date(day_end)]
  }
  
  #What to call the SD entry in the legend if displayed
  SD_label <- paste0('SD of LTN')
  
  variable <- as.list(copy(temp_variable))
  
  try(
      if (is.na(variable_rightAxis)) {
        variable_rightAxis <- NULL
      },silent = TRUE
  )
  
  if (is.null(variable_rightAxis) == FALSE) {
    variable <- c(variable,as.list(copy(variable_rightAxis)))
  }
  
  variable.orig <- copy(variable)
  
  if (!is.null(daysToAggregateOver)) {
    
    #check to see if the datastructure contains the column "seasonNumber."  If so do these calcs by that
    if ('seasonNumber' %in% colnames(dataToUse)) {
      seasonNumber_str <- ', by = \'seasonNumber\''
    } else {
      seasonNumber_str <- ''
    }
    
    typesOfColumns <- c('.amount','.average','.stdDev')
    
    variablesToProcess <- unique(gsub(pattern = paste0(typesOfColumns,collapse = '|')
                                      ,replacement = ''
                                      ,x = colnames(dataToUse)))
    
    variablesToProcess <- setdiff(variablesToProcess
                                  ,c('latitude','longitude','date','day'))
    
    if('wind' %in% variablesToProcess) {
      variablesToProcess <- setdiff(variablesToProcess,'wind')
      variablesToProcess <- c(variablesToProcess,'wind.average')
    }
    
    #The logic here is that the accumulated columns are already calculated for
    #temporally subsetting and nothing needs to be done.  For variables that are
    #logically summed over time, do that for the .amount and .average columns
    #but the .stdDev column should have the mean taken.  For all other columns
    #take the mean
    for (x in 1:length(variablesToProcess)) {
      for (y in 1:length(typesOfColumns)) {
        
        currentColumn <- paste0(variablesToProcess[x],typesOfColumns[y])
        
        #Additional years can be added to the dataset but only the .amount column will be present
        if ((grepl(pattern = 'year|rolling'
                   ,x = currentColumn
                   ,ignore.case = TRUE) & typesOfColumns[y] %in% c('.average','.stdDev')) == TRUE) {
          next
          
        } else if (grepl(pattern = 'accumulated'
                         ,x = currentColumn
                         ,fixed = TRUE) == TRUE) {
          
          eval(parse(text = paste0('dataToUse[,',paste0(currentColumn,'.new'),' := ',currentColumn,']')))
          
        } else if ((grepl(pattern = 'gdd|pet|ppet|precipitation'
                          ,x = currentColumn
                          ,ignore.case = TRUE) & typesOfColumns[y] != '.stdDev') == TRUE) {
          
          eval(parse(text = paste0('dataToUse[,',paste0(currentColumn,'.new'),' := zoo::rollapply(',currentColumn,' 
                                   ,width = daysToAggregateOver 
                                   ,align = "right"
                                   ,FUN = sum
                                   ,na.rm = TRUE
                                   ,fill = NA
                                   ,partial = TRUE)',seasonNumber_str,']')))
          
        } else if ((grepl(pattern = 'maxLenDrySpell'
                          ,x = currentColumn
                          ,ignore.case = TRUE) & typesOfColumns[y] != '.stdDev') == TRUE) {
          
          eval(parse(text = paste0('dataToUse[,',paste0(currentColumn,'.new'),' := zoo::rollapply(',currentColumn,' 
                                   ,width = daysToAggregateOver 
                                   ,align = "left"
                                   ,FUN = max
                                   ,na.rm = TRUE
                                   ,fill = NA
                                   ,partial = TRUE)',seasonNumber_str,']')))
          
        } else {
          eval(parse(text = paste0('dataToUse[,',paste0(currentColumn,'.new'),' := zoo::rollapply(',currentColumn,' 
                                   ,width = daysToAggregateOver 
                                   ,align = "right"
                                   ,FUN = mean
                                   ,na.rm = TRUE
                                   ,fill = NA
                                   ,partial = TRUE)',seasonNumber_str,']')))
        }
      }
    }
    
    #take every Nth row
    dataToUse <- dataToUse[seq(from = daysToAggregateOver + 1
                               ,to = nrow(dataToUse)
                               ,by = daysToAggregateOver),]
    
    #remove the previous variables
    suppressWarnings(dataToUse[,unique(as.data.table(expand.grid(variablesToProcess
                                                                 ,typesOfColumns))[,paste0(Var1,Var2)]) := NULL])
    #rename columns to previous names
    setnames(dataToUse
             ,colnames(dataToUse)
             ,gsub(pattern = '.new'
                   ,replacement = ''
                   ,x = colnames(dataToUse)
                   ,fixed = TRUE))
  }
  
  #for each variable to be plotted, loop through to create data structures for visualization
  for (x in 1:length(variable)) {
    
    #rename variable to match appropriate column in data
    if (variable[[x]] == 'maxTemp') {
      variable[[x]] <- 'temperatures.max'
    } else if (variable[[x]] == 'minTemp') {
      variable[[x]] <- 'temperatures.min'
    } else if (variable[[x]] == 'maxRH') {
      variable[[x]] <- 'relativeHumidity.max'
    } else if (variable[[x]] == 'minRH') {
      variable[[x]] <- 'relativeHumidity.min'
    } else if (variable[[x]] == 'averageWind') {
      variable[[x]] <- 'wind.average'
    } else if (variable[[x]] == 'dayMaxWind') {
      variable[[x]] <- 'wind.dayMax'
    } else if (variable[[x]] == 'rollingavgppet') {
      variable[[x]] <- 'ppet'
      doRoll <- TRUE
    }
    
    #Confirm chosen variable is in the data structure
    if (any(grepl(pattern = variable[[x]]
                  ,x = colnames(dataToUse)
                  ,fixed = TRUE)) == FALSE) {
      stop("Input Variable is not from allowed list. Please use maxTemp, minTemp, 
           accumulatedGdd, accumulatedPet, accumulatedPpet, accumulatedPrecipitation,
           gdd, pet, precipitation, maxRH, minRH, solar,averageWind,dayMaxWind, 
           rollingavgppet, or if plotting climate indices one of the indices described
           in that functions documentation.")
    } else {
      varsToChart[[x]] <- c(paste0(variable[[x]],'.amount')
                            ,paste0(variable[[x]],'.average')
                            ,paste0(variable[[x]],'.stdDev'))
      
      variableNames[[x]] <- c("date"
                              ,"Current"
                              ,"LTN"
                              ,"LTNstddev")
    }
    
    #if the variable to be plotted is relevant and if the user wants to use effective precipitation, calculate new values
    if ((grepl(pattern = 'precipitation|precip|Ppet'
               ,x = variable[[x]]
               ,ignore.case = TRUE)) & e_precip == TRUE) {
      
      #if e_precip is set to true, bring in daily precip data and calculate accumulated
      #daily precipitation using either default or user-defined threshold
      
      dataToUse[,precipitation.amount.effective := precipitation.amount]
      
      dataToUse[precipitation.amount > e_threshold, precipitation.amount.effective := e_threshold]
      dataToUse[,ppet.amount.effective := precipitation.amount.effective / pet.amount]
      
      dataToUse[,accumulatedPrecipitation.amount.effective := cumsum(precipitation.amount.effective)]
      
      #dataToUse[,accumulatedPpet.amount.effective := cumsum(ppet.amount.effective)]
      dataToUse[,accumulatedPpet.amount.effective := cumsum(accumulatedPrecipitation.amount.effective/accumulatedPet.amount)]
      
      varsToChart[[x]] <- c(varsToChart[[x]],paste0(variable[[x]],'.amount.effective'))
      variableNames[[x]] <- c(variableNames[[x]], 'EffectiveCurrent')
    }
    
    if (any(grepl(pattern = 'precipitation.amount.effective'
                  ,x = colnames(dataToUse)
                  ,fixed = TRUE)) == TRUE) {
      if (all(dataToUse[!is.na(precipitation.amount) & !is.na(precipitation.amount.effective)
                        ,precipitation.amount == precipitation.amount.effective]) == TRUE & x == 1) {
        warning('The chosen setting for effective precipitation did not alter figure.  Disabling the use of effective precipitation for ',variable[[x]],'\n')
        
        varsToChart[[x]] <- grep(pattern = 'effective'
                                 ,x = varsToChart[[x]]
                                 ,ignore.case = TRUE
                                 ,value = TRUE
                                 ,invert = TRUE)
        
        variableNames[[x]] <- grep(pattern = 'effective'
                                   ,x = variableNames[[x]]
                                   ,ignore.case = TRUE
                                   ,value = TRUE
                                   ,invert = TRUE)
      }
    }
    
    ##set ylabel
    if (grepl(pattern = 'Gdd'
              ,x = variable[[x]]
              ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'GDDs'
    } else if (grepl(pattern = 'PPet'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'Index'
    } else if (grepl(pattern = 'Pet'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'mm'
    } else if (grepl(pattern = 'precipitation|sumOfPrecip|seasonTotalPrecip|precipSumExceedPercentile'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'mm'
    } else if (grepl(pattern = 'relativeHumidity|RH'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = '%'
    } else if (grepl(pattern = 'solar'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'Wh/m^2'
    } else if (grepl(pattern = 'temperatures|MaxTemp|MinTemp'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'Celsius'
    } else if (grepl(pattern = 'wind'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'm/s'
    } else if (grepl(pattern = 'maxLenDrySpell|maxLenWetSpell|numFrostDays|numSummerDays|numIcingDays|numTropicalNights|warmSpellDurIndex|coldSpellDurIndex|countDaysPrecipExceedAmount'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'Days'
    } else if (grepl(pattern = 'dailyTempRange'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'Celcius/Day'
    } else if (grepl(pattern = 'maxSingleDayPrecip'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'mm/Day'
    } else if (grepl(pattern = 'max5ConsDayPrecip'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'mm/(5_Days)'
    } else if (grepl(pattern = 'simplePrecipIntensityIndex'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = 'mm/(Wet Day))'
    } else if (grepl(pattern = 'percentDaysMinTempBelowQuantile|percentDaysMaxTempBelowQuantile|percentDaysMinTempAboveQuantile|percentDaysMaxTempAboveQuantile'
                     ,x = variable[[x]]
                     ,ignore.case = TRUE) == TRUE) {
      ylabel[[x]] = '% of Days'
    }

    
    #filter out relevant data
    chart_data[[x]] <- dataToUse[, c("date", varsToChart[[x]]),with = FALSE]
    
    #set common names of columns
    chart_data[[x]] <- setNames(chart_data[[x]], c(variableNames[[x]]))
    
    
    chart_data[[x]][,c('ymax'
                       ,'ymin'):= list(LTN + LTNstddev
                                       ,LTN - LTNstddev)]
    
    #for these variables, the y axis should not below zero
    if (grepl(pattern = 'temperatures|MaxTemp|MinTemp'
              ,x = variable[[x]]
              ,ignore.case = TRUE) == TRUE) {
      chart_data[[x]][ymin < 0, ymin := 0]
    }
    
    chart_data[[x]][,LTNstddev := NULL]
    variableNames[[x]] <- setdiff(variableNames[[x]]
                                  ,'LTNstddev')
    
    #if variable is set to "rollingavgppet", bring in daily precip and pet data 
    #and calculate rolling averages. If e_precip = TRUE, add effective precip
    
    if(doRoll == TRUE) {
      if(e_precip == TRUE & any(grepl(pattern = 'EffectiveCurrent'
                                      ,x = colnames(chart_data[[x]])
                                      ,fixed = TRUE))) {
        
        chart_data[[x]][,EffectiveCurrent := zoo::rollapply(EffectiveCurrent 
                                                            ,width = rolling_window 
                                                            ,align = "right"
                                                            ,FUN = mean
                                                            ,na.rm = TRUE
                                                            ,fill = NA)]
      }
      
      chart_data[[x]][,Current := zoo::rollapply(Current 
                                                 ,width = rolling_window 
                                                 ,align = "right"
                                                 ,FUN = mean
                                                 ,na.rm = TRUE
                                                 ,fill = NA)]
      
      chart_data[[x]][,LTN := zoo::rollapply(LTN
                                             ,width = rolling_window
                                             ,align = "right"
                                             ,FUN = mean
                                             ,na.rm = TRUE
                                             ,fill = NA)]
      
      #for plotting SD info
      chart_data[[x]][,ymax := zoo::rollapply(ymax
                                              ,width = rolling_window
                                              ,align = "right"
                                              ,FUN = mean
                                              ,na.rm = TRUE
                                              ,fill = NA)]
      chart_data[[x]][,ymin := zoo::rollapply(ymin
                                              ,width = rolling_window
                                              ,align = "right"
                                              ,FUN = mean
                                              ,na.rm = TRUE
                                              ,fill = NA)]
    
      #Choice of columns is arbitrary as they all have the same rolling window
      if (nrow(chart_data[[x]][!is.na(Current),]) > 0) {
        chart_data[[x]] <- chart_data[[x]][!is.na(Current),]
        warning('Rolling Aggregation Performed; Truncating data to date range with complete data\n')
      }
      
      #If EffectiveCurrent is the same as non adjusted slightly increase so both lines show on graph
      if ((any(grepl(pattern = 'EffectiveCurrent'
                     ,x = colnames(chart_data[[x]])
                     ,fixed = TRUE)) & mainGraphType == 'line') == TRUE) {
        if (all(chart_data[[x]][,Current == EffectiveCurrent]) == TRUE) {
          chart_data[[x]][,EffectiveCurrent := EffectiveCurrent - .1]
        }
      }
    }
  
    if (length(variable) > 1) {
      #Add identifying variable information to variable names
      variableNames[[x]][-1] <- paste0(variableNames[[x]][-1],'-',variable[[x]])
      
      setnames(chart_data[[x]]
               ,setdiff(colnames(chart_data[[x]])[-1],c('ymin','ymax'))
               ,paste0(setdiff(colnames(chart_data[[x]])[-1],c('ymin','ymax')),'-',variable[[x]]))
    }

    #If the date column is a date we want to cast it to a date object.  If the date column is just years don't
    if (all(chart_data[[x]][,grepl('^\\d{4}$',date)]) == FALSE) {
      #convert character date column to Date
      chart_data[[x]][,date :=as.Date(date)]
      isTrendLine <- FALSE
    } else {
      variableNames[[x]] <- setdiff(variableNames[[x]],'LTN')
      isTrendLine <- TRUE
    }

    
    #change data format from wide to long
    chart_data_long[[x]] <- 
      tidyr::gather(chart_data[[x]][,variableNames[[x]],with = FALSE] 
                    ,key = Variable 
                    ,value = measure 
                    ,2:ncol(chart_data[[x]][,variableNames[[x]],with = FALSE])) %>%
      as.data.table(.)
    
    
    chart_data_long[[x]] <- merge(chart_data_long[[x]]
                                  ,chart_data[[x]][,list(date,ymin,ymax)]
                                  ,by = 'date')
    
    setkey(chart_data_long[[x]],Variable,date)
    
  }
  
  #if title is not given by user, set it to date range + variable
  if (is.null(title)) {
    title <- paste0(paste0(variable.orig,collapse = ' & '), "\nfrom ", min(dataToUse$date), " to ", max(dataToUse$date),'\n')
    
    if (!is.null(daysToAggregateOver)) {
      title <- paste0(title,paste0(daysToAggregateOver,' Day Aggregation\n'))
    }
  }
  
  ylabel_unique <- unique(unlist(ylabel))
  
  #Check to make sure we are not trying to plot more than two different types of units
  if (length(ylabel_unique) > 2) {
    stop('Trying to plot more variables than can be displayed with only two different types of units\n')
  }
  
  #Because of how ggplot functions, we need to calculate the scaling factor between the two axis
  for (x in 1:length(ylabel_unique)) {
    currentIndices <- which(ylabel == ylabel_unique[x])
  
    rangeToUse[[currentIndices]] <- rbindlist(chart_data_long[currentIndices])[,quantile(x = measure,na.rm = TRUE,probs = c(0,1))]
    
    if (x > 1) {
      
      for (z in 1:length(currentIndices)) {
        
        chart_data_long[[currentIndices[z]]][,measure := scales::rescale(x = measure
                                                                         ,from = c(min(measure
                                                                                       ,na.rm = TRUE)
                                                                                   ,max(measure
                                                                                        ,na.rm = TRUE))
                                                                         ,to = rangeToUse[[1]])]
      }
    }
  }
  
  #############################################################################
  #set color scale based on # of vars to chart. 
  for (q in 1:length(chart_data_long)) {
    if (q == 1) {
      colorScheme[[1]] <- data.table(variable = c('Current'
                                                  ,'EffectiveCurrent'
                                                  ,'LTN'
                                                  ,SD_label)
                                     # colors selected from 8-class diverging, colorbline-safe palette
                                     # http://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=8 
                                     ,color = c("#4575b4" # current var #1 - dark blue
                                                ,"#abd9e9" # effective current var #1 - light blue
                                                ,"#fdae61" # LTN var #1 line - orange 
                                                ,"#fdae61")) # LTN var #1 shading - orange
    } else if (q == 2) {
      colorScheme[[2]] <- data.table(variable = c('Current'
                                                  ,'EffectiveCurrent'
                                                  ,'LTN')
                                     ,color = c("#000000" # current var #2 - black
                                                ,"#fee090" #  effective current var # 2 - yellow 
                                                ,"#d73027")) # LTN var #  - red
    } else if (q == 3) {
      colorScheme[[3]] <- data.table(variable = c('Current'
                                                  ,'EffectiveCurrent'
                                                  ,'LTN')
                                     ,color = c("#862d59" # current var #3 - dark pink
                                                ,"#cc6699" #  effective current var #3 - light pink 
                                                ,"#33cc33")) # LTN var #3  - green
    } else if (q == 4) {
      colorScheme[[4]] <- data.table(variable = c('Current'
                                                  ,'EffectiveCurrent'
                                                  ,'LTN')
                                     ,color = c("#996600" # current var #3 - burn orange
                                                ,"#ffaa00" #  effective current var #3 light orange 
                                                ,"#9933ff")) # LTN var #  - purple
    }
    
    if (isTrendLine == TRUE) {
      colorScheme[[q]] <- colorScheme[[q]][!grep('LTN',variable)]
    }
  }
  
  for (x in 1:length(chart_data_long)) {
    currentVars <- unique(chart_data_long[[x]][,Variable])
    
    currentVars.split <- strsplit(x = currentVars
                                  ,split = '-'
                                  ,fixed = TRUE)
    
    currentVars.split = unlist(lapply(currentVars.split, function(l) l[[1]]))
    
    if (includeSTD == TRUE & x ==1)  {
      currentVars <- c(currentVars,SD_label) 
      currentVars.split <- c(currentVars.split,SD_label)
    }
    
    
    currentVars.dt <- data.table(currentVars,currentVars.split)
    
    colorScheme[[x]] <- merge(colorScheme[[x]]
                              ,currentVars.dt
                              ,by.x = 'variable'
                              ,by.y = 'currentVars.split')
  }
  
  colorScheme <- rbindlist(colorScheme)
  
  # Modify the Breaks.strings to set the order of the arguments you want in the legends
  colorScheme.string <- paste(colorScheme[,paste0('\"',currentVars,'\" = \"', color,'\"')],collapse = ',\n')
  colorSchemeBreaks.string <- paste(colorScheme[,paste0('\"',currentVars,'\"')],collapse = ', ')
  
  # Order the legend items - WITHOUT STANDARD DEVIATION entry 
  colorSchemeBreaks.string <- paste(colorScheme[variable != SD_label,paste0('\"',currentVars,'\"')],collapse = ', ')
  colorFillBreaks.string <- paste(colorScheme[variable != SD_label,paste0('\"',currentVars,'\"')],collapse = ', ')
  
  # Uncomment this line if you want SD ribbon to appear in the fill legend entry
  #colorFillBreaks.string <- paste(colorScheme[,paste0('\"',currentVars,'\"')],collapse = ', ')
  
  eval(parse(text = paste0('colorScaleToUse <- scale_color_manual(values = c(',colorScheme.string,'),breaks = c(',colorSchemeBreaks.string,'))')))
  #Do not include the SD info for the fill in the legend
  eval(parse(text = paste0('colorFillToUse  <- scale_fill_manual(values = c(',colorScheme.string,'),breaks = c(',colorFillBreaks.string,'))')))
  
  
  ############################################################################                                                        
  
  currentIndices <- which(ylabel == ylabel_unique[1])
  
  dataToPlot <- rbindlist(chart_data_long[currentIndices])
  #make chart based on appropriate graph type
  chart <- 
    ggplot()
  
  if (mainGraphType == 'line') {
    #plot actual lines on top
    chart <- 
      chart +
      geom_ribbon(data = dataToPlot
                  ,aes(x = date
                       ,ymin = measure
                       ,ymax = measure
                       ,fill = Variable
                       ,colour = Variable)
                  ,size = line_width) 
    
    numFillsLegend <- chart_data_long[[1]][,length(unique(Variable))]
  } else {
    chart <- 
      chart +
      geom_col(data = dataToPlot[!grepl(pattern = 'LTN',x = Variable,fixed = TRUE)], 
               aes(x = date
                   ,y = measure
                   ,fill = Variable)
               ,position = 'dodge'
               ,na.rm = TRUE) +
      geom_ribbon(data = dataToPlot[grepl(pattern = 'LTN',x = Variable,fixed = TRUE)]
                  ,aes(x = date
                       ,ymin = measure
                       ,ymax = measure
                       ,colour = Variable
                       ,fill = Variable)
                  ,na.rm = TRUE
                  ,size = line_width) 
    
    numFillsLegend <- dataToPlot[,length(unique(Variable))]
  }
  
  #include SD info for main variable
  if (includeSTD == TRUE) {
    
    for (z in 1:length(currentIndices)) {
      chart <- 
        chart + 
        geom_ribbon(data = chart_data_long[[currentIndices[z]]]
                    ,aes(x = date
                         ,ymin = ymin
                         ,ymax = ymax
                         ,fill = SD_label)
                    ,alpha = 0.3 # adjust transparency of SD DEV shading
                    ,linetype = "blank") 
    }
    
    
    numFillsLegend <- numFillsLegend + length(currentIndices)
  }
  
  #add in line charts for other variables
  #linetypes <- c("solid", "dotted", "dashed")
  if (length(ylabel_unique) > 1) {
    for (x in 2:length(ylabel_unique)) {
      currentIndices <- which(ylabel == ylabel_unique[x])
      
      chart <-
        chart +
        geom_ribbon(data = rbindlist(chart_data_long[currentIndices])
                    ,aes(x = date
                         ,ymin = measure
                         ,ymax = measure
                         ,colour = Variable
                         ,fill = Variable)
                    ,size = line_width
                    #,linetype = linetypes[x] # change line type for sunsequent variables
        ) +
        #we need to map the values back to the original scale for plotting on
        #the side if Multiple values were to exist for currentIndices they will
        #have the same info stored, hence we can take the first
        scale_y_continuous(sec.axis = sec_axis(~ scales::rescale(x = .
                                                                 ,from= rangeToUse[[1]]
                                                                 ,to = rangeToUse[currentIndices][[1]])
                                               ,name = unique(unlist(ylabel[currentIndices])))) 
      
      numFillsLegend <- numFillsLegend + dataToPlot[,length(unique(Variable))]
    }
  }
  
  #format figure
  chart <- 
    chart +
    colorScaleToUse +
    colorFillToUse +
    # plot theme: https://ggplot2.tidyverse.org/reference/ggtheme.html
    theme_bw() + 
    
    # format the legend 
    theme(
      # legend on RIGHT side of chart with items stacked vertically ---
      #legend.position="right" # place legend at right side of chart
      #,legend.box = "vertical"
      #,legend.direction= "vertical"
      
      # legend on BOTTOM of chart with items stacked horizontally ---
      legend.position = "bottom" # place legend at bottom of chart
      ,legend.box = "horizontal"
      ,legend.direction= "horizontal"
      ,legend.title = element_blank()
      ,legend.spacing.x = unit(.1, 'cm'))  +
    
    # main chart title 
    ggtitle(title) + # add  main title to the chart 
    theme(plot.title = element_text(size=size_font_main_title)) # main title font size  
  #theme(legend.spacing.y = unit(-0.18, "cm"))
  
  if (isTrendLine == FALSE) {
    # the next two lines may be commented out if the vertical current date line 
    # is not desired
    chart <- 
      chart + 
      geom_vline(xintercept = as.numeric(Sys.Date())
               ,linetype = "dashed") +
      # Reorder legend entries
      guides(
        fill = guide_legend(#ncol = numFillsLegend
          nrow = ceiling(length(chart_data_long)/2)
          ,order = 1), 
        #colour = guide_legend(ncol =  numColorsLegend 
        #                      ,order = 2)) + 
        colour = 'none') + 
      # set the y-axis labels 
      labs(y = ylabel[[1]]) + 
      # format the text color, size, angle, and face for x- and y- axes. 
      # x-axis labels 
      theme(axis.text.x = element_text(color = "grey20", # font color 
                                       size = size_font_axis_labels, # font size 
                                       angle = 45,       # font angle 
                                       hjust = 1,        # horizontal adjustment
                                       face = "plain"),  # font type "plain", "bold" 
            # y-axis labels 
            axis.text.y = element_text(color = "grey20", 
                                       size = size_font_axis_labels, 
                                       face = "plain"),
            # y-axis titles 
            axis.title.y = element_text(color = "grey20", 
                                        size = size_font_axis_titles, 
                                        face = "bold"),
            # turn off the x-axis title 
            axis.title.x=element_blank()) 
  } else {
    yearsPresent <- dataToPlot[,unique(date)]
    
    if (annotationsWhichSide == 'left') {
      xCoord_annotation <- dataToPlot[,min(date,na.rm = TRUE)]
      xCoord_hJust <- 0
    } else {
      xCoord_annotation <- dataToPlot[,max(date,na.rm = TRUE)]
      xCoord_hJust <- 1
    }
    
    chart <- 
      chart +
      # Reorder legend entries
      guides(
        fill = 'none', 
        colour = 'none') +
      scale_x_continuous(breaks=yearsPresent[seq(1,length(yearsPresent),2)]) +
      # set the y-axis labels 
      labs(y = ylabel[[1]]
           ,x = 'Year') + 
      # format the text color, size, angle, and face for x- and y- axes. 
      # x-axis labels 
      theme(axis.text.x = element_text(color = "grey20", # font color 
                                       size = size_font_axis_labels, # font size 
                                       angle = 45,       # font angle 
                                       hjust = 1,        # horizontal adjustment
                                       face = "plain"),  # font type "plain", "bold" 
            # y-axis labels 
            axis.text.y = element_text(color = "grey20", 
                                       size = size_font_axis_labels, 
                                       face = "plain"),
            # y-axis titles 
            axis.title.y = element_text(color = "grey20", 
                                        size = size_font_axis_titles, 
                                        face = "bold"),
            # x-axis titles 
            axis.title.x = element_text(color = "grey20", 
                                        size = size_font_axis_titles, 
                                        face = "bold")) +
      stat_smooth(aes(x = date
                      ,y = measure)
                  ,method=lm,
                  ,data = dataToPlot
                  ,se = FALSE) +
      ggpmisc::stat_fit_glance(method = 'lm'
                               ,data = dataToPlot
                      ,label.y = "top"
                      ,label.x = annotationsWhichSide
                      ,method.args = list(formula = y ~ x)
                      ,mapping = aes(x = date
                                     ,y = measure
                                     ,label = sprintf('italic(P)~"="~%.2g',
                                                    stat(p.value)))
                      ,parse = TRUE
                      ,size = 5) +
      annotate(
        geom = "text", x = xCoord_annotation, y = dataToPlot[,min(measure)], 
        label = paste0('CV = ',round(dataToPlot[,sd(measure,na.rm = TRUE)/mean(measure,na.rm = TRUE)],2))
        ,hjust = xCoord_hJust , vjust = -1.5, size = 5) +
      annotate(
        geom = "text", x = xCoord_annotation, y = dataToPlot[,min(measure)], 
        label = paste0('Average = ',round(dataToPlot[,mean(measure,na.rm = TRUE)],2))
        ,hjust = xCoord_hJust, vjust = 0, size = 5)
  }
  
  if (any(is.na(yAxisLimits)) == FALSE) {
    chart <- 
      chart + 
      coord_cartesian(ylim = yAxisLimits)
  }
  
  return(chart)
}