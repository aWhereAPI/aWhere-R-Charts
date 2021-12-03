#' @title processClimateIndices
#'
#' @description \code{processClimateIndices} Process aWhere weather data to generate climate indices
#'
#' @details This function extends the aWhere charts package to generate the ETCCDI climate
#' change indices (reference below).  Most indices are implement with the default behavior 
#' described int he publication.  Default values of the indices can be overriden using the 
#' "indexSpecificValue" parameter.
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
#'   percentDaysMinTempAboveQuantile, percentDaysMaxTempAboveQuantile, maxOfAccumulatedGdd,
#'   maxOfAccumulatedPet, sumOfGDD, sumOfPET, sumOfPrecip, sumOfSolar, sumOfPOverPET, averageMaxTemp, 
#'   averageMinTemp, averageMaxRH, averageMinRH, averageWind, and maxWindGust
#'   
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
#' @param indexSpecificValue For the Climate Indices this tool can plot the user
#'   can override the default value of the index using this parameter (optional)
#'
#' @import dplyr
#' @import zoo
#' @import data.table
#'
#' @return list
#'
#' @examples
#' \dontrun{processClimateIndices(data = weather_df
#'                                      ,variable = "seasonTotalPrecip"
#'                                      ,season.monthDay_start = '09-01'
#'                                      ,season.monthDay_end = '11-30'
#'                                      ,years.LTN = seq(2010,2019,1)
#'                                      ,e_precip = TRUE
#'                                      ,e_threshold = 10)}
#'
processClimateIndices <- function(dataToUse
                                  ,variable
                                  ,variable_rightAxis = NULL
                                  ,startYearOfSeasonToPlot = NULL
                                  ,season.monthDay_start = '01-01'
                                  ,season.monthDay_end = '12-31'
                                  ,years.LTN = seq(2006,2020,1)
                                  ,title = NULL
                                  ,e_precip = FALSE 
                                  ,e_threshold = 35 
                                  ,indexSpecificValue = NULL) {

  suppressWarnings(dataToUse[,seasonNumber := NULL])
  
  #check to make sure that they specified enough years for LTN calculation
  if (length(years.LTN) < 10) {
    stop('You must specify a minimum of 10 years for calculating the Long Term Normal.
         Please revise the "years.LTN" paramameter in the function call\n')
  }
  
  #We are going to store the pulled data in the aWhereEnv so that the user does
  #not lose it but we will not force them to update their data object.  However
  #we will give them the option of doing so
  if (exists('climateIndex_LTNData',where = awhereEnv75247,inherits = FALSE) == TRUE) {
    
    temp <- awhereEnv75247$climateIndex_LTNData
    
    #DM NOTE: MAY WANT TO CHANGE THIS TO STORING MANY LOCATIONS AND THEN SUBSETTING SO IT WORKS WITH JC's LOOPING
    
    #check to make sure they are pulling data for the same location
    lat.prev <- unique(temp[,latitude])
    lon.prev <- unique(temp[,longitude])
    
    lat.now <- unique(dataToUse[,latitude])
    lon.now <- unique(dataToUse[,longitude])
    
    #If data is for the same location we can add.  Its not a problem if its for a different date range
    if (length(lat.prev) == 1 & length(lon.prev) == 1 & 
        lat.prev == lat.now & lon.prev == lon.now) {
      dataToUse <- rbind(dataToUse,temp,use.names = TRUE,fill = TRUE)
      
      dataToUse <- unique(dataToUse, by = 'date')
    }
  }
  
  if (exists('generateaWhereDataset.LTN_years',where = awhereEnv75247,inherits = FALSE) == TRUE) {
    
    year_start <- awhereEnv75247$generateaWhereDataset.LTN_years[1]
    year_end <- awhereEnv75247$generateaWhereDataset.LTN_years[2]
    
    #In case the object in the aWhere env gets messed up
    if (is.null(year_start) == TRUE) {
      year_start <- min(years.LTN)
    }
    
    if (is.null(year_end) == TRUE) {
      year_end <- max(years.LTN)
    }
  } else {
    year_start <- min(years.LTN)
    year_end <- max(years.LTN)
  }
  
  ############################################################################
  #Logic for making sure we get all the needed daily data
  #yearsPresent <- dataToUse[,unique(lubridate::year(date))]
  #yearsNeeded <- sort(unique(c(yearsPresent,years.LTN)))
  
  
  yearsNeeded <- sort(unique(years.LTN,startYearOfSeasonToPlot))
  
  
  #handles data that goes over Jan 1st
  if (paste0('2020-',season.monthDay_start) > paste0('2020-',season.monthDay_end)) {
    year.increment <- 1
  } else {
    year.increment <- 0
  }
  
  #Create data structure of start and end date of each "season" that needs to be in dataset
  allDaysNeeded <-  data.table(start = as.Date(paste0(yearsNeeded,'-',season.monthDay_start)),end = as.Date(paste0(yearsNeeded + year.increment,'-',season.monthDay_end)))
  allDaysNeeded.list <- list()
  
  for (x in 1:nrow(allDaysNeeded)) {
    allDaysNeeded.list[[x]] <- seq.Date(allDaysNeeded[x,start],allDaysNeeded[x,end],by = 'days')
  }
  
  allDaysNeeded <- as.Date(unlist(allDaysNeeded.list),origin = '1970-01-01')
  
  allDaysNeeded <- allDaysNeeded[allDaysNeeded <= (Sys.Date() + 13)]
  
  if (length(setdiff(allDaysNeeded,dataToUse[,date])) > 0) {
    cat(paste0('This function requires the daily data over all years the index is to be calculated for. This function
                can automatically request this data.'))
    makeAPICalls <- readline("Do you wish to proceed with the current request? Type yes to begin API calls: ")
    
    if (tolower(makeAPICalls) != 'yes') {
      stop('User Input indicated they did not want to proceed with making API Calls \n')
    }
  }
  
  #we need to check if we have all the LTN data needed and if we don't request
  #it.  Even if we are not using for this function this is required to have the
  #user have a complete dataset.  We only need to request this data once however. 
  allDaysNeeded.LTN <- unique(substr(x = allDaysNeeded,6,10))
  
  #Figure out which days of LTN data are still needed
  allDaysNeeded.LTN <- setdiff(allDaysNeeded.LTN,dataToUse[,day])
  
  ##################################################################################
  #Loop over the years we need data and get it in the minimum number of API
  #calls.  Only request the LTN data once
  
  temp <- list()
  for (x in 1:length(yearsNeeded)) {
    
    currentYear <- yearsNeeded[x]
    startDate <- as.Date(paste0(currentYear,'-',season.monthDay_start))
    endDate <- as.Date(paste0(currentYear + year.increment,'-',season.monthDay_end))
    
    if (endDate > (Sys.Date() + 13)) {
      endDate <- Sys.Date() + 13
    }
    
    daysNeeded <- seq.Date(startDate,endDate,by = 'days')
    
    #Figure out which days are not currently present in the dataset
    daysNeeded.pull <- as.Date(setdiff(daysNeeded,dataToUse[,date]),origin = '1970-01-01')
    
    #determine if this pull contains needed LTN data
    daysNeeded.pull.LTN <- intersect(allDaysNeeded.LTN,unique(substr(x = daysNeeded.pull,6,10)))
    #These are the days we are still needed LTN Data for on the next iteration
    allDaysNeeded.LTN <- setdiff(allDaysNeeded.LTN,daysNeeded.pull.LTN )
    
    if (length(daysNeeded.pull.LTN) > 0) {
      requestLTNData <- TRUE
    } else {
      requestLTNData <- FALSE
    }
    
    if (length(daysNeeded.pull) > 0) {
      startDate.dataPull <- min(daysNeeded.pull)
      endDate.dataPull <- max(daysNeeded.pull)
      
      cat(paste0('    Requesting data between ',startDate.dataPull,' and ',endDate.dataPull,'\n'))
      
      if (requestLTNData == TRUE) {
        startYear.value <- year_start  #these should exist because the getaWhereDataset fxn has to be run before this
        endYear.value <- year_end
      } else {
        startYear.value <- NULL
        endYear.value <- NULL
      }
      
      dt <- 
        generateaWhereDataset(lat = dataToUse[,unique(latitude)]
                              ,lon = dataToUse[,unique(longitude)]
                              ,day_start = startDate.dataPull
                              ,day_end = endDate.dataPull
                              ,year_start = startYear.value
                              ,year_end = endYear.value
                              ,verbose = FALSE
                              ,appendPrevDataPull = FALSE)
      
    } else {
      dt <-
        data.frame(latitude = 0
                   ,longitude = 0
                   ,date = Sys.Date()
                   ,day = '0'
                   ,accumulatedGdd.amount = 0
                   ,accumulatedGdd.average = 0
                   ,accumulatedGdd.stdDev = 0
                   ,accumulatedPet.amount = 0
                   ,accumulatedPet.average = 0
                   ,accumulatedPet.stdDev = 0
                   ,accumulatedPpet.amount = 0
                   ,accumulatedPpet.average = 0
                   ,accumulatedPpet.stdDev = 0
                   ,accumulatedPrecipitation.amount = 0
                   ,accumulatedPrecipitation.average = 0
                   ,accumulatedPrecipitation.stdDev = 0
                   ,gdd.amount = 0
                   ,gdd.average = 0
                   ,gdd.stdDev = 0
                   ,pet.amount = 0
                   ,pet.average = 0
                   ,pet.stdDev = 0
                   ,ppet.amount = 0
                   ,ppet.average = 0
                   ,ppet.stdDev = 0
                   ,precipitation.amount = 0
                   ,precipitation.average = 0
                   ,precipitation.stdDev = 0
                   ,relativeHumidity.max.amount = 0
                   ,relativeHumidity.max.average = 0
                   ,relativeHumidity.max.stdDev = 0
                   ,relativeHumidity.min.amount = 0
                   ,relativeHumidity.min.average = 0
                   ,relativeHumidity.min.stdDev = 0
                   ,solar.amount = 0
                   ,solar.average = 0
                   ,solar.stdDev = 0
                   ,temperatures.max.amount = 0
                   ,temperatures.max.average = 0
                   ,temperatures.max.stdDev = 0
                   ,temperatures.min.amount = 0
                   ,temperatures.min.average = 0
                   ,temperatures.min.stdDev = 0
                   ,wind.average.amount = 0
                   ,wind.average.average = 0
                   ,wind.average.stdDev = 0
                   ,wind.dayMax.amount = 0
                   ,wind.dayMax.average = 0
                   ,wind.dayMax.stdDev = 0)
      
      dt <- dt[0,]
    }
    
    temp[[x]] <- rbind(dataToUse[date %in% daysNeeded,colnames(dt),with = FALSE],dt,use.names = TRUE)
    setkey(temp[[x]],date)
    
    #temp[[x]][,seasonNumber := x]
  }
  
  temp <- rbindlist(temp,use.names = TRUE,fill = TRUE)
  
  #####################################################################################
  #We now need to assign the LTN values to all appropriate rows instead of just the one the data was pulled with
  cols.LTN <- 
    grep(pattern = '.amount|latitude|longitude|date|day|seasonNumber'
         ,x = colnames(temp)
         ,value = TRUE
         ,invert = TRUE)
  
  temp.LTN <- temp[,lapply(.SD,mean,na.rm = TRUE),by = 'day',.SDcols = cols.LTN]
  
  temp <- 
    merge(temp[,setdiff(colnames(temp),cols.LTN),with = FALSE]
          ,temp.LTN
          ,by = 'day')
  
  setkey(temp,date)
  
  ####################################################################################################
  #Assign season number properly
  for (seasonCounter in 1:length(years.LTN)) {
    
    seasonStart.current <- paste0(years.LTN[seasonCounter],'-',season.monthDay_start)
    seasonEnd.current <- paste0(years.LTN[seasonCounter],'-',season.monthDay_end)
    
    if (seasonEnd.current < seasonStart.current) {
      seasonEnd.current <- paste0((years.LTN[seasonCounter] + 1),'-',season.monthDay_end)
    }
    
    temp[date >= seasonStart.current & date <= seasonEnd.current, seasonNumber := seasonCounter]
    
  }
  
  ####################################################################################################
  #we don't want to drop the data that was in the dataset even if we wont use it for this index
  cols.origOrder <- colnames(dataToUse)
  dataToUse.all <- rbind(temp,dataToUse,use.names = TRUE,fill = TRUE)
  dataToUse.all <- unique(dataToUse.all,by = 'date')
  setkey(dataToUse.all,'date')
  setcolorder(dataToUse.all,cols.origOrder)
  
  ####################################################################################################
  #We need to update something so that the user doesn't lose all the data that was pulled in this function
  if (exists('climateIndex_LTNData',where = awhereEnv75247,inherits = FALSE) == TRUE) {
    if (bindingIsLocked('climateIndex_LTNData',awhereEnv75247) == TRUE) {
      unlockBinding('climateIndex_LTNData',awhereEnv75247)
    }
  }
  
  if (exists('generateaWhereDataset.LTN_years',where = awhereEnv75247,inherits = FALSE) == TRUE) {
    if (bindingIsLocked('generateaWhereDataset.LTN_years',awhereEnv75247) == TRUE) {
      unlockBinding('generateaWhereDataset.LTN_years',awhereEnv75247)
    }
  }
  
  awhereEnv75247$climateIndex_LTNData <- dataToUse.all[,cols.origOrder,with = FALSE]
  awhereEnv75247$generateaWhereDataset.LTN_years <- c(year_start,year_end)  
  
  lockBinding('climateIndex_LTNData',awhereEnv75247)
  lockBinding('generateaWhereDataset.LTN_years',awhereEnv75247)
  
  ############################################################################
  #Subset the data if the user specifies
  #using 2010 is arbitrary
  daysToKeep <- 
    seq.Date(as.Date(paste0(2010,'-',season.monthDay_start)),as.Date(paste0(2010 + year.increment,'-',season.monthDay_end)),by = 'days') %>%
    substr(.,6,10) %>%
    unique(.)
  
  dataToUse <- dataToUse.all[day %in% daysToKeep]
  dataToUse <- dataToUse[!is.na(seasonNumber),]
  
  dataToUse[,seasonNumber_startYear := min(lubridate::year(date)),by = 'seasonNumber']
  
  ##################################################################################
  #if the variable to be plotted is relevant and if the user wants to use effective precipitation, calculate new values
  if (e_precip == TRUE) {
    
    #if e_precip is set to true, bring in daily precip data and calculate accumulated
    #daily precipitation using either default or user-defined threshold
    
    dataToUse[precipitation.amount > e_threshold, precipitation.amount := e_threshold]
    dataToUse[,ppet.amount:= precipitation.amount/ pet.amount]
    
    #NEED TO CODE THIS PORTION
    dataToUse[,accumulatedPrecipitation.amount := cumsum(precipitation.amount),by = 'seasonNumber']
    
    dataToUse[,accumulatedPpet.amount := cumsum(accumulatedPrecipitation.amount/accumulatedPet.amount),by = 'seasonNumber']
  }
  
  ##################################################################################
  #We don't need top do temporal aggregations here because it can be done in the
  #charts fxn.  Now has logic to know if should use "season number" column for a
  #"by" argument
  
  ####################################################################################################
  #We don't need to do rolling calculations here because it can be done in the charts fxn
  
  ####################################################################################################
  #for each variable to be plotted, loop through to create data structures for visualization
  variable.all <- variable
  
  if (is.null(variable_rightAxis) == FALSE) {
    variable.all <- c(variable,variable_rightAxis)
  } else {
    variable.all <- c(variable,NULL)
  }
  
  longname <- copy(variable.all)
  
  ##################################################################################
  #Place code for the specific Indices Here
  for (z in 1:length(variable.all)) {
    if (grepl(pattern = 'maxLenDrySpell|Maximum Length of Dry Spell|maxLenWetSpell|Maximum Length of Wet Spell'
              ,x = variable.all[z]
              ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the index 'Maximum Length of Dry Spell' and 'Maximum Length of Wet Spell'")
      } 
      
      dataToUse[,c('tempCol.current') := FALSE]
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 1
      }
      index.title <- paste0('Threshhold set at ',indexSpecificValue,'mm\n')
      
      if (grepl(pattern = 'maxLenDrySpell|Maximum Length of Dry Spell'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        variable.all[z] <- 'maxLenDrySpell'
        longname[z] <- 'Maximum Length of Dry Spell'
        
        dataToUse[precipitation.amount < indexSpecificValue,tempCol.current := TRUE]
      } else {
        variable.all[z] <- 'maxLenWetSpell'
        longname[z] <- 'Maximum Length of Wet Spell'
        
        dataToUse[precipitation.amount > indexSpecificValue,tempCol.current := TRUE]
      }
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      #Count counts of consecutive entries
      dataToUse[, counter.current := as.numeric(rowid(rleid(tempCol.current))),by = 'seasonNumber']
      
      #Set FALSE entries to NA
      dataToUse[tempCol.current == FALSE, counter.current := as.numeric(NA)]
      
      #initialize the column so that the line below doesn't fail if there are no valid rows to act on
      dataToUse[,group.current := as.numeric(NA)]
      
      #Creating a grouping variable for each TRUE period
      dataToUse[tempCol.current == TRUE & counter.current == 1, group.current := 1:.N, by = 'seasonNumber']
      
      #Carry grouping variable forwards
      dataToUse[,group.current := na.locf(group.current,na.rm = FALSE,fromLast = FALSE), by = 'seasonNumber']
      
      #Carry grouping variable backwards
      dataToUse[,group.current := na.locf(group.current,na.rm = FALSE,fromLast = TRUE), by = 'seasonNumber']
      
      #Find the max dry length for each period
      suppressWarnings(dataToUse[,counter.current := max(counter.current,na.rm = TRUE),by = c('group.current','seasonNumber')])
      
      dataToUse[counter.current == Inf | counter.current == -Inf, counter.current := 0]
      
      suppressWarnings(dataToUse[,c('tempCol.current','tempCol.ltn','group.current') := NULL])
      
      if (grepl(pattern = 'maxLenDrySpell'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        
        setnames(dataToUse,c('counter.current'),c('maxLenDrySpell.amount'))
        
        dataToUse[,maxLenDrySpell.stdDev := sd(maxLenDrySpell.amount,na.rm = TRUE),by = 'day']
        dataToUse[,maxLenDrySpell.average := mean(maxLenDrySpell.amount), by = 'day']
        
        dataToUse[,maxLenDrySpell.average := mean(maxLenDrySpell.average,na.rm = TRUE), by = 'day']
        
      } else {
        
        setnames(dataToUse,c('counter.current'),c('maxLenWetSpell.amount'))
        
        dataToUse[,maxLenWetSpell.stdDev := sd(maxLenWetSpell.amount,na.rm = TRUE),by = 'day']
        dataToUse[,maxLenWetSpell.average := mean(maxLenWetSpell.amount), by = 'day']
        
        dataToUse[,maxLenWetSpell.average := mean(maxLenWetSpell.average,na.rm = TRUE), by = 'day']
        
      }
      
    } else if (grepl(pattern = 'numFrostDays|Number of Frost Days'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.min'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Minimum Temperature data required to calculate the index 'Number of Frost Days'")
      }
      
      variable.all[z] <- 'numFrostDays'
      longname[z] <- 'Number of Frost Days'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 0
      }
      index.title <- paste0('Frost Day temperature set at ',indexSpecificValue,'C\n')
      
      dataToUse[,isFrostDay := FALSE]
      dataToUse[temperatures.min.amount < indexSpecificValue, isFrostDay := TRUE]
      dataToUse[,numFrostDays.amount := cumsum(isFrostDay),by = 'seasonNumber']
      
      dataToUse[,numFrostDays.stdDev := sd(numFrostDays.amount),by = 'day']
      dataToUse[,numFrostDays.average := mean(numFrostDays.amount), by = 'day']
      
      dataToUse[,numFrostDays.average := mean(numFrostDays.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,isFrostDay := NULL]
    } else if (grepl(pattern = 'numSummerDays|Number of Summer Days'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Maximum Temperature data required to calculate the index 'Number of Summer Days'")
      } 
      
      variable.all[z] <- 'numSummerDays'
      longname[z] <- 'Number of Summer Days'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 25
      }
      index.title <- paste0('Summer day temperature set at ',indexSpecificValue,'C\n')
      
      dataToUse[,isSummerDay := FALSE]
      dataToUse[temperatures.max.amount > indexSpecificValue, isSummerDay := TRUE]
      dataToUse[,numSummerDays.amount := cumsum(isSummerDay),by = 'seasonNumber']
      
      dataToUse[,numSummerDays.stdDev := sd(numSummerDays.amount),by = 'day']
      dataToUse[,numSummerDays.average := mean(numSummerDays.amount), by = 'day']
      
      dataToUse[,numSummerDays.average := mean(numSummerDays.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,isSummerDay := NULL]
    } else if (grepl(pattern = 'numIcingDays|Number of Icing Days'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Maximum Temperature data required to calculate the index 'Number of Icing Days'")
      } 
      
      variable.all[z] <- 'numIcingDays'
      longname[z] <- 'Number of Icing Days'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 0
      }
      index.title <- paste0('Icing temperature set at ',indexSpecificValue,'C\n')
      
      dataToUse[,isIcingDay := FALSE]
      dataToUse[temperatures.max.amount < indexSpecificValue, isIcingDay := TRUE]
      dataToUse[,numIcingDays.amount := cumsum(isIcingDay),by = 'seasonNumber']
      
      dataToUse[,numIcingDays.stdDev := sd(numIcingDays.amount),by = 'day']
      dataToUse[,numIcingDays.average := mean(numIcingDays.amount), by = 'day']
      
      dataToUse[,numIcingDays.average := mean(numIcingDays.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,isIcingDay := NULL]
    } else if (grepl(pattern = 'numTropicalNights|Number of Tropical Nights'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.min'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Minimum Temperature data required to calculate the index 'Number of Tropical Nights'")
      } 
      
      variable.all[z] <- 'numTropicalNights'
      longname[z] <- 'Number of Tropical Nights'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 20
      }
      index.title <- paste0('Nightime temperature set at ',indexSpecificValue,'C\n')
      
      dataToUse[,isTropicalNight := FALSE]
      dataToUse[temperatures.min.amount > indexSpecificValue, isTropicalNight := TRUE]
      dataToUse[,numTropicalNights.amount := cumsum(isTropicalNight),by = 'seasonNumber']
      
      dataToUse[,numTropicalNights.stdDev := sd(numTropicalNights.amount),by = 'day']
      dataToUse[,numTropicalNights.average := mean(numTropicalNights.amount), by = 'day']
      
      dataToUse[,numTropicalNights.average := mean(numTropicalNights.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,isTropicalNight := NULL]
    } else if (grepl(pattern = 'minOfMaxTemp|Minimum of Maximum Temperature'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Maximum Temperature data required to calculate the index 'Minimum of Maximum Temperature'")
      } 
      
      variable.all[z] <- 'minOfMaxTemp'
      longname[z] <- 'Minimum of Maximum Temperature'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,minOfMaxTemp.amount := cummin(temperatures.max.amount),by = 'seasonNumber']
      
      dataToUse[,minOfMaxTemp.stdDev := sd(minOfMaxTemp.amount),by = 'day']
      dataToUse[,minOfMaxTemp.average := mean(minOfMaxTemp.amount), by = 'day']
      
      dataToUse[,minOfMaxTemp.average := mean(minOfMaxTemp.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'maxOfMaxTemp|Maximum of Maximum Temperature'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Maximum Temperature data required to calculate the index 'Maximum of Maximum Temperature'")
      } 
      
      variable.all[z] <- 'maxOfMaxTemp'
      longname[z] <- 'Maximum of Maximum Temperature'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,maxOfMaxTemp.amount := cummax(temperatures.max.amount),by = 'seasonNumber']
      
      dataToUse[,maxOfMaxTemp.stdDev := sd(maxOfMaxTemp.amount),by = 'day']
      dataToUse[,maxOfMaxTemp.average := mean(maxOfMaxTemp.amount), by = 'day']
      
      dataToUse[,maxOfMaxTemp.average := mean(maxOfMaxTemp.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'minOfMinTemp|Minimum of Minimum Temperature'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.min'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Minimum Temperature data required to calculate the index 'Minimum of Minimum Temperature'")
      } 
      
      variable.all[z] <- 'minOfMinTemp'
      longname[z] <- 'Minimum of Minimum Temperature'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,minOfMinTemp.amount := cummin(temperatures.min.amount),by = 'seasonNumber']
      
      dataToUse[,minOfMinTemp.stdDev := sd(minOfMinTemp.amount),by = 'day']
      dataToUse[,minOfMinTemp.average := mean(minOfMinTemp.amount), by = 'day']
      
      dataToUse[,minOfMinTemp.average := mean(minOfMinTemp.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'maxOfMinTemp|Maximum of Minimum Temperature'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.min'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Minimum Temperature data required to calculate the index 'Maximum of Minimum Temperature'")
      } 
      
      variable.all[z] <- 'maxOfMinTemp'
      longname[z] <- 'Maximum of Minimum Temperature'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,maxOfMinTemp.amount := cummax(temperatures.min.amount),by = 'seasonNumber']
      
      dataToUse[,maxOfMinTemp.stdDev := sd(maxOfMinTemp.amount),by = 'day']
      dataToUse[,maxOfMinTemp.average := mean(maxOfMinTemp.amount), by = 'day']
      
      dataToUse[,maxOfMinTemp.average := mean(maxOfMinTemp.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'dailyTempRange|Daily Temperature Range'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if ((any(grepl(pattern = 'temperatures.min'
                     ,x = colnames(dataToUse)
                     ,fixed = TRUE)) & any(grepl(pattern = 'temperatures.max'
                                                 ,x = colnames(dataToUse)
                                                 ,fixed = TRUE))) == FALSE) {
        stop("Minimum Temperature and Maximum Temperature data required to calculate the index 'Daily Temperature Range'")
      } 
      
      variable.all[z] <- 'dailyTempRange'
      longname[z] <- 'Daily Temperature Range'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,dayInSeason := 1:.N,by = 'seasonNumber']
      
      dataToUse[,temp := temperatures.max.amount - temperatures.min.amount]
      dataToUse[,dailyTempRange.amount := cumsum(temp)/dayInSeason, by = 'seasonNumber']
      
      dataToUse[,dailyTempRange.stdDev := sd(dailyTempRange.amount), by = 'day']
      dataToUse[,dailyTempRange.average := mean(dailyTempRange.amount), by = 'day']
      
      dataToUse[,dailyTempRange.average := mean(dailyTempRange.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,c('dayInSeason','temp') := NULL]
    } else if (grepl(pattern = 'maxSingleDayPrecip|Maximum of Single Day Precipitation'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the index 'Maximum of Single Day Precipitation'")
      } 
      
      variable.all[z] <- 'maxSingleDayPrecip'
      longname[z] <- 'Maximum of Single Day Precipitation'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,maxSingleDayPrecip.amount := cummax(precipitation.amount),by = 'seasonNumber']
      
      dataToUse[,maxSingleDayPrecip.stdDev := sd(maxSingleDayPrecip.amount),by = 'day']
      dataToUse[,maxSingleDayPrecip.average := mean(maxSingleDayPrecip.amount), by = 'day']
      
      dataToUse[,maxSingleDayPrecip.average := mean(maxSingleDayPrecip.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'max5ConsDayPrecip|Maximum of Five Consecutive Day Precipitation'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the index 'Maximum of 5 Consecutive Day Precipitation'")
      } 
      
      variable.all[z] <- 'max5ConsDayPrecip'
      longname[z] <- 'Maximum of 5 Consecutive Day Precipitation'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,temp := c(NA,NA,NA,NA,rollsum(precipitation.amount,5)),by = 'seasonNumber']
      
      dataToUse[!is.na(temp),max5ConsDayPrecip.amount := cummax(temp),by = 'seasonNumber']
      
      dataToUse[,max5ConsDayPrecip.stdDev := sd(max5ConsDayPrecip.amount),by = 'day']
      dataToUse[,max5ConsDayPrecip.average := mean(max5ConsDayPrecip.amount), by = 'day']
      
      dataToUse[,max5ConsDayPrecip.average := mean(max5ConsDayPrecip.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,temp := NULL]
      
    } else if (grepl(pattern = 'seasonTotalPrecip|Seasonal Total Precipitation'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the index 'Seasonal Total Precipitation'")
      } 
      
      variable.all[z] <- 'seasonTotalPrecip'
      longname[z] <- 'Seasonal Total Precipitation'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,seasonTotalPrecip.amount := cumsum(precipitation.amount),by = 'seasonNumber']
      
      dataToUse[,seasonTotalPrecip.stdDev := sd(seasonTotalPrecip.amount),by = 'day']
      dataToUse[,seasonTotalPrecip.average := mean(seasonTotalPrecip.amount), by = 'day']
      
      dataToUse[,seasonTotalPrecip.average := mean(seasonTotalPrecip.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'simplePrecipIntensityIndex|Simple Precipitation Intensity Index'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the index 'Simple Precipitation Intensity Index'")
      } 
      
      variable.all[z] <- 'simplePrecipIntensityIndex'
      longname[z] <- 'Simple Precipitation Intensity Index'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 1
      }
      index.title <- paste0('Index set at value of ',indexSpecificValue,'\n')
      
      dataToUse[,tempCol.current := FALSE]
      dataToUse[,precip.temp := precipitation.amount]
      dataToUse[precip.temp <= indexSpecificValue, precip.temp := 0]
      
      dataToUse[precip.temp > indexSpecificValue,tempCol.current := TRUE]
      dataToUse[,count.tempCol := cumsum(tempCol.current),by = 'seasonNumber']
      
      dataToUse[,temp := cumsum(precip.temp),by = 'seasonNumber']
      
      dataToUse[,simplePrecipIntensityIndex.amount := temp/count.tempCol,by = 'seasonNumber']
      
      dataToUse[,simplePrecipIntensityIndex.stdDev := sd(simplePrecipIntensityIndex.amount),by = 'day']
      dataToUse[,simplePrecipIntensityIndex.average := mean(simplePrecipIntensityIndex.amount), by = 'day']
      
      dataToUse[,simplePrecipIntensityIndex.average := mean(simplePrecipIntensityIndex.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,c('temp','tempCol.current','precip.temp','count.tempCol') := NULL]
      
    } else if (grepl(pattern = 'warmSpellDurIndex|Warm Spell Duration Index|coldSpellDurIndex|Cold Spell Duration Index'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Maximum Temperature data required to calculate the indices 'Warm Spell Durhation Index' and 'Cold Spell Duration Index'")
      } 
      
      
      if (grepl(pattern = 'warmSpellDurIndex|Warm Spell Duration Index'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        variable.all[z] <- 'warmSpellDurIndex'
        longname[z] <- 'Warm Spell Duration Index'
        
        if (is.null(indexSpecificValue) == TRUE) {
          indexSpecificValue <- 90
        }
        
        
      } else {
        variable.all[z] <- 'coldSpellDurIndex'
        longname[z] <- 'Cold Spell Duration Index'
        
        if (is.null(indexSpecificValue) == TRUE) {
          indexSpecificValue <- 10
        }
      }
      index.title <- paste0('Quantile set at ',indexSpecificValue,'%\n')
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,tempM2 := shift(temperatures.max.amount, n = 2,type = 'lag'), by = 'seasonNumber']
      dataToUse[,tempM1 := shift(temperatures.max.amount, n = 1,type = 'lag'), by = 'seasonNumber']
      dataToUse[,tempF1 := shift(temperatures.max.amount, n = 1,type = 'lead'), by = 'seasonNumber']
      dataToUse[,tempF2 := shift(temperatures.max.amount, n = 2,type = 'lead'), by = 'seasonNumber']
      
      #Calculate the quantiles per day over the baseline period
      dataToUse[lubridate::year(date) %in% years.LTN
                ,temp := quantile(x = c(tempM2,tempM1,temperatures.max.amount,tempF1,tempF2)
                                  ,probs = indexSpecificValue / 100
                                  ,na.rm = TRUE),by = 'day']
      
      #assign the value to other days that currently have NA
      dataToUse[,temp := mean(temp,na.rm = TRUE),by = 'day']
      
      dataToUse[,spell_day := FALSE]
      dataToUse[,spell_int := FALSE]
      
      if (grepl(pattern = 'warmSpellDurIndex|Warm Spell Duration Index'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        
        dataToUse[temperatures.max.amount > temp, spell_day := TRUE]
        
      } else {
        dataToUse[temperatures.max.amount < temp, spell_day := TRUE]
      }
      
      #Count counts of consecutive entries
      dataToUse[, counter.current := as.numeric(rowid(rleid(spell_day))),by = 'seasonNumber']
      
      #Set FALSE entries to NA
      dataToUse[spell_day == FALSE, counter.current := as.numeric(NA)]
      
      #Defined in the index
      dataToUse[counter.current >= 6, spell_int := TRUE]
      
      if (grepl(pattern = 'warmSpellDurIndex|Warm Spell Duration Index'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        
        dataToUse[,warmSpellDurIndex.amount := cumsum(spell_int), by = 'seasonNumber']

        dataToUse[,warmSpellDurIndex.stdDev := sd(warmSpellDurIndex.amount),by = 'day']
        dataToUse[,warmSpellDurIndex.average := mean(warmSpellDurIndex.amount), by = 'day']
        
        dataToUse[,warmSpellDurIndex.average := mean(warmSpellDurIndex.average,na.rm = TRUE), by = 'day']
        
      } else {
        dataToUse[,coldSpellDurIndex.amount := cumsum(spell_int), by = 'seasonNumber']
        
        dataToUse[,coldSpellDurIndex.stdDev := sd(coldSpellDurIndex.amount),by = 'day']
        dataToUse[,coldSpellDurIndex.average := mean(coldSpellDurIndex.amount), by = 'day']
        
        dataToUse[,coldSpellDurIndex.average := mean(coldSpellDurIndex.average,na.rm = TRUE), by = 'day']
      }
      
      
      dataToUse[,c('temp','tempM2','tempM1','tempF1','tempF2','spell_day','spell_int','counter.current') := NULL]
      
      
    } else if (grepl(pattern = 'precipSumExceedPercentile|Sum of Precipitation when Precipitation Exceeding Quantile'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the indices 'Sum of Precipitation when Precipitation Exceeding Quantile'")
      } 
      
      variable.all[z] <- 'precipSumExceedPercentile'
      longname[z] <- 'Sum of Precipitation when Precipitation Exceeding Quantile'
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 95
      }
      index.title <- paste0('Quantile set at ',indexSpecificValue,'%\n')
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      temp <- dataToUse[lubridate::year(date) %in% years.LTN &
                          precipitation.amount > 1, quantile(x = precipitation.amount,probs = indexSpecificValue/100)]
      
      dataToUse[,temp_precip := precipitation.amount]
      dataToUse[precipitation.amount <= temp,temp_precip := 0]
      
      dataToUse[,precipSumExceedPercentile.amount := cumsum(temp_precip),by = 'seasonNumber']
      
      dataToUse[,precipSumExceedPercentile.stdDev := sd(precipSumExceedPercentile.amount),by = 'day']
      dataToUse[,precipSumExceedPercentile.average := mean(precipSumExceedPercentile.amount), by = 'day']
      
      dataToUse[,precipSumExceedPercentile.average := mean(precipSumExceedPercentile.average,na.rm = TRUE), by = 'day']
      
      rm(temp)
      dataToUse[,c('temp_precip') := NULL]
      
      
    } else if (grepl(pattern = 'countDaysPrecipExceedAmount|Count of Days When Precipitation Exceeding Thresshold'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the indices 'Count of Days When Precipitation Exceeding Thresshold'")
      } 
      
      variable.all[z] <- 'countDaysPrecipExceedAmount'
      longname[z] <- 'Count of Days When Precipitation Exceeding Thresshold'
      
      if (is.null(indexSpecificValue) == TRUE) {
        indexSpecificValue <- 10
      }
      index.title <- paste0('Threshhold set at ',indexSpecificValue,' mm\n')
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,temp := FALSE]
      dataToUse[precipitation.amount > indexSpecificValue, temp := TRUE]
      
      dataToUse[,countDaysPrecipExceedAmount.amount := cumsum(temp),by = 'seasonNumber']
      
      dataToUse[,countDaysPrecipExceedAmount.stdDev := sd(countDaysPrecipExceedAmount.amount),by = 'day']
      dataToUse[,countDaysPrecipExceedAmount.average := mean(countDaysPrecipExceedAmount.amount), by = 'day']
      
      dataToUse[,countDaysPrecipExceedAmount.average := mean(countDaysPrecipExceedAmount.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,c('temp') := NULL]
      
    }  else if (grepl(pattern = 'percentDaysMinTempBelowQuantile|Percentage of Days Minimum Temperature Below Quantile|percentDaysMaxTempBelowQuantile|Percentage of Days Maximum Temperature Below Quantile|percentDaysMinTempAboveQuantile|Percentage of Days Minimum Temperature Above Quantile|percentDaysMaxTempAboveQuantile|Percentage of Days Maximum Temperature Above Quantile'
                      ,x = variable.all[z]
                      ,ignore.case = TRUE) == TRUE)  {
      
      if ((any(grepl(pattern = 'temperatures.min'
                     ,x = colnames(dataToUse)
                     ,fixed = TRUE)) & any(grepl(pattern = 'temperatures.max'
                                                 ,x = colnames(dataToUse)
                                                 ,fixed = TRUE))) == FALSE) {
        stop("Minimum and Maximum temperature data required to calculate the indices 'Percentage of Days Minimum Temperature Below Quantile',
             'Percentage of Days Maximum Temperature Below Quantile', 'Percentage of Days Minimum Temperature Above Quantile', 'Percentage of Days Maximum Temperature Above Quantile'")
      } 
      
      if (grepl(pattern = 'percentDaysMinTempBelowQuantile|Percentage of Days Minimum Temperature Below Quantile'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        
        variable.all[z] <- 'percentDaysMinTempBelowQuantile'
        longname[z] <- 'Percentage of Days Minimum Temperature Below Quantile'
        
        if (is.null(indexSpecificValue) == TRUE) {
          indexSpecificValue <- 10
        }
      } else if (grepl(pattern = 'percentDaysMaxTempBelowQuantile|Percentage of Days Maximum Temperature Below Quantile'
                       ,x = variable.all[z]
                       ,ignore.case = TRUE) == TRUE){
        
        variable.all[z] <- 'percentDaysMaxTempBelowQuantile'
        longname[z] <- 'Percentage of Days Maxinum Temperature Below Quantile'
        
        if (is.null(indexSpecificValue) == TRUE) {
          indexSpecificValue <- 10
        }
      } else if (grepl(pattern = 'percentDaysMinTempAboveQuantile|Percentage of Days Minimum Temperature Above Quantile'
                       ,x = variable.all[z]
                       ,ignore.case = TRUE) == TRUE){
        
        variable.all[z] <- 'percentDaysMinTempAboveQuantile'
        longname[z] <- 'Percentage of Days Minimum Temperature Above Quantile'
        
        if (is.null(indexSpecificValue) == TRUE) {
          indexSpecificValue <- 90
        }
      } else {
        
        variable.all[z] <- 'percentDaysMaxTempAboveQuantile'
        longname[z] <- 'Percentage of Days Maximum Temperature Above Quantile'
        
        if (is.null(indexSpecificValue) == TRUE) {
          indexSpecificValue <- 90
        }
      }
      index.title <- paste0('Quantile set at ',indexSpecificValue,'%\n')
      
      #Calculate both sets of quantiles
      dataToUse[,min_tempM2 := shift(temperatures.min.amount, n = 2,type = 'lag'), by = 'seasonNumber']
      dataToUse[,min_tempM1 := shift(temperatures.min.amount, n = 1,type = 'lag'), by = 'seasonNumber']
      dataToUse[,min_tempF1 := shift(temperatures.min.amount, n = 1,type = 'lead'), by = 'seasonNumber']
      dataToUse[,min_tempF2 := shift(temperatures.min.amount, n = 2,type = 'lead'), by = 'seasonNumber']
      
      #Calculate the quantiles per day over the baseline period
      dataToUse[lubridate::year(date) %in% years.LTN
                ,min_temp := quantile(x = c(min_tempM2,min_tempM1,temperatures.min.amount,min_tempF1,min_tempF2)
                                      ,probs = indexSpecificValue / 100
                                      ,na.rm = TRUE),by = 'day']
      
      #assign the value to other days that currently have NA
      dataToUse[,min_temp := mean(min_temp,na.rm = TRUE),by = 'day']
      
      dataToUse[,max_tempM2 := shift(temperatures.max.amount, n = 2,type = 'lag'), by = 'seasonNumber']
      dataToUse[,max_tempM1 := shift(temperatures.max.amount, n = 1,type = 'lag'), by = 'seasonNumber']
      dataToUse[,max_tempF1 := shift(temperatures.max.amount, n = 1,type = 'lead'), by = 'seasonNumber']
      dataToUse[,max_tempF2 := shift(temperatures.max.amount, n = 2,type = 'lead'), by = 'seasonNumber']
      
      #Calculate the quantiles per day over the baseline period
      dataToUse[lubridate::year(date) %in% years.LTN
                ,max_temp := quantile(x = c(max_tempM2,max_tempM1,temperatures.max.amount,max_tempF1,max_tempF2)
                                      ,probs = indexSpecificValue / 100
                                      ,na.rm = TRUE),by = 'day']
      
      #assign the value to other days that currently have NA
      dataToUse[,max_temp := mean(max_temp,na.rm = TRUE),by = 'day']
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      dataToUse[,day_exceedThresh := FALSE]
      dataToUse[,dayOfSeason := 1:.N,by = 'seasonNumber']
      
      if (grepl(pattern = 'percentDaysMinTempBelowQuantile'
                ,x = variable.all[z]
                ,ignore.case = TRUE) == TRUE) {
        
        dataToUse[temperatures.min.amount < min_temp,day_exceedThresh := TRUE]
        
      } else if (grepl(pattern = 'percentDaysMaxTempBelowQuantile'
                       ,x = variable.all[z]
                       ,ignore.case = TRUE) == TRUE){
        
        dataToUse[temperatures.max.amount < max_temp,day_exceedThresh := TRUE]
        
      } else if (grepl(pattern = 'percentDaysMinTempAboveQuantile'
                       ,x = variable.all[z]
                       ,ignore.case = TRUE) == TRUE){
        
        dataToUse[temperatures.min.amount > min_temp,day_exceedThresh := TRUE]
        
      } else {
        
        dataToUse[temperatures.max.amount > max_temp,day_exceedThresh := TRUE]
      }
      
      dataToUse[,countDaysPrecipExceedThresh := cumsum(day_exceedThresh),by = 'seasonNumber']
      dataToUse[,tempIndex.amount := countDaysPrecipExceedThresh / dayOfSeason]
      
      dataToUse <- bootstrapByYear(data = dataToUse
                                   ,years.LTN = years.LTN)
      setkey(dataToUse,date)
      
      dataToUse[,tempIndex.average := mean(tempIndex.average,na.rm = TRUE), by = 'day']
      
      setnames(dataToUse,paste0('tempIndex',c('.amount','.stdDev','.average')),paste0(variable.all[z],c('.amount','.stdDev','.average')))
      
      dataToUse[,c('min_tempM2','min_tempM1','min_tempF1','min_tempF2','min_temp') := NULL]
      dataToUse[,c('max_tempM2','max_tempM1','max_tempF1','max_tempF2','max_temp') := NULL]
      dataToUse[,c('day_exceedThresh','dayOfSeason','countDaysPrecipExceedThresh') := NULL]
      
      
    } else if (grepl(pattern = 'sumOfGDD|Sum of Growing Degree Days'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'gdd'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("GDD data required to calculate the index 'Sum Of Growing Degree Days'")
      }
      
      variable.all[z] <- 'sumOfGDD'
      longname[z] <- 'Sum of Growing Degree Days'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,sumOfGDD.amount := cumsum(gdd.amount),by = 'seasonNumber']
      
      dataToUse[,sumOfGDD.stdDev := sd(sumOfGDD.amount),by = 'day']
      dataToUse[,sumOfGDD.average := mean(sumOfGDD.amount), by = 'day']
      
      dataToUse[,sumOfGDD.average := mean(sumOfGDD.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'sumOfPET|Sum of PET'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'pet'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("PET data required to calculate the index 'Sum Of PET'")
      }
      
      variable.all[z] <- 'sumOfPET'
      longname[z] <- 'Sum of PET'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,sumOfPET.amount := cumsum(pet.amount),by = 'seasonNumber']
      
      dataToUse[,sumOfPET.stdDev := sd(sumOfPET.amount),by = 'day']
      dataToUse[,sumOfPET.average := mean(sumOfPET.amount), by = 'day']
      
      dataToUse[,sumOfPET.average := mean(sumOfPET.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'sumOfPrecip|Sum of Precipitation'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Precipitation data required to calculate the index 'Sum Of Precipitation'")
      }
      
      variable.all[z] <- 'sumOfPrecip'
      longname[z] <- 'Sum of Precipitation'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,sumOfPrecip.amount := cumsum(precipitation.amount),by = 'seasonNumber']
      
      dataToUse[,sumOfPrecip.stdDev := sd(sumOfPrecip.amount),by = 'day']
      dataToUse[,sumOfPrecip.average := mean(sumOfPrecip.amount), by = 'day']
      
      dataToUse[,sumOfPrecip.average := mean(sumOfPrecip.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'sumOfPOverPET|Sum of Precipitation over PET'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'precipitation|pet'
                    ,x = colnames(dataToUse)
                    ,fixed = FALSE)) == FALSE) {
        stop("Precipitation and PET data required to calculate the index 'Sum Of Precipitation over PET'")
      }
      
      variable.all[z] <- 'sumOfPOverPET'
      longname[z] <- 'Sum of Precipitation over PET'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,c('sumOfPrecip.amount','sumOfPET.amount') := 0]
      
      dataToUse[,sumOfPrecip.amount := cumsum(precipitation.amount),by = 'seasonNumber']
      dataToUse[,sumOfPET.amount    := cumsum(pet.amount),by = 'seasonNumber']
      
      dataToUse[,sumOfPrecip.amount := zoo::na.locf(sumOfPrecip.amount), by = 'seasonNumber']
      dataToUse[,sumOfPET.amount := zoo::na.locf(sumOfPET.amount), by = 'seasonNumber']
      
      dataToUse[is.na(sumOfPrecip.amount), sumOfPrecip.amount := 0]
      dataToUse[is.na(sumOfPET.amount), sumOfPET.amount := 0]
      
      dataToUse[,sumOfPOverPET.amount := 0]
      dataToUse[sumOfPET.amount > 0 ,sumOfPOverPET.amount := sumOfPrecip.amount / sumOfPET.amount]
      
      dataToUse[,sumOfPOverPET.stdDev := sd(sumOfPOverPET.amount),by = 'day']
      dataToUse[,sumOfPOverPET.average := mean(sumOfPOverPET.amount), by = 'day']
      
      dataToUse[,sumOfPOverPET.average := mean(sumOfPOverPET.average,na.rm = TRUE), by = 'day']
      
      dataToUse[,c('sumOfPrecip.amount','sumOfPET.amount') := NULL]
      
    } else if (grepl(pattern = 'sumOfSolar|Sum of Solar Radiation'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'solar'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Solar data required to calculate the index 'Sum Of Solar Radiation'")
      }
      
      variable.all[z] <- 'sumOfSolar'
      longname[z] <- 'Sum of Solar Radiation'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,sumOfSolar.amount := cumsum(solar.amount),by = 'seasonNumber']
      
      dataToUse[,sumOfSolar.stdDev := sd(sumOfSolar.amount),by = 'day']
      dataToUse[,sumOfSolar.average := mean(sumOfSolar.amount), by = 'day']
      
      dataToUse[,sumOfSolar.average := mean(sumOfSolar.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'averageMaxTemp|Average Maximum Temperature'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Temperature data required to calculate the index 'Average Maximum Temperature'")
      }
      
      variable.all[z] <- 'averageMaxTemp'
      longname[z] <- 'Average Maximum Temperature'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,averageMaxTemp.amount := mean(temperatures.max.amount,na.rm = TRUE),by = 'seasonNumber']
      
      dataToUse[,averageMaxTemp.stdDev := sd(averageMaxTemp.amount),by = 'day']
      dataToUse[,averageMaxTemp.average := mean(averageMaxTemp.amount), by = 'day']
      
      dataToUse[,averageMaxTemp.average := mean(averageMaxTemp.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'averageMinTemp|Average Minimum Temperature'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'temperatures.min'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Temperature data required to calculate the index 'Average Maximum Temperature'")
      }
      
      variable.all[z] <- 'averageMinTemp'
      longname[z] <- 'Average Minimum Temperature'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,averageMinTemp.amount := mean(temperatures.min.amount,na.rm = TRUE),by = 'seasonNumber']
      
      dataToUse[,averageMinTemp.stdDev := sd(averageMinTemp.amount),by = 'day']
      dataToUse[,averageMinTemp.average := mean(averageMinTemp.amount), by = 'day']
      
      dataToUse[,averageMinTemp.average := mean(averageMinTemp.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'averageMaxRH|Average Maximum Relative Humidity'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'relativeHumidity.max'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Relative Humidity data required to calculate the index 'Average Maximum Relative Humidity'")
      }
      
      variable.all[z] <- 'averageMaxRH'
      longname[z] <- 'Average Maximum Relative Humidity'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,averageMaxRH.amount := mean(relativeHumidity.max.amount,na.rm = TRUE),by = 'seasonNumber']
      
      dataToUse[,averageMaxRH.stdDev := sd(averageMaxRH.amount),by = 'day']
      dataToUse[,averageMaxRH.average := mean(averageMaxRH.amount), by = 'day']
      
      dataToUse[,averageMaxRH.average := mean(averageMaxRH.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'averageMinRH|Average Minimum Relative Humidity'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'relativeHumidity.min'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Temperature data required to calculate the index 'Average Maximum Temperature'")
      }
      
      variable.all[z] <- 'averageMinRH'
      longname[z] <- 'Average Minimum Relative Humidity'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,averageMinRH.amount := mean(relativeHumidity.min.amount,na.rm = TRUE),by = 'seasonNumber']
      
      dataToUse[,averageMinRH.stdDev := sd(averageMinRH.amount),by = 'day']
      dataToUse[,averageMinRH.average := mean(averageMinRH.amount), by = 'day']
      
      dataToUse[,averageMinRH.average := mean(averageMinRH.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'averageWind|Average Windspeed'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'wind.average'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Wind data required to calculate the index 'Average Wind Speed'")
      }
      
      variable.all[z] <- 'averageWind'
      longname[z] <- 'Average Windspeed'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,averageWind.amount := mean(wind.average.amount,na.rm = TRUE),by = 'seasonNumber']
      
      dataToUse[,averageWind.stdDev := sd(averageWind.amount),by = 'day']
      dataToUse[,averageWind.average := mean(averageWind.amount), by = 'day']
      
      dataToUse[,averageWind.average := mean(averageWind.average,na.rm = TRUE), by = 'day']
      
    } else if (grepl(pattern = 'maxWindGust|Maximum Wind Gust Speed'
                     ,x = variable.all[z]
                     ,ignore.case = TRUE) == TRUE)  {
      
      if (any(grepl(pattern = 'wind.dayMax'
                    ,x = colnames(dataToUse)
                    ,fixed = TRUE)) == FALSE) {
        stop("Daily Maximum Wind data required to calculate the index 'Maximum Wind Gust Speed'")
      }
      
      variable.all[z] <- 'maxWindGust'
      longname[z] <- 'Maximum Wind Gust Speed'
      
      suppressWarnings(dataToUse[,paste0(variable.all[z],c('.amount','.average','.stdDev')) := NULL])
      
      dataToUse[,maxWindGust.amount := max(wind.dayMax.amount,na.rm = TRUE),by = 'seasonNumber']
      
      dataToUse[,maxWindGust.stdDev := sd(maxWindGust.amount),by = 'day']
      dataToUse[,maxWindGust.average := mean(maxWindGust.amount), by = 'day']
      
      dataToUse[,maxWindGust.average := mean(maxWindGust.average,na.rm = TRUE), by = 'day']
      
    } else if (is.null(variable.all[z] == TRUE)) {
      next
    }
  }
  
  ####################################################################################################
  if (is.null(startYearOfSeasonToPlot) == FALSE) {
    seasonToPlot.string <- paste0("\nCurrent Year is ",startYearOfSeasonToPlot)
  } else {
    seasonToPlot.string <- ''
  }
  
  if (is.null(title)) {
    title <-  paste0(paste0(longname,collapse = ' &\n'),
                     seasonToPlot.string,
                     '\nLTN calculated between ',min(years.LTN),' and ',max(years.LTN))
    
    if (exists('index.title') == TRUE) {
      title <- paste0(title,'\n',index.title)
    }
  }
  
  return(list(dataToUse,variable.all,title,year.increment))
  
  
}