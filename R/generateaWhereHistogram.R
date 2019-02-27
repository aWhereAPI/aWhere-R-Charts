#' @title generateaWhereHistogram
#'
#' @description
#' \code{generateaWhereHistogram} Generate a histogram of 1-2 weather variables with standardized formatting.
#'
#' @details
#' This function generates a histogram with standardized formatting in ggplot's gray theme. This 
#' function was created to work with the large datasets covering entire regions transferred by 
#' aWhere to clients, enabling them to examine variation in the current conditions of a region 
#' for any variable, as well as compare current vs. long-term normals. Technically, however, the 
#' function can work with any dataset, since unlike with the generateaWhereChart and 
#' generateaWhereStdDevChart functions, the input variable names are not restricted.
#'
#' @references 
#'
#' @param - data: data frame containing multiple observations - observations can be across time or locations (required)
#' @param - variable: character string denoting the column name (from data) to chart. (required)
#' @param - compare: logical, if set to TRUE function will chart a second variable, 
#'             defined by the parameter compare_var. Default set to FALSE. (optional)
#' @param - compare_var: character string denoting the column name of the second variable to chart (optional)
#' @param - xlabel: character string denoting the label to give to the x axis of the chart (optional)
#' @param - title: character string denoting the title to assign to the plot - default is the date span of data (optional)
#'
#' @import tidyr
#' @import zoo
#' @import ggplot2
#' @import ggthemes
#' @import data.table
#'
#' @return plot object
#'
#' @examples
#' \dontrun{generateaWhereHistogram(data = weather_df
#'                                  ,variable = "precipitation.amount"
#'                                  ,compare = TRUE
#'                                  ,compare_var = "precipitation.average"
#'                                  ,xlabel = "mm"
#'                                  ,title = "Current vs. LTN Rain across region X, past 30 days")}

#' @export



generateaWhereHistogram <- function(data
                                    ,variable 
                                    ,compare = FALSE
                                    ,compare_var = NULL
                                    ,xlabel = NULL
                                    ,title = NULL) {
  
  data <- data.table::as.data.table(data.table::copy(data))
  
  #if title is not given by user, set it to date range + variable
  if (is.null(title)) {
    title <- paste0("Aggregated aWhere Data - ", variable)
    }
  
  #set common names of columns and xlabel (if not specified)
  if (is.null(xlabel)) {
    xlabel <- variable
  }
  
  #filter out relevant data
  variablesToInclude <- c('latitude'
                        ,'longitude'
                        ,variable)
  
  variableNames <- c("place", "Current")

  if (compare == TRUE) {
    
    variablesToInclude <- c(variablesToInclude
                            ,compare_var)
    
    variableNames <- c(variableNames, 'LTN')
  } 
  
  chart_data <- data[, variablesToInclude, with = FALSE]
  
  chart_data[,place := paste0(chart_data$latitude, ", ", chart_data$longitude)]
  
  chart_data <- chart_data[, c('place',variablesToInclude[-c(1,2)]), with = FALSE]
  
  chart_data <- setNames(chart_data, variableNames)


  #set data format as long
  chart_data <- 
    tidyr::gather(chart_data, 
                  key = Variable, 
                  value = measure, 
                  2:ncol(chart_data)) %>%
    data.table::as.data.table(.)

  #set color scale based on # of vars to chart
  if(length(unique(chart_data$Variable)) == 2) {
    colorScaleToUse <- colorFillToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
  } else {
    colorScaleToUse <- colorFillToUse <- scale_colour_manual(values = c("#1696AC")) 
  } 

  #set x axis scale
  xScale <- scale_x_continuous(breaks = seq(from = chart_data[,min(measure)] 
                                            ,to = chart_data[,max(measure)]
                                            ,by = floor((ceiling(chart_data[,max(measure)]) - 
                                                                          floor(chart_data[,min(measure)]))/10)))
  
  #make chart
  chart <- ggplot() + 
    theme_igray() + 
    geom_histogram(data = chart_data, 
                   aes(measure,
                       col = Variable, 
                       fill = Variable),
                   position = 'identity',
                   bins = 40,
                   alpha = 0.4) +
    xScale +
    theme(legend.position="bottom", legend.direction="horizontal",
           legend.title = element_blank()) +
    labs(x=xlabel, y = "Count of Locations") +
    ggtitle(title)
    
  return(chart)
}
