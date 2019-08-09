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
    colorScaleToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1696AC", "#FF810E")) 
  } else {
    colorScaleToUse <- scale_colour_manual(values = c("#1696AC")) 
    colorFillToUse <- scale_fill_manual(values = c("#1696AC")) 
  } 
  
  #Set the bin spacing
  
  currentPrecision <- 0
  stepSize <- 0
  range <- 0
  
  while (range == 0) {
    minValue.range <- floor(chart_data[,min(measure)]/(currentPrecision + 1)) * (currentPrecision + 1)
    maxValue.range <- ceiling(chart_data[,max(measure)]/(currentPrecision + 1)) * (currentPrecision + 1)
    
    range <- maxValue.range - minValue.range
    
    if (range == 0) {
      currentPrecision <- currentPrecision + 1
    }
  }
  
  while(stepSize == 0) {
    
    stepSize <- round(range/10,currentPrecision)
    
    if (stepSize == 0) {
      currentPrecision <- currentPrecision + 1
    } else if (stepSize < 1) {
      break
    } else if (stepSize < 4) {
      break
    } else if (stepSize < 5) {
      stepSize <- 5
    } else if (stepSize < 10) {
      stepSize <- 10 
    } else if (stepSize < 25) {
      stepSize <- 25
    } else if (stepSize < 50) {
      stepSize <- 50
    } else if (stepSize < 100) {
      stepSize <- 100
    } else if (stepSize < 250) {
      stepSize <- 250
    } else if (stepSize < 500) {
      stepSize <- 500
    } else if (stepSize < 1000) {
      stepSize <- 1000
    }
  }
  
  
  #set x axis scale
  seqToUse <- seq(from = minValue.range 
                  ,to = maxValue.range
                  ,by = stepSize)
  
  xScale <- scale_x_continuous(breaks = seqToUse)
  
  # variables for title and label font sizes 
  size_font_main_title <- 16
  size_font_axis_titles <- 14
  size_font_axis_labels <- 14
  size_font_legend_entries <- 14
  
  #make chart
  chart <- 
    ggplot() + 
    theme_bw() + 
    geom_histogram(data = chart_data, 
                   aes(measure,
                       col = Variable, 
                       fill = Variable),
                   position = 'identity',
                   bins = 40,
                   alpha = 0.4) +
    xScale +
    theme(legend.position="bottom"
          ,legend.direction="horizontal"
          ,legend.title = element_blank()
          
          ,legend.spacing.x = unit(0.3,"cm")
          
          # x-axis labels
          ,axis.text.x = element_text(color = "grey20", # font color 
                                      size = size_font_axis_labels, # font size 
                                      hjust = 1,        # horizontal adjustment
                                      face = "plain")  # font type "plain", "bold" 
          # y-axis labels 
          ,axis.text.y = element_text(color = "grey20", 
                                      size = size_font_axis_labels, 
                                      face = "plain")
          # y-axis title
          ,axis.title.y = element_text(color = "grey20", 
                                       size = size_font_axis_titles, 
                                       face = "bold")
          
          # x-axis title
          ,axis.title.x = element_text(color = "grey20", 
                                       size = size_font_axis_titles, 
                                       face = "bold")
          
          # legend text size
          ,legend.text=element_text(size=size_font_legend_entries)) + 
    
    labs(x=xlabel, y = "Count of Locations") +
    colorScaleToUse + 
    colorFillToUse + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=size_font_main_title)) # main title size
  
  return(chart)
}
