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
#'
#' @return plot object
#'
#' @examples
#' \dontrun{generateaWhereHistogram(data = weather_df, variable = "precipitation.amount", compare = TRUE, 
#'                                  compare_var = "precipitation.average", xlabel = "mm", 
#'                                  title = "Current vs. LTN Rain across region X, past 30 days")}

#' @export



generateaWhereHistogram <- function(data
                                    ,variable 
                                    ,compare = FALSE
                                    ,compare_var = NULL
                                    ,xlabel = NULL
                                    ,title = NULL) {
  
  
    #if title is not given by user, set it to date range + variable
    if (is.null(title)) {
      title <- paste0("Aggregated aWhere Data - ", variable)
    }
  
    #filter out relevant data
  
    if (compare == TRUE) {
      chart_data <- data[, c("latitude", "longitude", variable, compare_var)]
      chart_data$place <- paste0(chart_data$latitude, ", ", chart_data$longitude)
      chart_data <- chart_data[, c("place", variable, compare_var)]
      chart_data <- setNames(chart_data, c("place", "Current", "LTN"))
    } else {
      chart_data <- data[, c("latitude", "longitude", variable)]
      chart_data$place <- paste0(chart_data$latitude, ", ", chart_data$longitude)
      chart_data <- chart_data[, c("place", variable)]
      chart_data <- setNames(chart_data, c("place", "Current"))
    }
  
    #set data format as long
    chart_data <- tidyr::gather(chart_data, 
                                key = Variable, 
                                value = measure, 
                                2:ncol(chart_data))
    
    #set common names of columns and xlabel (if not specified)
    if (is.null(xlabel)) {
      xlabel <- variable
    }

    
  
    #set color scale based on # of vars to chart
    if(length(unique(chart_data$Variable)) == 2) {
      colorScaleToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
    } else {
      colorScaleToUse <- scale_colour_manual(values = c("#1696AC")) 
    } 
  
    if(length(unique(chart_data$Variable)) == 2) {
      colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#FF810E")) 
    } else {
      colorFillToUse <- scale_fill_manual(values = c("#1F83B4")) 
    } 
  
    #set x axis scale
    xScale <- scale_x_continuous(breaks = seq(from = min(chart_data$measure), 
                                              to = max(chart_data$measure),
                                              by = as.integer((as.integer(max(chart_data$measure)) 
                                                               - as.integer(min(chart_data$measure)))/10)))
    
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
