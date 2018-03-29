#' @title generateMultiplot
#'
#' @description
#' \code{generateMultiplot} Assembles multiple plots into a single window
#'
#' @details
#' This function allows you to assemble several plot objects into a single window
#' for more convenient display and saving. The function has options for customizing 
#' the number of columns (into which plots are arranged), and the title assigned
#' to the overall multiplot. This function was created to serve users who were generating 
#' multiple weather/agronomic plots using functions elsewhere in this package, but
#' wanted to be able to combine them into a single "analysis" window in order to more 
#' easily detect combinations of conditions.
#'
#'
#' @references See other functions in this package for generating plots as inputs.
#'
#' @param - ...: All plot objects to assemble, separated by commas. (required)
#' @param - title: character string of the title you would like to assign to the multiplot,
#'                           multiplot title is independent of any titles set to individual plots. (required)
#' @param - fontsize: numeric value of the fontsize to use in rendering the multiplot title, default is 12. (optional)
#' @param - cols: numeric value for the number of columns into which to assemble input plots, default is 2. (optional)
#' 
#' @import grid
#'
#' @return plot object
#'
#' @examples
#' \dontrun{generateMultiplot(rain_plot, maxtemp_plot, fontsize = 20, cols = 1)}

#' @export

generateMultiplot <- function(..., title, fontsize = 12, cols = 2) {
  #Make a list of the input plots
  plots <- c(list(...))
  
  nplots <- length(plots)
  
  #Set up layout using the 'cols' argument
  #Number of rows is calculated as nplots/cols
  layout <- matrix(seq(1, cols * ceiling(nplots/cols)),
                     ncol = cols, nrow = ceiling(nplots/cols))
  
  #If for some reason the user only input 1 plot, print it
  if (nplots==1) {
    
    print(plots[[1]])
  
    #else, set up the multiplot  
  } else {
    plots <- lapply(plots, function(x) {
      x + theme(plot.title = element_text(size=fontsize))
    })
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout)+1, ncol(layout), heights = unit(c(1, 4, 4), "null"))))
    grid.text(title, gp = gpar(fontsize= fontsize*2), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2, gp = gpar(fill="gray")))
    
    # Make each plot, in the correct location
    for (i in 1:nplots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row +1,
                                      layout.pos.col = matchidx$col))
    }
  }
}

