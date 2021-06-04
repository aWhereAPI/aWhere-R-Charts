#' @title ClipValues
#'
#' @description
#' \code{ClipValues} Clips the extremely large or small values of an input vector. 
#'
#' @details
#' Clips the extremely large or small values of an input vector. 
#'
#' @param values (vector) input variables to clip.
#' @param max.thresh (numeric) values above this value will be assigned to this value.
#' @param min.thresh (numeric) values below this value will be assigned to this value.
#'
#' @return  values input vector, altered to have new minimum/maximum values. 
#'
#' @examples
#' 
#' @export

ClipValues <- function(values, max.thresh, min.thresh = 0){
  
  values[values < min.thresh] <- min.thresh
  values[values > max.thresh] <- max.thresh

  return(values)
}

#' @title WriteJpeg
#'
#' @description
#' \code{WriteJpeg} Saves the plot as a jpeg
#'
#' @details
#' Opens the jpeg graphics device with the specified settings and saves the plot
#' to an image file (.jpeg) 
#'
#' @param plt (ggplot plot object) plot to write to file
#' @param plt.title (character string) title of the plot, used to name the output file
#' @param w (integer, optional with default value of 10) width of graphics device
#' @param h (integer, optional with default value of 6) height of graphics device 
#' @param u (character, optional with default value of "in") the units in which 
#'             height and weight are given. Can be "in", "px", "cm", or "mm".
#' @param r (integer, optional with default value of 500) nominal resolution in ppi
#'
#' @return  
#'
#' @examples
#' 
#' @export

WriteJpeg <- function(plt, plt.title, w=10, h=6, u="in", r=500){

  # set graphics device
  jpeg(paste0(plt.title, ".jpeg"), 
       width = w, 
       height = h, 
       units = u, 
       res = r)
  
  # write plot to image file
  print(plt)
  
  # close graphics device 
  invisible(dev.off())
}

#' @title formatGraphTitleForFileName
#'
#' @description
#' \code{formatGraphTitleForFileName} Removes illegal characters for titles so they can be used for saving files
#'
#' @details
#' Removes illegal characters for titles so they can be used for saving files
#'
#' @param title character string to remove characters of
#'
#' @return  
#'
#' @examples
#' 
#' @export

formatGraphTitleForFileName <- function(title) {
  out <- 
    gsub(pattern = '\n| |-|/|:'
         ,replacement = '_'
         ,x = title)
  
  return(out)
}

#' @title generateColorScale
#'
#' @description
#' \code{generateColorScale} #used to generate colorscheme when additional years are added to graph
#'
#' @details
#' #used to generate colorscheme when additional years are added to graph
#'
#' @param fig
#' @param add.years
#' @param add.years.colors
#'
#' @return  
#'
#' @examples
#' 
#' @export

generateColorScale <- function(fig
                               ,add.years
                               ,add.years.colors) {
  
  #recast the ggplot object so we can extract additional info
  fig.build <- ggplot_build(fig)
  
  #In the charts we generate, if their is no line specified it refers to SD
  if (nrow(rbindlist(fig.build$data,use.names = TRUE,fill = TRUE)[linetype == 'blank']) > 0) {
    includeSTD <- TRUE
  } else {
    includeSTD <- FALSE
  }
  
  #Extract the break/color info
  fig.breaks <- fig$scales$scales[[2]]$breaks
  fig.colors <- unique(fig.build$data[[1]]$colour)
  
  #If it included the SD information, add that info to the color schem info
  if(includeSTD == TRUE) {
    fig.breaks <- c(fig.breaks, 'SD of LTN')
    fig.colors <- c(fig.colors,fig.colors[which(fig.breaks == 'LTN')])
  }
  
  
  fig.breaks <- c(fig.breaks,add.years)
  fig.colors <- c(fig.colors,add.years.colors[1:length(add.years)])
  
  
  colorScheme <- data.table(variable = fig.breaks
                            ,color = fig.colors)
  
  colors.string <- paste(colorScheme[,paste0('\"',variable,'\" = \"', color,'\"')],collapse = ',\n')
  colorBreaks.string <- paste(colorScheme[variable != 'SD of LTN',paste0('\"',variable,'\"')],collapse = ', ')
  
  
  eval(parse(text = paste0('colorScaleToUse <- scale_color_manual(values = c(',colors.string,'),breaks = c(',colorBreaks.string,'))')))
  eval(parse(text = paste0('fillScaleToUse <- scale_fill_manual(values = c(',colors.string,'),breaks = c(',colorBreaks.string,'))')))
  
  return(list(colorScale = colorScaleToUse,fillScale = fillScaleToUse))
  
}

#' @title loadVarMemRef_aWhereEnv 
#'
#' @description
#' \code{loadVarMemRef_aWhereEnv} load the data object stored at a specific memory address
#'
#' @details
#' load the data object stored at a specific memory address
#'
#' @param memAddress
#' @return  
#'
#' @examples
#' 
loadVarMemRef_aWhereEnv <- function(memAddress) {
  
  globalEnvVars <- ls(envir=.GlobalEnv)
  memRef_GlobalEnvVars <-  
    purrr::map_chr(globalEnvVars, ~ do.call(pryr::address,list(rlang::sym(.x))) ) %>%
    data.table::data.table(variable = globalEnvVars,address = .)
  
  out <- memRef_GlobalEnvVars[address == memAddress,variable]
  
  if (length(out) > 0) {
    
    return(eval(parse(text = paste0('as.data.table(.GlobalEnv$',out[1],')'))))
  
  } else {
    return(NULL)
  }
}


