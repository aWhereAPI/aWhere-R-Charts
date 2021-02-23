#' @title getCoordinateInfo
#'
#' @description
#' \code{getCoordinateInfo} Find some basic information about the geography in which a lat/long point falls.
#'
#' @details
#' This function returns the name of the country in which a given point is located,
#' or if desired, the continent, the development designation, or the population of 
#' the country. Users of the aWhere API often pull weather data for lists of  
#' points or fields for which no other geographic information is provided, or receive 
#' prepackaged large weather data files identified by points which do not include 
#' other geographic information for size reasons. This function helps to retrieve this
#' information quickly if needed for analysis without requiring modification of the 
#' original files.
#'
#'
#' @references 
#'
#' @param points data frame or matrix of points, with longitude in the first column and
#'                           latitude in the second. Points should be in decimal degrees. (required)
#' @param attribute character string of the type of information desired, accepted inputs 
#'                           are "country", "continent", or "continent classification". (required)
#'
#' @import sp
#' @import rworldmap
#'
#' @return vector of names with factor levels
#'
#' @examples
#' \dontrun{getCoordinateInfo(points = weather_df[, c("longitude", "latitude")], attribute = "country")}

#' @export


getCoordinateInfo <- function(points, attribute){  
  
  #retrieve world map
  countriesSP <- rworldmap::getMap(resolution='high')
  
  #convert the points to a SpatialPoints object and set the CRS
  pointsSP = sp::SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  #use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)
  
  if(attribute == "country") {
    # return the ADMIN names of each country
    return(indices$ADMIN)
  } else if(attribute == "continent") {
    # returns the continent (7 continent model)  
    return(indices$REGION)        
  } else if(attribute == "continent classification") {
    # returns the continent and classification   
    return(indices$GBD)             
  } else {
    stop("Your attribute parameter is not allowed. Please choose either 'country', 'continent', or 'continent classification'.")
  }
}
