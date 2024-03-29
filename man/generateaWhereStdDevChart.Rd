% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateaWhereStdDevChart.R
\name{generateaWhereStdDevChart}
\alias{generateaWhereStdDevChart}
\title{generateaWhereStdDevChart}
\usage{
generateaWhereStdDevChart(
  data,
  variable,
  title = NULL,
  e_precip = FALSE,
  e_threshold = 35,
  doRoll = FALSE,
  rolling_window = 30,
  yAxisLimits = NA,
  size_font_main_title = 16,
  size_font_axis_titles = 14,
  size_font_axis_labels = 12,
  size_font_legend_entries = 12,
  line_width = 1
)
}
\arguments{
\item{data}{data frame in which variables are named according to the aWhere API conventions (required)}

\item{variable}{character string denoting the variable to chart. Acceptable values 
are accumulatedGdd, accumulatedPet, accumulatedPpet, accumulatedPrecipitation,
gdd, pet, precipitation, maxRH, minRH, solar,averageWind,dayMaxWind
or rollingavgppet. (required)}

\item{title}{character string of title to assign to the plot. (required)}

\item{e_precip}{logical, if set to TRUE, effective precipitation will 
be calculated and charted based on e_threshold. Default is set to FALSE. (optional)}

\item{e_threshold}{numeric value (in milimeters) for the daily maximum used to calculate 
effective precipitation if e_precip is set to TRUE. (optional)}

\item{doRoll}{Boolean for whether a rolling calculation should be done to smooth the data (optional)}

\item{rolling_window}{numeric value for the number of days to use in rolling 
average calculations, only applicable if the variable parameter is set to
"rollingavgppet". Default value is 30. (optional)}

\item{yAxisLimits}{Used to set the limits of the y axis explicitly.  If used, must be a two element vector of the form 
c(minValue, maxValue) (optional)}

\item{size_font_main_title}{Font size of main title of graph (optional)}

\item{size_font_axis_titles}{Font size of axes on graph (optional)}

\item{size_font_axis_labels}{Font size of labels on axes on graph (optional)}

\item{size_font_legend_entries}{Font size of entries in lengend on graph (optional)}

\item{line_width}{Font size for line geometries on charts (optional)}
}
\value{
plot object
}
\description{
\code{generateaWhereStdDevChart} Generate a plot using aWhere weather data with standardized 
                                  formatting and standard deviations included.
}
\details{
This function makes basic line plots using the same structure, formatting and inputs utilized 
in the generateaWhereChart function, but adds one standard deviation's shading above and below 
the long-term normal line. This function is meant to help users understand whether the current
weather conditions are significantly outside the norm. 
 
Refer to the documentation for generateaWhereChart for more details on the parameter inputs and 
definitions. The input variables allowed are the same as those allowed for the generateaWhereChart
function, but it should be noted that as the datespan increases, the standard deviation for variables 
which are accumulations is likely to become increasingly wide.
}
\examples{
\dontrun{generateaWhereStdDevChart(data = weather_df
                                   ,variable = "accumulatedPrecipitation" 
                                   ,title = "Accumulated Precipitation values at Location from Xdate to Ydate"
                                   ,e_precip = TRUE
                                   ,e_threshold = 20)}
}
\references{
http://developer.awhere.com/api/reference/
}
