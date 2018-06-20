# aWhereCharts Library

This package of functions is designed to work primarily with the aWhere API, and users should first research and install the [aWhereAPI](https://github.com/aWhereAPI/aWhere-R-Library "aWhereAPI") package, and ensure they can make calls to the API and retrieve data. Users without an API account may sign up for a [trial account](www.developer.awhere.com).

The aWhereCharts package helps users accomplish a host of simple analytic tasks to get them started performing exploratory weather and agronomic charting as quickly as possible.

## Installation

**Note:** Prior to installation you will need a working installation of R.

**Note:** You will need to have the devtools library installed in R. Consult documentation if necessary.

### Automatically Install

This package can be installed directly from GitHub with the following command:

    devtools::install_github("aWhereAPI/aWhere-R-Charts")

### Manually Install

1. Download this Github repo and keep the `aWhere-R-Charts` folder somewhere useful on your computer, such as your Desktop or My Documents.

2. Set the Working Path in R to the location that contains the `aWhere-R-Charts` folder. If you placed it on your Desktop the working directory would be something like `C:\Users\USERNAME\Desktop`. In R, this command is:
	* `setwd("C:\Users\USERNAME\Desktop")`

3. Run the following set of commands to install and add the library to your environment:
	* `library(devtools)`
	* `install("aWhere-R-Charts")`
	* `library(aWhereAPI)`

## Documentation
Complete documentation of the functions in this package can be found in the **documentation** folder of the package files, as well as by accessing the function help files individually.

## Questions and Feedback

This package was developed by aWhere staff. Contact techsupport@awhere.com with feedback or questions.