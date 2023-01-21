#
setwd('2018/2018-06-26')
setwd('..')
#


# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2018-06-19')

tuesdata <- tidytuesdayR::tt_load('2018-06-26')

tuesdata$week13_alcohol_global