#' set directory with files from CollegeScorecard_rawData
#' 
setwd('CollegeScorecard_Raw_Data_09012022/')
#' 
#' the period we more interested in are in files MERGEDPERIOD_PP.csv
#' so MERGED2004_05_PP.csv to MERGED2016_17_PP.csv
#' get the pattern, check directory for file matching 
#'
x <- sprintf("%02d", 4:15)
y <- sprintf("%02d", 5:16)
acc_year <- paste(x, y, sep = '_')
# acc_year
#
file_s = c()
for (i in acc_year){
  file_s = c(file_s, 
             grep(pattern = i, x = list.files('.'), value = T))
}
# file_s # Files matching
#' 
#' Read in the data
#' na.strings are represented as NULLS
#' data files are bulky, so we only get columns of interest via select argument
#' then add .id as file_s being supplied that gives us new column of 
#' index of file being read, ie our filenames are in vector,
#' so to be read into R via pmap_dfr ie merged via rows, it gives us 
#' period from which data is obtained.
#' ie file 2004_05 is the first file in vector directory, so new id column marks
#' it as 1, and so forth until 12 files ie number 12, after 
#' which we replace the indices 1 to 12 with years
#' 
library(data.table)
library(purrr)

state_admission_rate <- pmap_dfr(.f = fread, 
              .l = list(file_s ), 
              .id = 'file_s', #index of file
              na.strings = "NULL",
              select = c("STABBR", "ADM_RATE") )
#
file_s = gsub(pattern = "MERGED", replacement = "", file_s)
file_s = gsub(pattern = "_PP.csv", replacement = "", file_s)
file_s = gsub(pattern = "_", replacement = "-20", file_s)
# file_s
#
library(dplyr)

state_admission_rate = state_admission_rate |> 
  mutate(
    year = case_when(
      file_s == 1 ~ "2004-2005", file_s == 2 ~ "2005-2006", 
      file_s == 3 ~ "2006-2007", file_s == 4 ~ "2007-2008", 
      file_s == 5 ~ "2008-2009", file_s == 6 ~ "2009-2010", 
      file_s == 7 ~ "2010-2011", file_s == 8 ~ "2011-2012", 
      file_s == 9 ~ "2012-2013", file_s == 10 ~ "2013-2014", 
      file_s == 11 ~ "2014-2015", file_s == 12 ~ "2015-2016"
)) |> select(-file_s)
#'
#' We get average admission rates by year and state
#' 
state_admission_rate_avg = state_admission_rate |> 
  group_by(year, STABBR) |> 
  summarise(
    average_admission_rates = mean(ADM_RATE, na.rm = TRUE)
  )
#'
#' convert state abbreviations to full names
#' 
state_admission_rate_avg$STABBR <- state.name[
  match(state_admission_rate_avg$STABBR, state.abb)]
#
state_admission_rate_avg <- 
  state_admission_rate_avg[complete.cases(state_admission_rate_avg$STABBR),]
#
# save data 
write_csv(state_admission_rate_avg, 'state_admission_rate_avg.csv')
#