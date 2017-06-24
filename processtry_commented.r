#!/usr/bin/Rscript

# Processing of raw tree trait data downloaded from try-db.org
# Code cleaned up by QDR, 25 May 2017
# Edited a bit by Jay, but mostly by QDR on the week of 18 June 2017

fp <- '/home/jay/Desktop/nasabio/' # set path to data files.

# Load the try species list.
# Must disable quoting to read the entire file.
# To determine fileEncoding: install the readr package
# Run the following command: guess_encoding("your_file", n_max = 1000)
try_all <- read.delim(file.path(fp, 'nasa_try.txt'), fileEncoding='ISO-8859-1',stringsAsFactors = FALSE, quote = '')

nmeas <- table(try_all$AccSpeciesName, try_all$TraitName)
unittable <- table(try_all$TraitName, try_all$OrigUnitStr)


flagLatLong <- function(x)
{
  # If Longitude/Latitude are zeroes, then flag the row
  x$flag[((x$DataName == "Latitude" | x$DataName == "Longitude") &
            (x$OrigValueStr == '0'))] <- 'flag coordinates'
  return(x)
}

# Calls flagLatLong function on the data
try_all <- flagLatLong(try_all)

# Remove some of the columns that have irrelevant information.
# Numeric traits use StdValue instead of OrigValueStr
useStdValueIfNumeric <- function (x)
{
  x$OrigValueStr[!is.na.data.frame(as.numeric(x$OrigValueStr))] <- NA
  return(x)
}

try_all <- useStdValueIfNumeric(try_all)

try_all <- try_all[,c('DatasetID','AccSpeciesName','ObservationID','TraitName','DataName','OrigValueStr','UnitName','StdValue', 'OrigUncertaintyStr','UncertaintyName')]

try_all$correct_value <- try_all$OrigValueStr # Writes character values to correct value column
try_all$correct_value[is.na(try_all$correct_value)] <- try_all$StdValue[is.na(try_all$correct_value)] # Writes Std Value to correct value

# Create new column called flag and fill with NA's
try_all["flag"] <- NA

# check what values Validity can take
table(try_all$OriglName[try_all$DataName == 'Validity'])




# Keep only rows that have a trait measurement. (note: this gets rid of the latitude and longitude rows).
try_nometa <- subset(try_all, TraitName != "")

# Figure out whether individual traits have more than one unit of measurement.
measByUnitTable <- table(try_nometa$TraitName, try_nometa$UnitName)
measByUnitTable[apply(measByUnitTable>0, 1, sum) > 1, ]

# Plant longevity has some blank units and some in years
longevitynounit <- subset(try_nometa, grepl('Plant lifespan',TraitName) & UnitName=='')
# Replace the names with two different values
try_nometa$TraitName[grepl('Plant lifespan',try_nometa$TraitName) & try_nometa$UnitName==''] <- 'Plant lifespan categorical'
try_nometa$TraitName[grepl('Plant lifespan',try_nometa$TraitName) & try_nometa$UnitName=='year'] <- 'Plant lifespan years'

# Seedbank longevity has some entries given as a percentage, some dimensionless, and some in years
seedbanknounit <- subset(try_nometa, grepl('\\(seedbank\\) longevity',TraitName) & UnitName=='dimensionless') # Categorical (e.g. transient, persistent)

try_nometa$TraitName[grepl('\\(seedbank\\) longevity',try_nometa$TraitName) & try_nometa$UnitName=='dimensionless'] <- 'Seedbank longevity categorical'

# Species by trait table for all the traits 
spByTraitTable <- table(try_nometa$AccSpeciesName, try_nometa$TraitName)
apply(spByTraitTable>0, 2, sum) # How many species do we have for each trait?

library(reshape2) # Package for reshaping data frames and matrices into long or wide format.

# Function to be run on each column to either return the mean or the first character result.
# Use most common character value rather than the first one.
mean_with_char <- function(x) {
  xnum <- as.numeric(x)
  if (any(!is.na(xnum))) as.character(mean(xnum, na.rm=TRUE)) else x<- names(table(x))[(which.max(table(x)))]
}

# Function to change columns in a df that only consist of numeric strings to numerics.
numstring2num <- function(x) {
  xnum <- as.numeric(x)
  if (!any(is.na(xnum)) & !is.factor(x)) xnum else x
}

# Cast (convert from long form to wide so that each trait has its own column and can be edited)
try_byobsmean <- dcast(try_nometa[,c(1,2,3,4,5,6)], 
                       ObservationID+DatasetID+AccSpeciesName+DataName ~ TraitName, 
                       value.var='correct_value', 
                       fun.aggregate = mean_with_char)

# Function to return either mean or categorical variable
mean_and_category <- function(x) {
  xnum <- as.numeric(x$correct_value)
  if (any(!is.na(xnum))) {
    data.frame(value = mean(xnum, na.rm=TRUE), category = NA)
  }
  else {
    data.frame(value = NA, category = x$correct_value[1])
  }
}

# Aggregate the trait values by species, calculating the means.
library(dplyr)
try_byspmean <- try_nometa %>%
  group_by(AccSpeciesName, TraitName, DataName) %>%
  do(mean_and_category(.))

# Make try_byspmean into a species x trait matrix. (this is the main output we need)
try_wide <- dcast(try_byspmean[,c(1,2,4)], AccSpeciesName ~ TraitName, fun.aggregate = mean)
try_wide <- try_wide[, apply(try_wide, 2, function(x) sum(!is.na(x))) > 10]

write.csv(try_wide,'/home/jay/Desktop/try_fia_traitmeans.csv',row.names = FALSE)
