# Author: Jay Jain
# Community & Spatial Ecology Laboratory
# June 06, 2017
# This script attempts to merge traits from  try_fia_traitmeans.csv and Traits_Stevens_FIA.xlsx

# ============================================================================================
# ============================================================================================
# Step One: Read in the data and store it in an R object 
# ============================================================================================
# ============================================================================================

# NOTE: You may have to change the working directory if your data file is in a different location. 
setwd("/home/jay/Desktop/nasabio/")
# filepath <- '/home/jay/Desktop/nasabio/'

# NOTE: Since there are spaces between variable names, we have to set check.names to FALSE
# so that the read.csv function does not replace spaces with periods. 
fia_traitmeans <- read.csv('try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)
print("Created fia_traitmeans object.......")

# The readxl library helps read in data from Excel Spreadsheets
library(readxl)

# Note: The default value for a NA is a blank cell, so we have to specify the NA value as "NA".
traits_stevens_FIA <- read_excel("Traits_Stevens_FIA.xlsx", sheet = "Master", na = "NA")

# Columns #17 (Leaf.area) through #30 (SSD) are the TRY data we want. Column #4 is the name of the 
# tree species. 
try_data <- traits_stevens_FIA[,c(4,17:30)]

# ============================================================================================
# ============================================================================================
# Step Two: Merge the columns without losing any data, using dplyr.                           
# ============================================================================================
# ============================================================================================
library(dplyr)

# Join the data frames and account for duplicate column names
all_traits <- full_join(fia_traitmeans, try_data, by = c("Scientific_Name", 
                                                         "Leaf area per leaf dry mass (specific leaf area, SLA)"="SLA",
                                                         "Leaf carbon/nitrogen (C/N) ratio"="Leaf.CN",
                                                         "Leaf lifespan (longevity)" = "Leaf.lifespan",
                                                         "Leaf nitrogen (N) content per leaf dry mass" = "Leaf.N",
                                                         "Leaf nitrogen/phosphorus (N/P) ratio" = "Leaf.NP",
                                                         "Leaf thickness" ="Leaf.thickness",
                                                         "Plant lifespan years" = "Plant.lifespan",
                                                         "Stem dry mass per stem fresh volume (stem specific density, SSD, wood density)" = "SSD",
                                                         "Seed dry mass" = "Seed.dry.mass"
                                                         ))

# The following method merges the CSV files, but does not get rid of duplicate columns. 
# test_merge <- merge(fia_traitmeans,try_data)
