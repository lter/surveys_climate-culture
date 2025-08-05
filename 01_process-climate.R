## ------------------------------------------------------------ ##
# Climate & Culture - Process Climate/Culture Responses
## ------------------------------------------------------------ ##
# Author(s): Nick J Lyon

# Purpose:
## Process raw survey responses into a format that is ready for summarization and visualization

# Pre-requisites:
## Download raw data and data key (can be done with `00_setup.R`)

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse, readxl)

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Read in Data ----
## ----------------------------- ##

# Identify file name
clim_rawname <- "2024_LTER Climate_Survey_11_19_2024_11.16_de_identified.xlsx"

# Read in the data
clim_v1 <- readxl::read_excel(path = file.path("data", "raw", clim_rawname),
                              sheet = "Sheet1")

# Check structure
dplyr::glimpse(clim_v1)

## ----------------------------- ##
# Fix Column Names ----
## ----------------------------- ##




## ----------------------------- ##
# Export ----
## ----------------------------- ##



# End ----
