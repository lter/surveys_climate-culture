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
librarian::shelf(tidyverse, readxl, ltertools)

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Read in Data ----
## ----------------------------- ##

# Identify file name
clim_rawname <- "2024_LTER Climate_Survey_11_19_2024_11.16_de_identified.xlsx"

# Read in the data
clim_v1 <- readxl::read_excel(path = file.path("data", "raw", clim_rawname))

# Check structure
dplyr::glimpse(clim_v1)

# Get this into the right format for `ltertools`
clim_list <- list()
clim_list[[clim_rawname]] <- clim_v1

## ----------------------------- ##
# Fix Column Names ----
## ----------------------------- ##

# Read in data key
key_obj <- read.csv(file = file.path("data", "climate24_data-key.csv")) %>% 
  ltertools::check_key(key = .)

# Structure check
dplyr::glimpse(key_obj)

# Rename columns more informatively
clim_v2 <- ltertools::standardize(focal_file = clim_rawname,
                         key = key_obj, df_list = clim_list)

# Check structure
dplyr::glimpse(clim_v2)







## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
clim_v99 <- clim_v2

# Export locally
write.csv(x = clim_v99, row.names = F, na = '',
          file = file.path("data", "01_processed-climate.csv"))

# End ----
