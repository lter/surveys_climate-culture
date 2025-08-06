## ------------------------------------------------------------ ##
# Climate & Culture - Summarize Climate/Culture Responses
## ------------------------------------------------------------ ##
# Author(s): Nick J Lyon

# Purpose:
## Summarize survey responses into a format that is ready for visualization

# Pre-requisites:
## Have processed survey data (can be done with `01a_process-climate.R`)

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Read in Data ----
## ----------------------------- ##

# Read in data
clim_v1 <- read.csv(file = file.path("data", "01_processed-climate.csv")) %>% 
  # Make empty cells into real NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(nchar(.) == 0,
                                              yes = NA, no = .)))

# Check structure
dplyr::glimpse(clim_v1)

## ----------------------------- ##
# Remove Unwanted Columns ----
## ----------------------------- ##

# Remove unwanted columns
clim_v2 <- clim_v1 %>% 
  # Don't want free text columns at this point
  dplyr::select(-dplyr::ends_with(c("_other", "_text"))) %>% 
  dplyr::select(-dplyr::starts_with(c("open_question_"))) %>% 
  # Remove site-specific columns too
  dplyr::select(-dplyr::starts_with(c("gce_", "kbs_", "luq_"))) %>% 
  # Original respondent activities column in superseded
  dplyr::select(-respondent_activities)

# What was lost?
supportR::diff_check(old = names(clim_v1), new = names(clim_v2))

# Check structure
dplyr::glimpse(clim_v2)











## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
clim_v99 <- clim_v2

# Export locally
write.csv(x = clim_v99, row.names = F, na = '',
          file = file.path("data", "01_summarized-climate.csv"))

# End ----
