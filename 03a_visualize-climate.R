## ------------------------------------------------------------ ##
# Climate & Culture - Visualize Climate/Culture Responses
## ------------------------------------------------------------ ##
# Author(s): Nick J Lyon

# Purpose:
## Visualize survey responses with relevant graphs

# Pre-requisites:
## Have summarized survey data (can be done with `02a_summarize-climate.R`)

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls()); gc()

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools")),
            .f = ~ source(file.path("tools", .x)))

## ----------------------------- ##
# Read in Data ----
## ----------------------------- ##

# Read in composite score data
comp_v1 <- read.csv(file = file.path("data", "02a_composite-scores.csv"))

# Check structure
dplyr::glimpse(comp_v1)

# Also read in summarized data
sry_v1 <- read.csv(file = file.path("data", "02a_summarized-climate.csv"))

# Check structure
dplyr::glimpse(sry_v1)






# End ----
