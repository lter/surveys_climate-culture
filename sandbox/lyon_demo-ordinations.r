## ---------------------------------------- ##
# Lyon - Demo Ordinations
## ---------------------------------------- ##
# Purpose: explore multivariate visualizations of C&C survey data

# Load libraries
librarian::shelf(tidyverse, vegan, ape, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

## ----------------------------------- ##
# Prep Data ----
## ----------------------------------- ##

# Load data
fine_v01 <- read.csv(file.path("data", "01a_processed-climate.csv"))

# Check structure
dplyr::glimpse(fine_v01)

# Do necessary prep
fine_v02 <- fine_v01 %>% 
    dplyr::select(site, dplyr::where(fn = ~ is.numeric(.) == TRUE)) %>% 
    dplyr::select(-dplyr::starts_with("luq_")) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
        .fns = ~ ifelse(nchar(.) == 0 | is.na(.), yes = 0, no = .)))

# Check structure
dplyr::glimpse(fine_v02)

# Create distance matrices
fine_dist <- vegan::vegdist(x = fine_v02[-1], )



# End ----
