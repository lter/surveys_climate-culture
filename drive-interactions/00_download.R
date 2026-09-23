## ------------------------------------------------------------ ##
# Climate & Culture - Download Inputs
## ------------------------------------------------------------ ##
# Purpose:
## Download necessary inputs/data from Google Drive

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls()); gc()

# Get set up
source(file = file.path("-setup.R"))

## ----------------------------- ##
# Download Raw Data ----
## ----------------------------- ##

# Identify relevant Drive folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1NNnp4wXRZjzC5Cfk8Rc3xuPFdjrXeO_z")

# Identify relevant files' in that folder
(drive_raw <- googledrive::drive_ls(path = drive_url) %>% 
  dplyr::filter(name %in% c("2024_LTER Climate_Survey_11_19_2024_11.16_de_identified.xlsx",
    "LTER_Demographic_Survey_November 19_2024_De_Identified.xlsx",
    "climate24_data-key")))

# Download them
purrr::walk2(.x = drive_raw$id, .y = drive_raw$name,
 .f = ~ googledrive::drive_download(file = .x, overwrite = TRUE, 
    path = file.path("data", "raw", paste0("00_", .y))))

# Clear environment
rm(list = ls()); gc()

# End ----

