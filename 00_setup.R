## ------------------------------------------------------------ ##
# Climate & Culture - Setup
## ------------------------------------------------------------ ##
# Author(s): Nick J Lyon

# Purpose:
## Create needed folders and download necessary data from Google Drive

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Make Folder(s) ----
## ----------------------------- ##

# Make needed folders
dir.create(path = file.path("data", "raw"), showWarnings = F, recursive = T)

## ----------------------------- ##
# Download Climate Data ----
## ----------------------------- ##

# Identify relevant Drive folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1NNnp4wXRZjzC5Cfk8Rc3xuPFdjrXeO_z")

# Grab ID of relevant file
drive_clim <- googledrive::drive_ls(path = drive_url) %>% 
  dplyr::filter(name == "2024_LTER Climate_Survey_11_19_2024_11.16_de_identified.xlsx")

# Check that worked
drive_clim

# Download it
googledrive::drive_download(file = drive_clim$id, overwrite = T, 
                            path = file.path("data", "raw", drive_clim$name))

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Download Demographic Data ----
## ----------------------------- ##

# Identify relevant Drive folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1NNnp4wXRZjzC5Cfk8Rc3xuPFdjrXeO_z")

# Grab ID of relevant file
drive_demo <- googledrive::drive_ls(path = drive_url) %>% 
  dplyr::filter(name == "LTER_Demographic_Survey_November 19_2024_De_Identified.xlsx")

# Check that worked
drive_demo

# Download it
googledrive::drive_download(file = drive_demo$id, overwrite = T, 
                            path = file.path("data", "raw", drive_demo$name))

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Download Data Key ----
## ----------------------------- ##

# Identify relevant Drive folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1NNnp4wXRZjzC5Cfk8Rc3xuPFdjrXeO_z")

# Grab ID of relevant file
drive_key <- googledrive::drive_ls(path = drive_url) %>% 
  dplyr::filter(name == "climate24_data-key")

# Check that worked
drive_key

# Download it
googledrive::drive_download(file = drive_key$id, overwrite = T, type = "csv",
                            path = file.path("data", drive_key$name))

# Clear environment
rm(list = ls()); gc()

# End ----

