## ------------------------------------------------------------ ##
# Climate & Culture - Upload Products
## ------------------------------------------------------------ ##
# Purpose:
## Upload outputs of (some of) the preceding scripts

# Pre-requisites:
## (A) Have run some of the preceding scripts
## (B) Have authenticated `googledrive` R package
### See tutorial here: https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

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
# Upload Tidy Data ----
## ----------------------------- ##

# Identify local graphs
(local_data <- dir(path = file.path("data"), pattern = "climate-02"))

# Identify desired Drive location
drive_loc <- googledrive::as_id("https://drive.google.com/drive/folders/16IpRaaqMlrb4Bgw8KN7q1Fe1biWHyqbP")

# Upload 'em
purrr::walk(.x = local_data,
  .f = ~ googledrive::drive_upload(media = file.path("data", .x),
    overwrite = TRUE, path = drive_loc))

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Upload Network Graphs ----
## ----------------------------- ##

# Identify local graphs
local_graphs <- dir(path = file.path("graphs", "network"),
  pattern = "climate-03_")

# Identify desired Drive location
drive_loc <- googledrive::as_id("https://drive.google.com/drive/folders/1Iiq0cdVplt7jnrG2X-Uuf6yuzW-YaOGb")

# Upload 'em
purrr::walk(.x = local_graphs,
  .f = ~ googledrive::drive_upload(media = file.path("graphs", "network", .x),
    overwrite = TRUE, path = drive_loc))

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Upload Site Graphs ----
## ----------------------------- ##

# Identify local graphs
local_graphs <- dir(path = file.path("graphs", "sites"),
  pattern = "climate-03_")

# Identify desired Drive location
drive_loc <- googledrive::as_id("https://drive.google.com/drive/folders/1FM3IeNZM9RtIzapCyj2EhLAP7Xwz2-X5")

# Upload 'em
purrr::walk(.x = local_graphs,
  .f = ~ googledrive::drive_upload(media = file.path("graphs", "sites", .x),
    overwrite = TRUE, path = drive_loc))

# Clear environment
rm(list = ls()); gc()

# End ----
