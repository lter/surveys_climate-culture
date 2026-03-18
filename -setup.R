## ------------------------------------------------------------ ##
# Climate & Culture - Setup
## ------------------------------------------------------------ ##
# Purpose:
## Create needed folders and do other generally-useful setup tasks

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Make Folder(s) ----
## ----------------------------- ##

# Make needed folders
dir.create(path = file.path("data", "raw"), showWarnings = F, recursive = T)
dir.create(path = file.path("graphs", "network"), showWarnings = F, recursive = T)
dir.create(path = file.path("graphs", "sites"), showWarnings = F)

# End ----

