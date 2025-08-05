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
# Remove Qualtrics Double Header ----
## ----------------------------- ##

# Remove bad Qualtrics header and any other test rows
clim_v3 <- clim_v2 %>% 
  dplyr::filter(response_id != "Response ID") %>% 
  # Also make all NAs true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(test = nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  # And remove all columns that are entirely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))

# Check structure
dplyr::glimpse(clim_v3)

## ----------------------------- ##
# Quantify Activity Types ----
## ----------------------------- ##

# Survey had a 'select all that apply' question for activities
sort(unique(clim_v3$respondent_activities))

# Want to separate and quantify
clim_v4 <- clim_v3 %>% 
  # Generate columns for each activity type
  dplyr::mutate(
    ## Administrative duties
    activity_admin = ifelse(stringr::str_detect(string = respondent_activities,
                                                pattern = "Administrative duties"),
                            yes = 1, no = 0),
    ## Education, outreach, and/or public engagement
    activity_education = ifelse(stringr::str_detect(string = respondent_activities,
                                                    pattern = "Education, outreach, and/or public engagement"),
                                yes = 1, no = 0),
    ## Any type of field work
    activity_fieldwork_any = ifelse(stringr::str_detect(string = respondent_activities,
                                                        pattern = "Field work"),
                                    yes = 1, no = 0),
    ## Field work (land-based)
    activity_fieldwork_land = ifelse(stringr::str_detect(string = respondent_activities,
                                                         pattern = "Field work (land-based)"),
                                     yes = 1, no = 0),
    ## Field work (small boats)
    activity_fieldwork_boat = ifelse(stringr::str_detect(string = respondent_activities,
                                                         pattern = "Field work (small boats)"),
                                     yes = 1, no = 0),
    activity_fieldwork_ship = ifelse(stringr::str_detect(string = respondent_activities,
                                                         pattern = "Field work (ship-based)"),
                                     yes = 1, no = 0),
    ## Lab work
    activity_lab = ifelse(stringr::str_detect(string = respondent_activities,
                                              pattern = "Lab work"),
                          yes = 1, no = 0),
    ## Research
    activity_research = ifelse(stringr::str_detect(string = respondent_activities,
                                                   pattern = "Research"),
                               yes = 1, no = 0),
    ## Modeling
    activity_model = ifelse(stringr::str_detect(string = respondent_activities,
                                                pattern = "Modeling"),
                            yes = 1, no = 0),
    ## Information management
    activity_im = ifelse(stringr::str_detect(string = respondent_activities,
                                             pattern = "Information management"),
                         yes = 1, no = 0),
    ## In-person events
    activity_event_inperson = ifelse(stringr::str_detect(string = respondent_activities,
                                                         pattern = "In-person events"),
                                     yes = 1, no = 0),
    ## Virtual events
    activity_event_virtual = ifelse(stringr::str_detect(string = respondent_activities,
                                                        pattern = "Virtual events"),
                                    yes = 1, no = 0),
    ## Synthesis
    activity_synthesis = ifelse(stringr::str_detect(string = respondent_activities,
                                                    pattern = "Synthesis"),
                                yes = 1, no = 0),
    .after = respondent_activities) %>% 
  # Rename the 'other' category column
  dplyr::rename(activity_other = respondent_activities_other)

# Check new categories
clim_v4 %>% 
  dplyr::select(dplyr::starts_with("activity_")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

# General structure check
dplyr::glimpse(clim_v4)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
clim_v99 <- clim_v4

# Export locally
write.csv(x = clim_v99, row.names = F, na = '',
          file = file.path("data", "01_processed-climate.csv"))

# End ----
