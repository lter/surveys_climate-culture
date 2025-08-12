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
                                                         pattern = "land-based"),
                                     yes = 1, no = 0),
    ## Field work (small boats)
    activity_fieldwork_boat = ifelse(stringr::str_detect(string = respondent_activities,
                                                         pattern = "small boats"),
                                     yes = 1, no = 0),
    ## Field work (ship-based)
    activity_fieldwork_ship = ifelse(stringr::str_detect(string = respondent_activities,
                                                         pattern = "ship-based"),
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
# Quantify Antagonistic Ixn Stages ----
## ----------------------------- ##

# Survey had a 'select all that apply' question for activities
sort(unique(clim_v3$antagonistic_interactions_stage))

# Want to separate and quantify
clim_v5 <- clim_v4 %>% 
  # Generate columns for each activity type
  dplyr::mutate(
    ## Don't know
    antagonistic_stage_unknown = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage, pattern = "Don’t know"),
      yes = 1, no = 0),
    ## External community member
    antagonistic_stage_external = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage, pattern = "External community member"),
      yes = 1, no = 0),
    ## Non-LTER staff
    antagonistic_stage_nonlterstaff = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage, pattern = "Non-LTER staff"),
      yes = 1, no = 0),
    ## LTER staff
    antagonistic_stage_lterstaff = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage, pattern = "LTER staff"),
      yes = 1, no = 0),
    ## Visiting faculty/researcher
    antagonistic_stage_visitor = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage, pattern = "Visiting faculty/researcher"),
      yes = 1, no = 0),
    ## Other resident faculty and researchers
    antagonistic_stage_residents = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "Other resident faculty and researchers"),
      yes = 1, no = 0),
    ## Undergraduate student
    antagonistic_stage_undergrad = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "Undergraduate student"),
      yes = 1, no = 0),
    ## Graduate student
    antagonistic_stage_grad = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "Graduate student"),
      yes = 1, no = 0),
    ## Postdoctoral scientist
    antagonistic_stage_postdoc = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "Postdoctoral scientist"),
      yes = 1, no = 0),
    ## PI team (lead PI and Co PIs)
    antagonistic_stage_pi = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "PI team"),
      yes = 1, no = 0),
    ## Prefer not to say
    antagonistic_stage_prefernotsay = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "Prefer not to say"),
      yes = 1, no = 0),
    ## Prefer not to say
    antagonistic_stage_other = ifelse(stringr::str_detect(
      string = antagonistic_interactions_stage,
      pattern = "Other"),
      yes = 1, no = 0),
    ## Then add them after the original column
    .after = antagonistic_interactions_stage) %>% 
  # Remove bad partial string match counts
  dplyr::mutate(
    antagonistic_stage_other = antagonistic_stage_other - antagonistic_stage_residents,
    antagonistic_stage_lterstaff = antagonistic_stage_lterstaff - antagonistic_stage_nonlterstaff
    )

# Check new categories
clim_v5 %>% 
  dplyr::select(antagonistic_interactions_stage, dplyr::starts_with("antagonistic_stage_")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

# General structure check
dplyr::glimpse(clim_v5)

## ----------------------------- ##
# Check/Repair Sites ----
## ----------------------------- ##

# What sites are in the data?
sort(unique(clim_v5$site))

# Want to extract site abbreviations (for graphs)
clim_v6 <- clim_v5 %>% 
  # Resolve "Other" entry (just one response had this)
  dplyr::mutate(site = ifelse(site == "Other",
                              # Response in 'site other' column: "pie and gce"
                              yes = "Plum Island Ecosystems LTER (PIE)", 
                              no = site)) %>% 
  # Split into separate columns by parentheses
  tidyr::separate_wider_delim(cols = site, delim = " (",
                              names = c("site_name", "site")) %>% 
  # Remove trailing parentheses left over
  dplyr::mutate(site = gsub(pattern = "\\)", replacement = "", x = site)) %>% 
  # Relocate to start of data
  dplyr::relocate(site, site_name, .before = response_id)

# What sites are left?
sort(unique(clim_v6$site))

# Check structure
dplyr::glimpse(clim_v6)

## ----------------------------- ##
# Fix Categorical Missing Values ----
## ----------------------------- ##

# Need to fill missing values in some columns
clim_v7 <- clim_v6 %>% 
  # Antagonistic interactions
  dplyr::mutate(antagonistic_interactions_stage = ifelse(is.na(antagonistic_interactions_stage),
                                                         yes = "No antagonistic interactions",
                                                         no = antagonistic_interactions_stage))

# Check structure
dplyr::glimpse(clim_v7)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
clim_v99 <- clim_v7

# Export locally
write.csv(x = clim_v99, row.names = F, na = '',
          file = file.path("data", "01a_processed-climate.csv"))

# End ----
