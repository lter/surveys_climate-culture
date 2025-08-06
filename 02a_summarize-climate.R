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

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools")),
            .f = ~ source(file.path("tools", .x)))

## ----------------------------- ##
# Read in Data ----
## ----------------------------- ##

# Read in data
clim_v1 <- read.csv(file = file.path("data", "01a_processed-climate.csv")) %>% 
  # Make empty cells into real NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(nchar(.) == 0,
                                              yes = NA, no = .)))

# Check structure
dplyr::glimpse(clim_v1)

## ----------------------------- ##
# Identify Core Questions ----
## ----------------------------- ##

# Define columns we're interested in summarizing (i.e., not free text)
## We summarize site climate score and activity types without needing to specify that here
questions <- c("fieldwork_duration", "contact_time", 
               "lter_role", "years_with_lter", 
               "general_productivity", "general_wellbeing",
               "belonging_self", "belonging_others", "physical_safety", 
               "information_resources_safety", "self_advocacy",
               "gender_harassment", "field_safety_plan",
               "accomodations", "reporting", 
               "internal_antagonistic_interactions",
               "external_antagonistic_interactions",
               "antagonistic_interactions_stage",
               "frequency_courtesy", "frequency_assistance",
               "frequency_praise", "frequency_interest", 
               "frequency_public_recognition", 
               "gender_identity", "marginalized_identity")

# What's missing?
supportR::diff_check(old = names(clim_v1), new = questions)

## ----------------------------- ##
# Summarize Data ----
## ----------------------------- ##

# Empty lists for storing results
result_list <- list()
score_list <- list()

# Calculate at network level and within sites
for(scope in c("Network", "Site-Specific")){
  ## scope <- "Network"
  
  # Processing message
  message("Summarizing data for ", scope)
  
  # Duplicate data
  clim_v2 <- clim_v1
  
  # Coerce site to "Network" for all responses to use same grouping variables within/across sites
  if(scope == "Network"){
    clim_v2$site <- "Network"
  }
  
  # Remove LNO & missing sites from site-specific aggregation (too few responses)
  if(scope != "Network"){
    clim_v2 <- dplyr::filter(clim_v2, site != "LNO" & !is.na(site))
  }
  
  # Calculate climate score means
  score_list[[paste0(scope, "_climate")]] <- clim_v2 %>% 
    dplyr::group_by(site) %>% 
    dplyr::summarize(climate_score_mean = mean(site_climate_score, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup()
  
  # Check structure
  # dplyr::glimpse(score_list[[paste0(scope, "_climate")]])
  
  # Summarize activity counts
  result_list[[paste0(scope, "_activity")]] <- clim_v2 %>% 
    # Pare down to only needed columns
    dplyr::select(site, dplyr::starts_with("activity_")) %>%
    dplyr::select(-activity_other) %>% 
    # Reshape to long format
    tidyr::pivot_longer(cols = dplyr::starts_with("activity_"),
                        names_to = "category",
                        values_to = "response") %>% 
    # Expand question/answer columns
    dplyr::mutate(answer = dplyr::case_when(
      category == "activity_admin" ~ "Administrative duties",
      category == "activity_education" ~ "Education, outreach, and/or public engagement",
      category == "activity_fieldwork_any" ~ "Field work (Any)",
      category == "activity_fieldwork_land" ~ "Field work (land-based)",
      category == "activity_fieldwork_boat" ~ "Field work (small boats",
      category == "activity_fieldwork_ship" ~ "Field work (ship-based)",
      category == "activity_lab" ~ "Lab work",
      category == "activity_research" ~ "Research",
      category == "activity_model" ~ "Modeling",
      category == "activity_im" ~ "Information management",
      category == "activity_event_inperson" ~ "In-person events",
      category == "activity_event_virtual" ~ "Virtual events",
      category == "activity_synthesis" ~ "Synthesis",
      T ~ NA),
      question = "respondent_activities") %>% 
    # Sum within activity types
    dplyr::group_by(site, question, answer) %>% 
    dplyr::summarize(total = dplyr::n(),
                     ct = sum(response, na.rm = T),
                     percent = (ct / total) * 100,
                     .groups = "keep") %>% 
    dplyr::ungroup()
  
  # Check structure
  # dplyr::glimpse(result_list[[paste0(scope, "_activity")]])
  
  # Empty list for storing question-specific summaries
  q_list <- purrr::map(.x = questions,
                       .f = ~ calc_percents(df = clim_v2, q = .x))
  
  # Check that out
  dplyr::glimpse(q_list[c(1:3)])
  
  # Unlist question-specific dataframe and add to higher-level list
  result_list[[paste0(scope, "_qs")]] <- purrr::list_rbind(x = q_list)
  
} # Close loop

# Process that output
result_v1 <- result_list %>% 
  # Unlist that list
  purrr::list_rbind(x = .)

# Check structure
dplyr::glimpse(result_v1)

# All sites included?
sort(unique(result_v1$site))

# Get a score dataframe too
score_v1 <- purrr::list_rbind(x = score_list)

# Check structure
dplyr::glimpse(score_v1)

## ----------------------------- ##
# Calculate 80th Percentile (Results) ----
## ----------------------------- ##

# Split out Network averages from site-specifics
result_v2a <- dplyr::filter(result_v1, site == "Network")
result_v2b <- dplyr::filter(result_v1, site != "Network")

# Process site-specific data as needed
result_v3b <- result_v2b %>% 
  # Calculate 80th percentile
  dplyr::group_by(question) %>% 
  dplyr::mutate(perc20 = as.numeric(quantile(x = percent, probs = 0.8))) %>% 
  dplyr::ungroup() %>% 
  # Make a site column that is ambiguous for sites below the 80th percentile
  dplyr::mutate(site_ambig = ifelse(percent < perc20,
                                    yes = "Other", no = site),
                .after = site)

# Check structure
dplyr::glimpse(result_v3b)

# Recombine with un-aggregated data
result_v4 <- dplyr::bind_rows(result_v3b, result_v2a)

# Check structure
dplyr::glimpse(result_v4)

## ----------------------------- ##
# Calculate 80th Percentile (Scores) ----
## ----------------------------- ##

# Split out Network averages from site-specifics
score_v2a <- dplyr::filter(score_v1, site == "Network")
score_v2b <- dplyr::filter(score_v1, site != "Network")

# Process site-specific data as needed
score_v3b <- score_v2b %>% 
  # Calculate 80th percentile
  dplyr::mutate(perc20 = as.numeric(quantile(x = climate_score_mean, probs = 0.8))) %>% 
  # Make a site column that is ambiguous for sites below the 80th percentile
  dplyr::mutate(site_ambig = ifelse(climate_score_mean < perc20,
                                    yes = "Other", no = site),
                .after = site)

# Check structure
dplyr::glimpse(score_v3b)

# Recombine with un-aggregated data
score_v4 <- dplyr::bind_rows(score_v3b, score_v2a)

# Check structure
dplyr::glimpse(score_v4)






## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make final objects
result_v99 <- result_df
score_v99 <- score_v4

# Export locally
write.csv(x = result_v99, row.names = F, na = '',
          file = file.path("data", "02a_summarized-climate.csv"))
write.csv(x = score_v99, row.names = F, na = '',
          file = file.path("data", "02a_climate-scores.csv"))

# End ----
