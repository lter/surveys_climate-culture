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
  
  # Calculate climate score means
  score_list[[paste0(scope, "_climate")]] <- clim_v2 %>% 
    dplyr::group_by(site) %>% 
    dplyr::summarize(climate_score_mean = mean(site_climate_score, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup()
  
  # Check structure
  # dplyr::glimpse(score_list[[paste0(scope, "_climate")]])
  
  # Empty list for storing question-specific summaries
  q_list <- purrr::map(.x = c(questions, "site_climate_score"),
                       .f = ~ calc_percents(df = clim_v2, q = .x))
  
  # Check that out
  dplyr::glimpse(q_list[c(1:3)])
  
  # Unlist question-specific dataframe and add to higher-level list
  result_list[[paste0(scope, "_qs")]] <- q_list %>% 
    purrr::map(.x = ., 
               .f = ~ dplyr::mutate(.data = .x,
                                    answer = as.character(answer))) %>% 
    purrr::list_rbind(x = .)
  
} # Close loop

# Process that output
result_v1 <- result_list %>% 
  # Unlist that list
  purrr::list_rbind(x = .)

# Check structure
dplyr::glimpse(result_v1)

# All sites included?
sort(unique(result_v1$site))

## ----------------------------- ##
# Export (Summarized Results) ----
## ----------------------------- ##

# Make final objects
result_v99 <- result_v1

# Export locally
write.csv(x = result_v99, row.names = F, na = '',
          file = file.path("data", "02a_summarized-climate.csv"))

## ----------------------------- ##
# Process Respondent Climate Scores ----
## ----------------------------- ##

# Get a dataframe from what our loop returns
score_v1 <- purrr::list_rbind(x = score_list)

# Check structure
dplyr::glimpse(score_v1)

# Wrangle this to identify 80th percentile
score_v2 <- score_v1 %>% 
  dplyr::filter(site != "Network") %>%   
  # Identify 80th percentile
  dplyr::mutate(
    climate_score_perc80 = as.numeric(quantile(x = climate_score_mean, probs = 0.8))) %>% 
  # Get ambiguous site column for this variable
  dplyr::mutate(climate_score_site_ambig = ifelse(climate_score_mean < climate_score_perc80,
                                                  yes = "Other", no = site))

# Check structure
dplyr::glimpse(score_v2)

# Process network-level result
score_net <- score_v1 %>% 
  dplyr::filter(site == "Network")

# Check structure
dplyr::glimpse(score_net)

# Combine network & site-specific results
score_v3 <- dplyr::bind_rows(score_net, score_v2)

# Check structure
dplyr::glimpse(score_v3)

## ----------------------------- ##
# Calculate Composites ----
## ----------------------------- ##

# Process data to prepare to get composite scores
comp_v1 <- result_v1 %>% 
  # Streamline data to only questions included in the composite scores
  dplyr::filter(question %in% c(
    ## Composite climate
    "general_productivity", "general_wellbeing", "site_climate_score",
    ## Composite belonging
    "belonging_self", "belonging_others",
    ## Composite safety (general)
    "physical_safety", "information_resources_safety", "self_advocacy",
    ## Composite safety (social)
    "gender_harassment", "internal_antagonistic_interactions",
    "external_antagonistic_interactions",
    ## Composite trust
    "reporting", "accomodations",
    ## Composite pro-social
    "frequency_courtesy", "frequency_assistance", "frequency_praise",
    "frequency_interest", "frequency_public_recognition")) %>% 
  dplyr::filter((answer %in% c("Agree", "Strongly agree",
                               as.character(8:10),
                               "Yes- and I would know how to do so")) |
                  (stringr::str_detect(string = question, 
                                       pattern = "frequency") & 
                     answer %in% c("Frequently", "Very frequently")) |
                  (stringr::str_detect(string = question, 
                                       pattern = "antagonistic") & 
                     answer == "Never"))

# Check what that leaves us with
comp_v1 %>% 
  dplyr::group_by(question) %>% 
  dplyr::summarize(answers = paste(unique(answer), collapse = "; "),
                   .groups = "keep")

# Sum within questions across remaining answers
comp_v2 <- comp_v1 %>% 
  dplyr::group_by(site, question) %>% 
  dplyr::summarize(perc_total = sum(percent, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(comp_v2)

# Actually calculate composites
comp_v3 <- comp_v2 %>% 
  # Identify composites
  dplyr::mutate(composite = dplyr::case_when(
    ## Composite climate
    question %in% c("general_productivity", "site_climate_score", "general_wellbeing") ~ "composite_climate",
    ## Composite belonging
    question %in% c("belonging_self", "belonging_others") ~ "composite_belonging",
    ## Composite safety (general)
    question %in% c("physical_safety", "information_resources_safety", "self_advocacy") ~ "composite_safety_general",
    ## Composite safety (social)
    question %in% c("gender_harassment", "internal_antagonistic_interactions",
                    "external_antagonistic_interactions") ~ "composite_safety_social",
    ## Composite trust
    question %in% c("reporting", "accomodations") ~ "composite_trust",
    ## Composite pro-social
    question %in% c("frequency_courtesy", "frequency_assistance", "frequency_praise",
                    "frequency_interest", "frequency_public_recognition") ~ "composite_prosocial",
    T ~ NA)) %>% 
  dplyr::filter(!is.na(composite)) %>% 
  # Actually calculate composite scores
  dplyr::group_by(site, composite) %>% 
  dplyr::summarize(score = mean(perc_total, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(comp_v3)

# Want to identify 80th percentile sites for each composite score
comp_v4 <- comp_v3 %>% 
  # Remove network-wide averages
  dplyr::filter(site != "Network") %>% 
  # Calculate 80th percentile per score
  dplyr::group_by(composite) %>% 
  dplyr::mutate(perc80 = as.numeric(quantile(x = score, probs = 0.8))) %>% 
  dplyr::ungroup() %>% 
  # Generate site names that are anonymous beneath 80th percentile
  dplyr::mutate(site_ambig = ifelse(score < perc80, yes = "Other", no = site),
                .after = site)

# Check structure
dplyr::glimpse(comp_v4)

# Tweak data shape before exporting
comp_v5 <- comp_v4 %>% 
  # Pivot longer
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ as.character(.))) %>% 
  dplyr::relocate(composite, .after = site) %>% 
  tidyr::pivot_longer(cols = -site:-composite) %>% 
  # Repair names
  dplyr::mutate(names = paste0(composite, "_", name)) %>% 
  # Pare down columns
  dplyr::select(site, names, value) %>% 
  # Reshape to wide format
  tidyr::pivot_wider(names_from = names, values_from = value) %>% 
  # Make number columns back into numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with(c("_score", "_perc80")),
                              .fns = ~ as.numeric(.)))

# Check structure
dplyr::glimpse(comp_v5)

# Parse the network-level composites
comp_net <- comp_v3 %>% 
  dplyr::filter(site == "Network") %>% 
  dplyr::mutate(composite = paste0(composite, "_score")) %>% 
  tidyr::pivot_wider(names_from = composite, values_from = score)

# Check structure
dplyr::glimpse(comp_net)

# Get a final object
comp_v6 <- comp_v5 %>% 
  # Combine network level composite scores
  dplyr::bind_rows(comp_net) %>% 
  # And attach the respondents mean climate scores too
  dplyr::left_join(score_v3, by = "site") %>% 
  dplyr::relocate(dplyr::starts_with("climate_score"), .after = site)

# Check structure
dplyr::glimpse(comp_v6)

## ----------------------------- ##
# Export (Composite Scores) ----
## ----------------------------- ##

# Make a final object
comp_v99 <- comp_v6

# Export locally
write.csv(x = comp_v99, row.names = F, na = '',
          file = file.path("data", "02a_composite-scores.csv"))

# End ----
