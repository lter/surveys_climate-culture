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

## ----------------------------- ##
# Export (Summarized Results) ----
## ----------------------------- ##

# Make final objects
result_v99 <- result_v1

# Export locally
write.csv(x = result_v99, row.names = F, na = '',
          file = file.path("data", "02a_summarized-climate.csv"))

## ----------------------------- ##
# Calculate Composites ----
## ----------------------------- ##

# Process data to prepare to get composite scores
comp_v1 <- result_v1 %>% 
  # Streamline data to only questions included in the composite scores
  dplyr::filter(question %in% c("belonging_others", "belonging_self", "general_productivity",
                                "general_wellbeing", "information_resources_safety",
                                "physical_safety", "self_advocacy")) %>% 
  dplyr::filter(answer %in% c("Neutral", "Disagree", "Strongly disagree") != T) %>% 
  # Sum within questions across answers
  dplyr::group_by(site, question) %>% 
  dplyr::summarize(perc_total = sum(percent, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(comp_v1)


sort(unique(comp_v1$question))

# Actually calculate composites
comp_v2 <- comp_v1 %>% 
  # Identify composites
  dplyr::mutate(composite = dplyr::case_when(
    question %in% c("general_productivity") ~ "composite_climate",
    question %in% c("belonging_self", "belonging_others") ~ "composite_belonging",
    question %in% c("self_advocacy") ~ "composite_civility",
    question %in% c("physical_safety") ~ "composite_safety_general",
    question %in% c("general_wellbeing") ~ "composite_safety_social",
    question %in% c("information_resources_safety") ~ "composite_inst_knowledge",
    T ~ NA)) %>% 
  dplyr::filter(!is.na(composite)) %>% 
  # Actually calculate composite scores
  dplyr::group_by(site, composite) %>% 
  dplyr::summarize(score = mean(perc_total, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(comp_v2)

# Want to identify 80th percentile sites for each composite score
comp_v3 <- comp_v2 %>% 
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
dplyr::glimpse(comp_v3)

## ----------------------------- ##
# Export (Composite Scores) ----
## ----------------------------- ##

# Make a final object
comp_v99 <- comp_v3

# Export locally
write.csv(x = comp_v99, row.names = F, na = '',
          file = file.path("data", "02a_composite-scores.csv"))

## ----------------------------- ##
# Process Respondent Climate Score ----
## ----------------------------- ##

# Get a score dataframe for the original climate score too
score_v1 <- purrr::list_rbind(x = score_list)

# Check structure
dplyr::glimpse(score_v1)

## ----------------------------- ##
# Export (Respondent Climate Scores) ----
## ----------------------------- ##

# Make a final object
score_v99 <- score_v1

# Export locally
write.csv(x = score_v99, row.names = F, na = '',
          file = file.path("data", "02a_climate-score.csv"))

# End ----
