## ------------------------------------------------------------ ##
# Climate & Culture - Visualize Climate/Culture Responses
## ------------------------------------------------------------ ##
# Author(s): Nick J Lyon

# Purpose:
## Visualize survey responses with relevant graphs

# Pre-requisites:
## Have summarized survey data (can be done with `02a_summarize-climate.R`)

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR, cowplot)

# Clear environment
rm(list = ls()); gc()

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools")),
            .f = ~ source(file.path("tools", .x)))

## ----------------------------- ##
# Read in Data ----
## ----------------------------- ##

# Read in composite score data
comp_v1 <- read.csv(file = file.path("data", "02a_composite-scores.csv"))

# Check structure
dplyr::glimpse(comp_v1)

# Also read in summarized data
res_v1 <- read.csv(file = file.path("data", "02a_summarized-climate.csv"))

# Check structure
dplyr::glimpse(res_v1)

## ----------------------------- ##
# Process Data ----
## ----------------------------- ##

# We also want a different format of the results data for some plots
res_net <- res_v1 %>% 
  dplyr::select(-site, -total:-percent) %>% 
  dplyr::mutate(site = "Network") %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "network_", replacement = "", x = .)) %>% 
  dplyr::distinct()

# Check structure
dplyr::glimpse(res_net)

# Combine with other result data
res_v2 <- dplyr::bind_rows(res_v1, res_net) %>% 
  # Make 'network' the first factor level
  dplyr::mutate(site = factor(site, levels = c("Network", setdiff(sort(site), "Network")))) %>% 
  # Do any needed tidying of particular answers
  dplyr::mutate(answer = dplyr::case_when(
    ## Intense data collection
    answer == "I don't participate in this type of data collection" ~ "0 weeks",
    ## LTER role
    answer == "Administrative staff" ~ "Admin",
    answer == "Information manager" ~ "IM",
    answer == "Education/communication staff" ~ "Education/Communication",
    answer == "Graduate student (non-supervisory role)" ~ "Grad student (non-supervisor)",
    answer == "Graduate student (supervisory role*)" ~ "Grad student (supervisor)",
    answer == "Research technician/research assistant" ~ "Research tech",
    answer == "Undergraduate student" ~ "Undergrad",
    ## Years with LTER
    answer == "More than 10 years" ~ "> 10 years",
    # answer == "" ~ "",
    ## Otherwise, use answer as-is
    T ~ answer))

# Check that out
sort(unique(res_v2$site))
dplyr::glimpse(res_v2)

## ----------------------------- ##
# Activities ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Education or public engagement" = "#fca311",
         "Virtual events" = "#748cab",
         "In-person events" = "#1d2d44",
         "Information management" = "#005f73",
         "Modeling" = "#94d2bd",
         "Synthesis" = "#9b5de5",
         "Lab work" = "#e9d8a6",
         "Field work (small boats)" = "#ee9b00",
         "Field work (ship-based)" = "#ca6702",
         "Field work (land-based)" = "#bb3e03",
         "Field work (any)" = "#9b2226",
         "Research" = "#cdb4db",
         "Administrative duties" = "#dad7cd")

# Prepare data
df_prep <- res_v2 %>% 
  dplyr::filter(question == "respondent_activities" & !is.na(network_percent)) %>% 
  dplyr::mutate(answer = factor(answer, levels = names(ord)))

# Check structure
dplyr::glimpse(df_prep)

# Make a 'network only' version
df_net <- df_prep %>% 
  dplyr::select(question, answer, dplyr::starts_with("network_")) %>% 
  dplyr::distinct()

# Actually generate graph
ggplot(data = df_net, aes(x = network_percent, y = answer, fill = answer, color = "x")) +
  geom_bar(stat = "identity") +
  labs(x = "Percent Responses", title = "Network-Wide Averages") +
  # scale_fill_manual(values = ord) +
  scale_color_manual(values = "#000") +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.position = "none",
        axis.title.y = element_blank())

# Export locally
ggsave(filename = file.path("graphs", "network", "respondent-activities__network.png"),
       height = 4, width = 6, units = "in")

# Loop across sites
for(focal_site in sort(unique(df_prep$site))){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph for one site
  ggplot() +
    geom_bar(data = dplyr::filter(df_prep, site == focal_site), 
             mapping = aes(x = percent, y = answer, fill = answer, color = "x"),
             stat = "identity") +
    geom_point(df_net, mapping = aes(x = network_percent, y = answer),
               size = 3, shape = 18) +
    facet_grid(. ~ site) +
    labs(x = "Percent Responses") +
    scale_color_manual(values = "#000") +
    theme_bw() +
    theme(strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 12),
          legend.position = "none",
          axis.title.y = element_blank())
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites", 
                              paste0("respondent-activities_", focal_site, ".png")),
         height = 4, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "df_prep", "df_net", "focal_site"))

## ----------------------------- ##
# Intense Data Collection ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Other" = "gray80",
         "0 weeks" = "#e9d8a6",
         "1-3 weeks" = "#ee9b00",
         "1-3 months" = "#bb3e03",
         "Longer" = "#540b0e")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "fieldwork_duration", 
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "fieldwork-duration__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "fieldwork_duration",
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites", 
                              paste0("fieldwork-duration_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# Non-Data Collection Community ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Other" = "gray80",
         "Daily" = "#ffe6a7",
         "Weekly" = "#bb9457",
         "Monthly" = "#99582a",
         "Quarterly" = "#432818")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "contact_time",
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "contact-time__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "contact_time",
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites", 
                              paste0("contact-time_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# LTER Role ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Other" = "gray80",
         "Education/Communication" = "#264653",
         "IM" = "#2a9d8f",
         "Admin" = "#fff",
         "Volunteer" = "#e9c46a",
         "Research tech" = "#e76f51",
         "Undergrad" = "#dad7cd",
         "Grad student (non-supervisor)" = "#a3b18a",
         "Grad student (supervisor)" = "#588157",
         "Postdoc" = "#3a5a40",
         "Investigator" = "#344e41",
         "Prefer not to answer" = "#000")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "lter_role", answers = names(ord)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "lter-role__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "lter_role", answers = names(ord)); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites", 
                              paste0("lter-role_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# LTER Years ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("< 1 year" = "#caf0f8",
         "2-5 years" = "#90e0ef",
         "6-10 years" = "#0096c7",
         "> 10 years" = "#023e8a",
         "Prefer not to answer" = "#000")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "years_with_lter", 
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "years-with-lter__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "years_with_lter", 
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites", 
                              paste0("years-with-lter_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# Agreement Questions ----
## ----------------------------- ##

# Answers colors that span agreement questions
agree_cols <- c("Strongly disagree" = "#78290f",
                "Disagree" = "#ff7d00",
                "Neutral" = "#ffecd1",
                "Agree" = "#15616d",
                "Strongly agree" = "#001524")

# Loop across 'agreement questions'
## Where allowed answers are conserved across several questions
for(agree_q in c("general_productivity", "general_wellbeing", 
                 "belonging_self", "belonging_others",
                 "physical_safety", "self_advocacy",
                 "information_resources_safety")){
  
  # Progress message
  message("Graphs for '", agree_q, "'")
  
  # Tweak delimeter for graphs
  agree_q_dash <- gsub(pattern = "_", replacement = "-", x = agree_q)
  
  # Make a network-wide version
  res_v2 %>% 
    plot_bar_stack(df = ., focal_q = agree_q, 
                   answers = names(agree_cols), colors = agree_cols) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Export locally
  ggsave(filename = file.path("graphs", "network", paste0(agree_q_dash, "__network.png")),
         height = 4, width = 8, units = "in")
  
  # Loop across sites
  for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
    
    # Progress message
    message("Making graph for ", focal_site)
    
    # Make graph
    plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                           focal_q = agree_q, 
                           answers = names(agree_cols), colors = agree_cols); plot
    
    # Export locally
    ggsave(filename = file.path("graphs", "sites", 
                                paste0(agree_q_dash, "_", focal_site, ".png")),
           height = 6, width = 6, units = "in")
    
  } # Close site loop
} # Close question loop

# Clear environment
rm(list = c("focal_site", "plot", "agree_q", "agree_q_dash", "agree_cols"))

## ----------------------------- ##
# Site Climate Score ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("1" = "#9b2226", "2" = "#ae2012", 
         "3" = "#bb3e03", "4" = "#ca6702", 
         "5" = "#ee9b00", "6" = "#e9d8a6",
         "7" = "#94d2bd", "8" = "#0a9396", 
         "9" = "#005f73", "10" = "#001219")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "site_climate_score", 
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "site-climate-score__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "site_climate_score", 
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites",
                              paste0("site-climate-score_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# Gender Identity ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Prefer not to say" = "#000",
         "Man" = "#d90368",
         "Non-binary" = "#ffd400",
         "Woman" = "#147df5",
         "Other" = "gray80")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "gender_identity", 
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "gender-identity__network.png"),
       height = 4, width = 10, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "gender_identity", 
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites",
                              paste0("gender-identity_", focal_site, ".png")),
         height = 6, width = 10, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# Gender Harassment ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Prefer not to say" = "#000",
         "Yes, it happened to me" = "#f95738",
         "Yes, I witnessed it happening to someone else" = "#ee964b",
         "Yes, I heard about it happening to someone else" = "#f4d35e",
         "No" = "#0d3b66")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "gender_harassment", 
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "gender-harassment__network.png"),
       height = 4, width = 10, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "gender_harassment", 
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites",
                              paste0("gender-harassment_", focal_site, ".png")),
         height = 6, width = 10, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# Accomodations / Reporting ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Prefer not to say" = "#000",
         "No- I wouldn't feel comfortable" = "#147df5",
         "Unsure" = "#feb204",
         "Yes- but I wouldn't know where to start" = "#a5bf12",
         "Yes- and I would know how to do so" = "#38601d",
         "Other" = "gray80")

# Loop across questions with the above answers
for(know_q in c("accomodations", "reporting")){
  
  # Progress message
  message("Graphs for '", know_q, "'")
  
  # Tweak delimeter for graphs
  know_q_dash <- gsub(pattern = "_", replacement = "-", x = know_q)
  
  # Make a network-wide version
  res_v2 %>% 
    plot_bar_stack(df = ., focal_q = know_q, 
                   answers = names(ord), colors = ord) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Export locally
  ggsave(filename = file.path("graphs", "network", paste0(know_q_dash, "__network.png")),
         height = 4, width = 10, units = "in")
  
  # Loop across sites
  for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
    
    # Progress message
    message("Making graph for ", focal_site)
    
    # Make graph
    plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                           focal_q = know_q, 
                           answers = names(ord), colors = ord); plot
    
    # Export locally
    ggsave(filename = file.path("graphs", "sites",
                                paste0(know_q_dash, "_", focal_site, ".png")),
           height = 6, width = 10, units = "in")
    
  } # Close site loop
} # Close question loop

# Clear environment
rm(list = c("ord", "focal_site", "plot", "know_q", "know_q_dash"))

## ----------------------------- ##
# Field Safety Plan / Marginalized Identity ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Prefer not to say" = "#000",
         "No" = "#ffa62b",
         "Unsure" = "#ede7e3",
         "Yes" = "#82c0cc",
         "Other" = "gray80")

# Iterate across questions that use these answers
for(yn_q in c("field_safety_plan", "marginalized_identity")){
  
  # Progress message
  message("Graphs for '", yn_q, "'")
  
  # Tweak delimeter for graphs
  yn_q_dash <- gsub(pattern = "_", replacement = "-", x = yn_q)
  
  # Make a network-wide version
  res_v2 %>% 
    plot_bar_stack(df = ., focal_q = yn_q, 
                   answers = names(ord), colors = ord) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Export locally
  ggsave(filename = file.path("graphs", "network", paste0(yn_q_dash, "__network.png")),
         height = 4, width = 10, units = "in")
  
  
  # Loop across sites
  for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
    
    # Progress message
    message("Making graph for ", focal_site)
    
    # Make graph
    plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                           focal_q = yn_q, 
                           answers = names(ord), colors = ord); plot
    
    # Export locally
    ggsave(filename = file.path("graphs", "sites",
                                paste0(yn_q_dash, "_", focal_site, ".png")),
           height = 6, width = 10, units = "in")
    
  } # Close site loop
} # Close question loop

# Clear environment
rm(list = c("ord", "focal_site", "plot", "yn_q", "yn_q_dash"))

## ----------------------------- ##
# Antagonistic Interactions ----
## ----------------------------- ##

# Identify preferred order & colors
## These 'bad frequency' questions get a different color scheme than 'good frequency'
ord <- c("Very frequently" = "#720026",
         "Frequently" = "#ce4257",
         "Occasionally" = "#ff9b54",
         "Rarely" = "#cbf3f0",
         "Never" = "#2ec4b6")

# Loop across 'agreement questions'
## Where allowed answers are conserved across several questions
for(antag_q in c("external_antagonistic_interactions",
                 "internal_antagonistic_interactions")){
  
  # Progress message
  message("Graphs for '", antag_q, "'")
  
  # Tweak delimeter for graphs
  antag_q_dash <- gsub(pattern = "_", replacement = "-", x = antag_q)
  
  # Make a network-wide version
  res_v2 %>% 
    plot_bar_stack(df = ., focal_q = antag_q, 
                   answers = names(ord), colors = ord) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Export locally
  ggsave(filename = file.path("graphs", "network", paste0(antag_q_dash, "__network.png")),
         height = 4, width = 8, units = "in")
  
  # Loop across sites
  for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
    
    # Progress message
    message("Making graph for ", focal_site)
    
    # Make graph
    plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                           focal_q = antag_q, 
                           answers = names(ord), colors = ord); plot
    
    # Export locally
    ggsave(filename = file.path("graphs", "sites", 
                                paste0(antag_q_dash, "_", focal_site, ".png")),
           height = 6, width = 6, units = "in")
    
  } # Close site loop
} # Close question loop

# Clear environment
rm(list = c("focal_site", "plot", "antag_q", "antag_q_dash"))

## ----------------------------- ##
# Antagonistic Interaction Stage ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("Prefer not to say" = "#000",
         "Unknown" = "#fff",
         "External community" = "#f26419",
         "Visitor" = "#f6ae2d",
         "Staff (Non-LTER)" = "#86bbd8",
         "Staff (LTER)" = "#33658a",
         "Undergraduate" = "#e0aaff",
         "Grad Student" = "#c77dff",
         "Postdoc" = "#7b2cbf",
         "Resident researchers" = "#5a189a",
         "PI team" = "#3c096c",
         "Other" = "gray80")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "antagonistic_interaction_stage", 
                 answers = names(ord), colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "network", "antagonistic-interaction-stage__network.png"),
       height = 4, width = 10, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "antagonistic_interaction_stage", 
                         answers = names(ord), colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", "sites",
                              paste0("antagonistic-interaction-stage_", focal_site, ".png")),
         height = 6, width = 10, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# 'Good' Frequency Questions ----
## ----------------------------- ##

# Identify preferred order & colors
## These 'good frequency' questions get a different color scheme than 'bad frequency'
ord <- c("Never" = "#004b23",
         "Rarely" = "#008000",
         "Occasionally" = "#38b000",
         "Frequently" = "#70e000",
         "Very frequently" = "#ccff33")

# Loop across 'agreement questions'
## Where allowed answers are conserved across several questions
for(freq_q in c("frequency_assistance", "frequency_courtesy", 
                "frequency_interest", "frequency_praise", 
                "frequency_public_recognition")){
  
  # Progress message
  message("Graphs for '", freq_q, "'")
  
  # Tweak delimeter for graphs
  freq_q_dash <- gsub(pattern = "_", replacement = "-", x = freq_q)
  
  # Make a network-wide version
  res_v2 %>% 
    plot_bar_stack(df = ., focal_q = freq_q, 
                   answers = names(ord), colors = ord) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Export locally
  ggsave(filename = file.path("graphs", "network", paste0(freq_q_dash, "__network.png")),
         height = 4, width = 8, units = "in")
  
  # Loop across sites
  for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
    
    # Progress message
    message("Making graph for ", focal_site)
    
    # Make graph
    plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                           focal_q = freq_q, 
                           answers = names(ord), colors = ord); plot
    
    # Export locally
    ggsave(filename = file.path("graphs", "sites", 
                                paste0(freq_q_dash, "_", focal_site, ".png")),
           height = 6, width = 6, units = "in")
    
  } # Close site loop
} # Close question loop

# Clear environment
rm(list = c("ord", "focal_site", "plot", "freq_q", "freq_q_dash"))

## ----------------------------- ##
# Composite Scores (Network) ----
## ----------------------------- ##

# Site colors
site_cols <- c("AND" = "#386641", "ARC" = "#dda15e", "BLE" = "#005f73", 
               "BNZ" = "#386641", "CAP" = "#ffc8dd", "CCE" = "#0a9396", 
               "CDR" = "#bc6c25", "FCE" = "#0a9396", "GCE" = "#0a9396", 
               "HBR" = "#386641", "HFR" = "#386641", "JRN" = "#bc6c25", 
               "KBS" = "#a7c957", "KNZ" = "#a7c957", "LUQ" = "#386641", 
               "MCM" = "#94d2bd", "MCR" = "#0a9396", "MSP" = "#ffc8dd", 
               "NES" = "#005f73", "NGA" = "#005f73", "NTL" = "#94d2bd", 
               "NWT" = "#dda15e", "PAL" = "#005f73", "PIE" = "#0a9396", 
               "SBC" = "#0a9396", "SEV" = "#bc6c25", "VCR" = "#0a9396", 
               "Other" = "#3a0ca3")

# Site shapes
site_shps <- c("AND" = 21, "ARC" = 21, "BLE" = 21, "BNZ" = 22, 
               "CAP" = 21, "CCE" = 21, "CDR" = 21, "FCE" = 22, 
               "GCE" = 23, "HBR" = 23, "HFR" = 24, "JRN" = 22, 
               "KBS" = 21, "KNZ" = 22, "LUQ" = 25, "MCM" = 21, 
               "MCR" = 24, "MSP" = 22, "NES" = 22, "NGA" = 23, 
               "NTL" = 22, "NWT" = 22, "PAL" = 24, "PIE" = 25, 
               "SBC" = 22, "SEV" = 23, "VCR" = 23, "Other" = 21)

# Make a list for storing outputs
comp_plot_list <- list()

# Loop across composite score questions
for(focal_comp in paste0("composite_", c("belonging", "climate", "prosocial",
                                         "safety_general", "safety_social", 
                                         "trust"))){
  # focal_comp <- "composite_belonging"
  
  # Progress message
  message("Graphs for '", focal_comp, "'")
  
  # Get some where the delimeter is changed
  focal_dash <- gsub(pattern = "_", replacement = "-", x = focal_comp)
  focal_lab <- gsub(pattern = "composite_", replacement = "", x = focal_comp)
  
  # Prepare the data
  comp_prep <- comp_v1 %>% 
    # Pare down columns and rename more generically
    dplyr::select(site, dplyr::starts_with(focal_comp)) %>% 
    supportR::safe_rename(data = ., bad_names = names(.),
                          good_names = gsub(pattern = paste0(focal_comp, "_"), 
                                            replacement = "", x = names(.))) %>% 
    # Order the 'ambiguous site' column by score (high to low)
    dplyr::arrange(-score) %>% 
    dplyr::mutate(site_ambig = factor(site_ambig, levels = unique(site_ambig)))
  
  # Actually make the graph and add to list
  comp_plot_list[[focal_comp]] <- ggplot(comp_prep, aes(x = 'x', y = score)) +
    # Add horizontal lines for 80th percentile and network average
    geom_hline(yintercept = unique(comp_prep$perc80), linetype = 2) +
    geom_hline(yintercept = unique(comp_prep$network_score), linetype = 1) +
    # Add points for site means (only >80th percentile sites are named)
    geom_jitter(aes(fill = site_ambig, shape = site_ambig),
                size = 4, alpha = 0.8, width = 0.1, height = 0) +
    # Customize labels / theme elements
    labs(y = paste0("Composite Score: ", stringr::str_to_title(gsub("_", " ", focal_lab)))) +
    scale_fill_manual(values = site_cols) +
    scale_shape_manual(values = site_shps) +
    theme_bw() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.8, 0.3),
          legend.background = element_blank(),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10))
  
  # Assmemble the figure
  cowplot::plot_grid(plotlist = comp_plot_list, nrow = 2, labels = "AUTO")
  
  # Export locally
  ggsave(filename = file.path("graphs", "network", paste0("composite-scores__network.png")),
         height = 10, width = 10, units = "in")
}

# Clear environment
rm(list = c("comp_prep", "comp_plot_list", "site_shps",
            "focal_comp", "focal_dash", "focal_lab"))

## ----------------------------- ##
# Composite Scores (Sites) ----
## ----------------------------- ##

# Use different shapes for this one (all circles except 'other')
site_shps <- c("AND" = 21, "ARC" = 21, "BLE" = 21, "BNZ" = 21, 
               "CAP" = 21, "CCE" = 21, "CDR" = 21, "FCE" = 21, 
               "GCE" = 21, "HBR" = 21, "HFR" = 21, "JRN" = 21, 
               "KBS" = 21, "KNZ" = 21, "LUQ" = 21, "MCM" = 21, 
               "MCR" = 21, "MSP" = 21, "NES" = 21, "NGA" = 21, 
               "NTL" = 21, "NWT" = 21, "PAL" = 21, "PIE" = 21, 
               "SBC" = 21, "SEV" = 21, "VCR" = 21,  "Other" = 23)

# Make a list for storing outputs
comp_site_list <- list()

# Loop across sites
for(focal_site in sort(unique(comp_v1$site))){
  # focal_site <- "AND"
  
  # Progress message
  message("Composite score graphs for ", focal_site)
  
  # Make the 'site' column ambiguous except for the focal site
  comp_v2 <- comp_v1 %>% 
    dplyr::mutate(site = ifelse(site == focal_site,
                                yes = focal_site, no = "Other")) %>% 
    dplyr::mutate(site = factor(site, levels = c(focal_site, "Other")))
  
  # Now loop across composite score questions
  for(focal_comp in paste0("composite_", c("belonging", "climate", "prosocial",
                                           "safety_general", "safety_social", 
                                           "trust"))){
    # focal_comp <- "composite_belonging"
    
    # Progress message
    message("Graphs for '", focal_comp, "'")
    
    # Get some where the delimeter is changed
    focal_dash <- gsub(pattern = "_", replacement = "-", x = focal_comp)
    focal_lab <- gsub(pattern = "composite_", replacement = "", x = focal_comp)
    
    # Prepare the data
    comp_prep <- comp_v2 %>% 
      # Pare down columns and rename more generically
      dplyr::select(site, dplyr::starts_with(focal_comp)) %>% 
      supportR::safe_rename(data = ., bad_names = names(.),
                            good_names = gsub(pattern = paste0(focal_comp, "_"), 
                                              replacement = "", x = names(.)))
    
    # Actually make the graph and add to list
    comp_site_list[[focal_comp]] <- ggplot(comp_prep, aes(x = 'x', y = score)) +
      # Add horizontal lines for network average
      geom_hline(yintercept = unique(comp_prep$network_score), linetype = 1) +
      # Add points for site means (only >80th percentile sites are named)
      geom_jitter(aes(fill = site, shape = site), size = 4,
                  alpha = 0.8, width = 0.1, height = 0) +
      # Customize labels / theme elements
      labs(y = paste0("Composite Score: ", stringr::str_to_title(gsub("_", " ", focal_lab)))) +
      scale_fill_manual(values = site_cols) +
      scale_shape_manual(values = site_shps) +
      theme_bw() +
      theme(legend.position = "inside",
            legend.position.inside = c(0.8, 0.3),
            legend.background = element_blank(),
            legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 10))
    
    # Assmemble the figure
    cowplot::plot_grid(plotlist = comp_site_list, nrow = 2, labels = "AUTO")
    
    # Export locally
    ggsave(filename = file.path("graphs", "sites", 
                                paste0("composite-scores__", focal_site, ".png")),
           height = 10, width = 10, units = "in")
    
  } # Close composite loop
} # Close site loop

# Clear environment
rm(list = c("comp_prep", "comp_site_list", "site_shps",
            "focal_comp", "focal_dash", "focal_lab"))

# End ----
