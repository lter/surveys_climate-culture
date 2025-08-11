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
librarian::shelf(tidyverse)

# Clear environment
rm(list = ls()); gc()

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools")),
            .f = ~ source(file.path("tools", .x)))

# Answers colors that span questions
agree_cols <- c("Strongly disagree" = "#78290f",
                "Disagree" = "#ff7d00",
                "Neutral" = "#ffecd1",
                "Agree" = "#15616d",
                "Strongly agree" = "#001524")

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
ggsave(filename = file.path("graphs", "respondent-activities__network.png"),
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
  ggsave(filename = file.path("graphs", paste0("respondent-activities_", focal_site, ".png")),
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
ggsave(filename = file.path("graphs", "fieldwork-duration__network.png"),
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
  ggsave(filename = file.path("graphs", paste0("fieldwork-duration_", focal_site, ".png")),
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
ggsave(filename = file.path("graphs", "contact-time__network.png"),
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
  ggsave(filename = file.path("graphs", paste0("contact-time_", focal_site, ".png")),
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
ggsave(filename = file.path("graphs", "lter-role__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "lter_role", answers = names(ord)); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", paste0("lter-role_", focal_site, ".png")),
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
ggsave(filename = file.path("graphs", "years-with-lter__network.png"),
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
  ggsave(filename = file.path("graphs", paste0("years-with-lter_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# General Productivity ----
## ----------------------------- ##

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "general_productivity", 
                 answers = names(agree_cols), colors = agree_cols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "general-productivity__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "general_productivity", 
                         answers = names(agree_cols), colors = agree_cols); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", paste0("general-productivity_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))

## ----------------------------- ##
# Template ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("" = "#")

# Make a network-wide version
res_v2 %>% 
  plot_bar_stack(df = ., focal_q = "fieldwork_duration", answer_colors = ord) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "fieldwork-duration__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(res_v2$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- plot_bar_stack(df = dplyr::filter(res_v2, site %in% c("Network", focal_site)), 
                         focal_q = "fieldwork_duration",
                         answer_colors = ord); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", paste0("fieldwork-duration_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "focal_site", "plot"))






# End ----
