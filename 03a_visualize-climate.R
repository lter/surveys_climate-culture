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
  dplyr::mutate(site = factor(site, levels = c("Network", setdiff(sort(site), "Network"))))

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
ggsave(filename = file.path("graphs", "respondent_activities__network.png"),
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
  ggsave(filename = file.path("graphs", paste0("respondent_activities_", focal_site, ".png")),
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

# Prepare data
df_prep <- res_v2 %>% 
  dplyr::filter(question == "fieldwork_duration") %>% 
  dplyr::mutate(answer = ifelse(answer == "I don't participate in this type of data collection",
                                yes = "0 weeks", no = answer)) %>% 
  dplyr::mutate(answer = factor(answer, levels = rev(names(ord))))

# Check structure
dplyr::glimpse(df_prep)

# Make a network-wide version
ggplot(df_prep, aes(x = site, y = percent, fill = answer, color = "x")) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  scale_fill_manual(values = ord) +
  labs(y = "Percent Responses") +
  guides(color = "none") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10))

# Export locally
ggsave(filename = file.path("graphs", "fieldwork_duration__network.png"),
       height = 4, width = 8, units = "in")

# Loop across sites
for(focal_site in setdiff(sort(unique(df_prep$site)), "Network")){
  
  # Progress message
  message("Making graph for ", focal_site)
  
  # Make graph
  plot <- ggplot(dplyr::filter(df_prep, site %in% c("Network", focal_site)), 
                      aes(x = site, y = percent, fill = answer, color = "x")) +
    geom_bar(stat = "identity") +
    scale_color_manual(values = "#000") +
    scale_fill_manual(values = ord) +
    labs(y = "Percent Responses") +
    guides(color = "none") +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10)); plot
  
  # Export locally
  ggsave(filename = file.path("graphs", paste0("fieldwork_duration_", focal_site, ".png")),
         height = 6, width = 6, units = "in")
  
} # Close loop

# Clear environment
rm(list = c("ord", "df_prep", "focal_site", "plot"))

## ----------------------------- ##
# Template ----
## ----------------------------- ##

# Identify preferred order & colors
ord <- c("" = "#")

# Prepare data
df_prep <- res_v1 %>% 
  dplyr::filter(question == "respondent_activities") %>% 
  dplyr::mutate(answer = factor(answer, levels = names(ord)))

# Check structure
dplyr::glimpse(df_prep)

# Make a 'network only' version
df_net <- df_prep %>% 
  dplyr::select(question, answer, dplyr::starts_with("network_")) %>% 
  dplyr::distinct()


# Clear environment
rm(list = c("ord", "df_prep", "df_net", "focal_site"))






# End ----
