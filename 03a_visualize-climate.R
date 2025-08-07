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
df_prep <- res_v1 %>% 
  dplyr::filter(question == "respondent_activities") %>% 
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
  labs(x = "Percent Responses") +
  # scale_fill_manual(values = ord) +
  scale_color_manual(values = "#000") +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.position = "none",
        axis.title.y = element_blank())

# Make graph for one site
ggplot() +
  geom_bar(data = dplyr::filter(df_prep, site == "MCR"), 
           mapping = aes(x = percent, y = answer, fill = answer),
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

ggsave(filename = file.path("graphs", "demo-plot_activities.png"),
       height = 4, width = 6, units = "in")



# Clear environment
rm(list = c("ord", "df_prep", "df_net"))




# End ----
