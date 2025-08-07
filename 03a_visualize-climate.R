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

# Make 'network-wide' and 'site-specific' variants of both
## Composites
comp_net <- dplyr::filter(.data = comp_v1, site == "Network")
comp_site <- dplyr::filter(.data = comp_v1, site != "Network")
## General results
res_net <- dplyr::filter(.data = res_v1, site == "Network")
res_site <- dplyr::filter(.data = res_v1, site != "Network")

## ----------------------------- ##
# Activities ----
## ----------------------------- ##

# Identify preferred order
ord <- res_net %>% 
  dplyr::filter(question == "respondent_activities") %>% 
  dplyr::arrange(-percent)

view(ord)

# Identify colors



ggplot(data = dplyr::filter(res_net, question == "respondent_activities"), 
       aes(x = percent, y = answer, fill = answer)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        legend.position = "none",
        axis.title.y = element_blank())



# Clear environment
rm(list = setdiff(ls(), c("comp_net", "comp_site", "res_net", "res_site")))




# End ----
