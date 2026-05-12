## ---------------------------------------- ##
# Lyon - Demo Ordinations
## ---------------------------------------- ##
# Purpose: explore multivariate visualizations of C&C survey data

# Load libraries
librarian::shelf(tidyverse, vegan, ape, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

## ----------------------------------- ##
# Prep Data ----
## ----------------------------------- ##

# Load data
comp_v01 <- read.csv(file.path("data", "02a_composite-scores.csv"))

# Check structure
dplyr::glimpse(comp_v01)

# Do necessary prep
comp_v02 <- comp_v01 %>% 
    dplyr::select(site, climate_score_mean, dplyr::ends_with("_score"))

# What's lost?
supportR::diff_check(old = names(comp_v01), new = names(comp_v02))

# Check structure
dplyr::glimpse(comp_v02)

# Compute distance matrix
comp_dist <- vegan::vegdist(x = comp_v02[-1], method = "bray")

# Perform PCoA / NMS
pcoa_mod <- ape::pcoa(comp_dist)
nms_mod <- vegan::metaMDS(comp_dist, autotransform = FALSE, expand = FALSE, k = 2, try = 50)

# Create PCoA ordination (with optional agruments)
supportR::ordination(mod = pcoa_mod, grps = comp_v02$site, 
    lty = 2, col = "black", x = "topright")

# Create NMS ordination
supportR::ordination(mod = nms_mod, grps = comp_v02$site, 
    lty = 2, col = "black", x = "topright")

# End ----
