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

# Assemble useful data/graph
as.data.frame(pcoa_mod$vectors) %>% 
  dplyr::select(Axis.1, Axis.2) %>% 
  dplyr::mutate(site = comp_v02$site) %>% 
  ggplot(., aes(x = Axis.1, y = Axis.2, fill = site)) +
    geom_point(pch = 21, size = 2, alpha = 0.5) +
    labs(x = paste0("PC1 (", round(pcoa_mod$values$Relative_eig[1] * 100, digits = 2), "%)"),
        y = paste0("PC2 (", round(pcoa_mod$values$Relative_eig[2] * 100, digits = 2), "%)")) +
    scale_fill_manual(values = c("#780000", "#c1121f", "#003049", "#669bbc",
    "#ffb703", "#fb8500", "#606c38", "#283618",
    "#fff3b0", "#fb5607", "#ff006e", "#8338ec",
    "#3a86ff", "#7f4f24", "#c2c5aa", "#a7c957",
    "#6a994e", "#386641", "#caf0f8", "#00b4d8",
    "#e9ff70", "#ff9770", "#ffd670", "#70d6ff",
    "#ffb3c1", "#fff", "#000")) +
    supportR::theme_lyon()

ggsave(file.path("graphs", "composite-ordination_2026-05-12_pcoa.png"), height = 7, width = 7, units = "in")

# End ----
