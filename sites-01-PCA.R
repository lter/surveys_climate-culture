### clear your environment
rm(list = ls())
### Restart R session to remove loaded packages
.rs.restartR()

# Load required packages
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(readxl)

# Read in site characteristics survey dataset
d <- read_excel("2026 LTER Site Characteristics.xlsx") 
d <- d[-18,]                     #removing Andrew Brooks (18) in favor of Deron Burkepile (20)

# Rename columns using number order for ease of use in analysis
d <- d %>%
  rename(near_road = 6,
         emergency = 7,
         shopping = 8,
         cold = 14,
         heat = 15,
         dehydration = 16,
         storms = 17,
         altitude = 18,
         fire = 19,
         water = 20,
         slips = 21,
         scuffs = 22,
         p.plants = 23,
         v.snakes = 24,
         insects = 25,
         wildlife = 26,
         pesticide = 27,
         exhaustion = 28,
         isolation = 29,
         ppl.offsite = 30,
         ppl.onsite = 31,
         other = 32,
         at.home = 37,
         institutions = 44,
         exec.size = 50,
         exec.inst = 52)

# Convert emergency response time responses to numeric scale
d <- d %>%
  mutate(emergency = recode_values(
    emergency,
    "Less than 20 minutes"~0.3, 
    "20 minutes to one hour"~0.67, 
    "1-3 hours"~2,
    "3-6 hours"~4.5, 
    "6-12 hours"~9, 
    "12-24 hours"~18, 
    "Greater than 24 hours"~36,
    default = NA))

# Convert time to grocery store responses to numeric scale
d <- d %>%
  mutate(shopping = recode_values(
    shopping,
    "Less than 20 minutes"~0.3, 
    "20 minutes to one hour"~0.67, 
    "1-3 hours"~2,
    "It is not possible for deployed individuals to travel to a grocery store."~NA,
    default = NA))

# Convert risk assessment responses to numeric scale
d <- d %>%
  mutate(across(13:31, ~ recode_values(
    .x,
    "Low"~1, 
    "Medium"~2, 
    "High"~3,
    default = NA)))

# Convert percent of personnel that live at home to numeric values
d <- d %>%
  mutate(at.home = recode_values(
    at.home,
    "0-20%"~0.1, 
    "20-40%"~0.3, 
    "40-60%"~0.5,
    "60-80%"~0.7,
    "80-100%"~0.9,
    "Not available or not used"~NA,
    default = NA))

# Convert availability provided meals to numeric values
d <- d %>%
  mutate(Food = recode_values(
    Food,
    "No"~0, 
    "Yes, some days"~0.5, 
    "Yes, every day"~1,
    default = NA))


# Extract numeric data for PCA by column number
pca_data <- d %>% select(6:8,14:31,37,42,44,45,50,52) %>% mutate(across(everything(),as.numeric))
#pca_data <- d %>% select(14:31) %>% mutate(across(everything(),as.numeric))       # extract only risk assessments
#pca_data <- d %>% select(6:8,37,42,44,45,50,52) %>% mutate(across(everything(),as.numeric))      # extract all prepared columns other than risk assessments
pca_data <- as.data.frame(pca_data)       # convert to dataframe from tibble
rownames(pca_data) <- d$Abbreviation      # assign site abbreviations to rownames for later display in PCA

# Run PCA with scaling
res.pca <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE)
res.pca$svd$V

# Visualize individual points (locations)
fviz_pca_ind(res.pca,
             geom = "point",
             pointshape = 21,
             fill = "steelblue",
             repel = TRUE)

# Visualize variables (how each variable contributes)
fviz_pca_var(res.pca)

# Biplot (locations + variables)
fviz_pca_biplot(res.pca, repel = TRUE)