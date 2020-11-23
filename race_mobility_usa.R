# Loading packages
library(ipumsr)
library(tidyverse)

# Importing data
ddi <- read_ipums_ddi("ipumsi_00004.xml")
data <- read_ipums_micro(ddi)

# Checking
glimpse(data)


# Check labels in variables
ipums_val_labels(data$RACE)
ipums_val_labels(data$REGIONW)
ipums_val_labels(data$MARST)
ipums_val_labels(data$RACEUS)
ipums_val_labels(data$LABFORCE)
ipums_val_labels(data$OCCISCO)
ipums_val_labels(data$URBAN)
ipums_val_labels(data$REGIONH)


# Convert the labels to factors (and drop the unused levels)
data <- data %>%
  mutate(marital_factor = as_factor(lbl_clean(MARST)),
         nativity_factor = as_factor(lbl_clean(NATIVITY)),
         birth_cty_factor = as_factor(lbl_clean(BPLCOUNTRY)),
         generation_factor = as_factor(lbl_clean(GENERATION)),
         
         sex_factor = as_factor(lbl_clean(SEX)),
         race_factor = as_factor(lbl_clean(RACEUS)),
         
         labforce_factor = as_factor(lbl_clean(LABFORCE)),
         occup_factor = as_factor(lbl_clean(OCCISCO)),
         urban_factor = as_factor(lbl_clean(URBAN)),
         region_factor = as_factor(lbl_clean(REGIONH)),
         race_v2_factor = case_when(as.character(race_factor) %in% c("Native Hawaiian", "Asiatic Hawiian", "Caucasian Hawaiian") ~ "Hawaiian",
                                    as.character(race_factor) %in% c("American Indian/Alaska Native, not specified", "Alaskan Athabaskan") ~ "Native American",
                                    TRUE ~ as.character(race_factor))

  )


#####################
# Simple tables

# Races by census year
round(prop.table(table(data$race_v2_factor, data$YEAR),2),4)*100

# Races by occupation
round(prop.table(table(data$occup_factor, data$race_factor),1),4)*100
round(prop.table(table(data$race_factor, data$labforce_factor),1),4)*100

# Country of Birth by Year of Census
round(prop.table(table(data$birth_cty_factor, data$YEAR),2),4)*100
# Irish: from 3.7 to 1.48%
# Germans: form 3.71 to 2.71
# UK: from 1.84 to 1.35



# Let's keep BLACKS + Migrants from Ireland + Germany + UK
migrants <- data %>% filter(birth_cty_factor %in% c("Ireland", "Germany", "United Kingdom") | race_v2_factor %in% c("Black", "Mulatto (1850-1910)"))
# From 3M to 570k registers














