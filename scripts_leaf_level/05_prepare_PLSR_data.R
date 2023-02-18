### Prepare PLSR data

## Install all packages and load libraries----
library(tidyverse)

## Set working directory----
setwd("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Leaf Spectra")

## Import csv files (spectra + traits)---
spectra_data <- read.csv("spectra_cleaned_large.csv", header = T, sep = ",", check.names = F)
all_traits <- read.csv("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Preprocessing/all_traits.csv",
                       header = T,
                       sep = ",") %>% #not in working directory, write complete extension.
  dplyr::filter(sample_id != "138613648" & sample_id != "138616196" & sample_id != "139021905") #Remove these 3 samples. No spectra (traits only).
length(unique(all_traits$sample_id)) #to count the number of unique plant_id, should be 167.

## Merge spectra df and traits df----
plsr_data <- list(all_traits, spectra_data) %>% #put all data frames into list
  reduce(full_join, by = "sample_id") %>%  #merge all data frames together
  dplyr::select(- "specie.x") %>% #remove specie.x column because it's in duplicate
  relocate(specie.y, .after = sample_id) %>% #relocate column specie.y
  rename(specie = specie.y) #rename column specie

# Save the data (csv file)
write.csv(x = plsr_data, file = "plsr_data.csv", row.names = F)
