### Trait data preprocessing

## Install all packages and load libraries----
install.packages(c("tidyverse", "stringr"))
library(tidyverse)
library(stringr) #str_extract()

## Change working directory----
setwd("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Preprocessing")
getwd()

## Data importation from FULCRUM files----
# When downloading data from fulcrum, add “sample_id” for SLA, “bottle_id” for Cfractions, “vial_id” for pigments
plants_raw <- read_csv("plants.csv")
bulk_raw <- read_csv("bulk_leaf_samples.csv")
SLA_raw_data <- read_csv("leaf_area_and_water_samples.csv")
Cfractions_raw_data <- read_csv("carbon_fractions_bags.csv")
pigments_raw_data <- read_csv("pigments_extracts.csv")
CN_raw_data <- read_csv("cn_leaf_concentrations.csv")


## Cleaning data from FULCRUM----
# Plant file
# Use the plant records to be sure it's the right specie

plants_specie <- plants_raw %>%
  select(record_id = "_record_id", #to join later with bulk leaf samples
         plant_id,
         specie = "vascan_taxon") #to have all species
plants_specie #verify result

bulk <- bulk_raw %>%
  select(sample_id, #to have bulk id
         plant) %>% #to join later with plants
  filter(sample_id != 139284436) #remove plant that is same tree (Populus)
bulk #verify result

plants <- right_join(plants_specie, bulk, by = c("record_id" = "plant" )) %>%
  select(-"record_id") %>% #remove record_id
  arrange(sample_id) %>% #ascending order
  relocate(plant_id, sample_id, .before = specie) #to change column order
plants #verify result

# Simplify species names
plants$specie <- 
  str_extract(plants$specie, "(\\w+\\s\\w+)") #replace species names with only first two words
unique(plants$specie) #check result

PiceaA <- which(plants$specie == "Picea A") #find index of rows that contains Picea A in column specie
plants$specie[PiceaA] <- 
  gsub("Picea A", "Picea sp.", plants$specie[PiceaA]) #replace Picea A by Picea sp. for those rows
unique(plants$specie) #check result

write.csv(plants,"plants_and_samples_ids.csv", row.names = F) #save data
# plants <- read_csv("plants_and_samples_ids.csv") #to reimport it

# Leaf area and water samples file
SLA_data <- SLA_raw_data %>%
  select(sample_id,
         specific_leaf_area_m2_kg,         
         leaf_mass_per_area_g_m2,
         leaf_dry_matter_content_mg_g,
         actual_leaf_dry_matter_content_perc,
         leaf_water_content_mg_g,
         leaf_relative_water_content_perc,
         equivalent_water_thickness_cm)
SLA_data #verify result   

# Carbon fractions file
Cfractions_data <- Cfractions_raw_data %>%
  select(sample_type,
         sample_id = bottle_id, #change the name of the variable bottle_id for sample_id
         soluble_perc,
         hemicellulose_perc,
         cellulose_perc,
         lignin_perc,
         recalcitrants_perc,
         sum_fractions_perc,
         quality_flag_bag) %>%
  filter(sample_type == "sample", quality_flag_bag == "good") %>% #remove blanks and bad samples 
  select(-c("sample_type","quality_flag_bag")) %>% #remove these columns
  arrange(sample_id) #ascending order
Cfractions_data #verify result

# Pigments extracts file
pigments_data <- pigments_raw_data %>%
  select(sample_code,
         sample_id = vial_id, #change the name of the variable vial_id for plant_id
         chla_mg_g_disk_mass,
         chlb_mg_g_disk_mass,
         carot_mg_g_disk_mass,
         chl_a_chl_b_ratio, 
         quality_flag_extract) %>%
  filter(sample_code != c("S1", "S2"), quality_flag_extract == "good") %>% #remove S1 and S2 (not samples) and keep only good extracts
  select(- c("sample_code", "quality_flag_extract")) %>% #remove these columns
  arrange(sample_id) #ascending order
pigments_data #verify result 

# C/N file
CN_data <- CN_raw_data %>%
  select(sample_id,
         c_perc,
         n_perc) %>%
  arrange(sample_id)
CN_data #verify result

# mean C/N for 138769323 (measured twice)
mean_138769323 <- CN_data %>%
  filter(sample_id == "138769323") %>% #Select rows containing the sample
  colMeans() %>% #to have the mean of every column (every trait)
  as.data.frame.list() %>% #change to dataframe
  mutate(sample_id = 138769323)
mean_138769323 #check result

CN_data_good <- CN_data %>%
  filter(sample_id != "138769323") %>% #remove rows containing the sample
  rbind(mean_138769323) #add row with mean of sample
duplicated(CN_data_good$sample_id) #Check if there is duplicates in column plant_id

## Verification of primary keys choice----
## When the result is ? 0 lines ? : means that each value comes once, good choice of primary key
# primary key: plants
plants %>% 
  count(sample_id) %>% 
  filter(n > 1)

# primary key: SLA
SLA_data %>% 
  count(sample_id) %>% 
  filter(n > 1)

# primary key: Cfractions
Cfractions_data %>% 
  count(sample_id) %>% 
  filter(n > 1)

# primary key: pigments
pigments_data %>% 
  count(sample_id) %>% 
  filter(n > 1)

# primary key: C/N
CN_data_good %>% 
  count(sample_id) %>% 
  filter(n > 1)

## Mean trait for 139284436 & 139293925 (same tree, Populus)----
# Mean SLA
mean_populus <- SLA_data %>%
  filter(sample_id %in% c("139284436", "139293925")) %>% #select both samples
  colMeans() %>% #to have the mean of every column (every trait)
  as.data.frame.list() %>% #change to dataframe
  mutate(sample_id = 139293925)
mean_populus #check result

SLA <- SLA_data %>%
  filter(sample_id != "139284436" & sample_id != "139293925") %>% #remove both samples
  rbind(mean_populus) #add row with mean of both samples
duplicated(SLA$sample_id) #Check if there is duplicates in column sample_id

# Mean Cfractions
mean_populus2 <- Cfractions_data %>%
  filter(sample_id %in% c("139284436", "139293925")) %>% #select both samples
  colMeans() %>% #to have the mean of every column (every trait)
  as.data.frame.list() %>% #change to dataframe
  mutate(sample_id = 139293925)
mean_populus2 #check result

Cfractions <- Cfractions_data %>%
  filter(sample_id != "139284436" & sample_id != "139293925") %>% #remove both samples
  rbind(mean_populus2) #add row with mean of both samples
duplicated(Cfractions$sample_id) #Check if there is duplicates in column sample_id

# Mean pigments
mean_populus3 <- pigments_data %>%
  filter(sample_id %in% c("139284436", "139293925")) %>% #select both samples
  colMeans() %>% #to have the mean of every column (every trait)
  as.data.frame.list() %>% #change to dataframe
  mutate(sample_id = 139293925)
mean_populus3 #check result

pigments <- pigments_data %>%
  filter(sample_id != "139284436" & sample_id != "139293925") %>% #remove both samples
  rbind(mean_populus3) #add row with mean of both samples
duplicated(pigments$sample_id) #Check if there is duplicates in column sample_id

# Mean C/N
mean_populus4 <- CN_data_good %>%
  filter(sample_id %in% c("139284436", "139293925")) %>% #select both samples
  colMeans() %>% #to have the mean of every column (every trait)
  as.data.frame.list() %>% #change to dataframe
  mutate(sample_id = 139293925)
mean_populus4 #check result

CN <- CN_data_good %>%
  filter(sample_id != "139284436" & sample_id != "139293925") %>% #remove both samples
  rbind(mean_populus4) #add row with mean of both samples
duplicated(CN$sample_id) #Check if there is duplicates in column sample_id


## Join tables----
# Join tables
SLA <- right_join(plants, SLA, by = "sample_id") 
SLA #check result

Cfractions <- right_join(plants, Cfractions, by = "sample_id")
Cfractions #check result

pigments <- right_join(plants, pigments, by = "sample_id")
pigments #check result

CN <- right_join(plants, CN, by = "sample_id")
CN #check result

## Export dataframes (csv files)----
write.csv(SLA,"SLA_data.csv", row.names = F)
write.csv(Cfractions,"Cfractions_data.csv", row.names = F)
write.csv(pigments,"pigments_data.csv", row.names = F)
write.csv(CN,"CN_data.csv", row.names = F)

## Merge all dataframes----
##### ADD PHENOLS AND NUTRIENTS ####
df_list <- list(SLA, Cfractions, pigments, CN) %>% #put all data frames into list
  reduce(full_join, by = c("plant_id", "sample_id", "specie")) %>%  #merge all data frames together
  relocate(specie, .after = sample_id) #relocate column specie

## Export dataframe (csv file)----
write.csv(df_list,"all_traits.csv", row.names = F)
