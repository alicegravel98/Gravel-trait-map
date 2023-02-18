### Preprocessing leaf-level spectra

## Install all packages and load libraries----
library(tidyverse)
library(stringr) #str_extract()

## Explore the spectral dataset----
spectra_path <- "/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Leaf Spectra"
setwd(spectra_path) #set the absolute filepath for spectral dataset
spectral_data_raw <- read.csv("project_all_combined.csv", header = T, sep = ",") #import csv file
names(spectral_data_raw) #see column's names
plot(x = spectral_data_raw$wavelength, y = spectral_data_raw$R_T_Average)
plot

## Cleaning dataframe----
spectra_data <- spectral_data_raw %>%
  select(sample_id,
         specie = scientific_name,
         reflectance_transmittance,
         wavelength,
         R_T_Average) %>% 
  filter(reflectance_transmittance == "reflectance")
spectra_data #verify result 

length(unique(spectra_data$sample_id)) #count the number of unique plant_id = 168! GREAT!

## Remove bad spectra from leaf 3 (138769323 - Abies balsamea)----
data_leaves <- read.csv("project_leaves_combined.csv", header = T, sep = ",") %>%
  select(sample_id, 
         leaf_number,
         reflectance.transmittance,
         wavelength,
         calculated_value)

length(unique(data_leaves$sample_id)) #count the number of unique plant_id

# Select only data from plant 138769323 and remove leaf 3
spectra_abies <- filter(data_leaves,
                        sample_id == 138769323,
                        leaf_number != 3,
                        reflectance.transmittance == "reflectance")

# Calculate mean reflectance of leaves for each wavelength
mean_spectra <- aggregate(spectra_abies$calculated_value, by=list(spectra_abies$wavelength), mean) %>%
  rename(wavelength = 1, R_T_Average = 2) %>% #rename columns
  mutate(sample_id = 138769323, specie = "Abies balsamea (Linnaeus) Miller") #unique(spectra_data$specie) to see species names
head(mean_spectra) #verify result

# Bind spectra of Abies with the rest
leave_spectra <- spectra_data %>%
  filter(sample_id != 138769323) %>% #remove bad sample and length(unique(dataframename$plant_id)) to see if 167 samples
  select(-3) %>% #remove column 3 for the bind
  rbind(mean_spectra) %>%
  rename(R_average = R_T_Average) %>%
  arrange(sample_id)

length(unique(leave_spectra$sample_id)) #check if there is still 168 samples
head(leave_spectra) #check result

# Simplify species names
leave_spectra$specie <- 
  str_extract(leave_spectra$specie, "(\\w+\\s\\w+)") #replace species names with only first two words
unique(leave_spectra$specie) #check result

PiceaA <- which(leave_spectra$specie == "Picea A") #find index of rows that contains Picea A in column specie
leave_spectra$specie[PiceaA] <- 
  gsub("Picea A", "Picea sp.", leave_spectra$specie[PiceaA]) #replace Picea A by Picea sp. for those rows
unique(leave_spectra$specie) #check result

## Export dataframe (csv file)----
write.csv(leave_spectra,"leaf_level_spectra.csv", row.names = F)

## Visualise all spectra----
spectra_visualisation <- ggplot(data = leave_spectra,
       aes(x = wavelength,
           y = R_average,
           group = sample_id,
           color = sample_id)) + 
  geom_line() +
  ggtitle("Leaf-level reflectance spectra of 168 samples") +
  xlab("Wavelength (nm)") +
  ylab("Reflectance (%)") +
  scale_y_continuous(limits = c(0, 1))

spectra_visualisation #check plot

# Save plot as a pdf
ggsave("spectra_visualisation.pdf",
       spectra_visualisation,
       height = 6, width = 12,
       units = "in")
