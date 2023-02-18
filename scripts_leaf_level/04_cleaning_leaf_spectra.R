### Cleaning leaf-level spectra----

## Install all packages and load libraries----
install.packages(c("spectrolab", "devtools", "signal"))
library(tidyverse)
library(spectrolab)
library(devtools)
library(signal) #sg filter function

## Set working directory----
setwd("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Leaf Spectra")

## Import csv file (spectra)---
leaf_level_spectra <- read.csv("leaf_level_spectra.csv", header = T, sep = ",", check.names = F)

## Cleaning spectra----
# Remove duplicated wavelenths
test <- leaf_level_spectra[leaf_level_spectra$sample_id=="138423052",] #select one sample
matplot(test$wavelength,test$R_average, type="l") #plot

all_wvls <- test$wavelength #select all wavelengths
rem <- all_wvls[which(duplicated(all_wvls))] #select wvls that are duplicated
rem #see duplicated wvls

# Remove those wavelengths 
detach("package:signal", unload = TRUE) # make sure signal package is detached

spectra_no_ovrlp <- leaf_level_spectra %>%
  filter(!wavelength %in% rem) #remove wvls that are stored in object rem (duplicated wvls)

test2 <- spectra_no_ovrlp[spectra_no_ovrlp$sample_id=="138423052",] #select one sample
matplot(test2$wavelength,test2$R_average, pch=1, cex=0.5) #plot to see if wvls removed

# Linear interpolation to 1 nm
inter_wvl <- ceiling(min(spectra_no_ovrlp$wavelength)):floor(max(spectra_no_ovrlp$wavelength))

interpolate <- function(x) {
  wvl <- x$wavelength
  valx <- x$R_average
  new_val <- approx(wvl, valx, xout = inter_wvl)$y
  tmp <- data_frame(wvl = inter_wvl, val = new_val)
  return(tmp)
}

spectra_linear <- spectra_no_ovrlp %>% 
  group_by(sample_id) %>% 
  do(interpolate(.))

test3 <- spectra_linear[spectra_linear$sample_id=="138423052",] #select one sample
matplot(test3$wvl,test3$val, pch=1, cex=0.3) #plot to see if linear interpolation worked

length(unique(spectra_linear$sample_id)) #supposed to be 168 unique plants_id

# Apply Savitzy-Golay filter, order 3 to every spectrum
library(signal)

sg_filter <- function(x, p, n) {
  x$value_sg <- sgolayfilt(x$val, p, n)
  return(x)  
}

sg_data_VIS <- spectra_linear %>% #for visible
  dplyr::filter(wvl <= 715) %>% #select wvls that are less or equal than 715
  group_by(sample_id) %>% 
  do(sg_filter(., p = 3, n = 21))

sg_data_NIR <- spectra_linear %>% #for near-infrared
  dplyr::filter(wvl > 715,
                wvl <= 1390 ) %>% #select wvls from 716 to 1390
  group_by(sample_id) %>% 
  do(sg_filter(., p = 3, n = 35))

sg_data_SWIR1 <- spectra_linear %>% #for SWIR1
  dplyr::filter(wvl > 1390,
                wvl <= 1880) %>% #select wvls from 1391 to 1880
  group_by(sample_id) %>% 
  do(sg_filter(., p = 3, n = 75))

sg_data_SWIR2 <- spectra_linear %>% #for SWIR2
  dplyr::filter(wvl > 1880) %>% #select wvls that are more than 1880
  group_by(sample_id) %>% 
  do(sg_filter(., p = 5, n = 175)) #greater p is more aggressive smoothing

sg_data <- bind_rows(sg_data_VIS, sg_data_NIR,
                     sg_data_SWIR1,
                     sg_data_SWIR2)

# Transpose to wide format
sg_wide <- sg_data %>% 
  select (-val) %>%
  spread(wvl, value_sg)

# Combine with metadata species
sample_id <- leaf_level_spectra %>% #create a df sample_id with species associated
  select(sample_id, specie) #select only these columns
sample_id <- sample_id[!duplicated(sample_id), ] #remove duplicated species name to have only 168 samples

sg_wide_specie <- sg_wide %>% left_join(sample_id, by = "sample_id") %>%
  relocate(specie, .after = sample_id) #to change column order

## Mean spectra for 139284436 & 139293925 (same tree, Populus)----
mean_populus <- sg_wide_specie %>%
  dplyr::filter(sample_id %in% c("139284436", "139293925")) %>% #Select both samples
  dplyr::select(-1,-2) %>% #remove column that are numerical
  colMeans() %>% #to have the mean of every column (every wavelength)
  as.data.frame.list() %>% #change to dataframe
  mutate(sample_id = 139293925, specie = "Populus tremuloides Michaux")
colnames(mean_populus) <- gsub("X", "", colnames(mean_populus)) #remove X before wave number

# Add mean_populus (plant 139293925) to sg_wide_specie
df_large <- sg_wide_specie %>%
  dplyr::filter(sample_id != "139284436" & sample_id != "139293925") %>% #remove both samples
  rbind(mean_populus) #add row with mean of both samples

duplicated(df_large$sample_id) #Check if there is duplicates in column plant_id
length(unique(df_large$sample_id)) #check if there is 167 samples

# Convert to spectra object
spectra_sg <- as_spectra(df_large,name_idx = 1, meta_idxs = 2)
plot_interactive(spectra_sg)
summary(spectra_sg) #check number of samples

# Trim wavelengths 400-2400
spectra_sg_trim <- spectra_sg[, bands(spectra_sg) %in% 400:2400]

# Plotting
plot(spectra_sg_trim,
     main="Leaf-level reflectance spectra of 167 samples",
     xlab = "Wavelength (nm)",
     ylab = "reflectance",
     cex.main = 1,
     cex.lab = 0.70) #see result in plot all black

# Convert spectra object to a dataframe (wide format)
spectra_clean_large <- as.data.frame(spectra_sg_trim, fix_names = "none", metadata = TRUE) %>%
  dplyr::rename(sample_id = sample_name) %>%
  dplyr::mutate(sample_id = gsub("spec_", "", sample_id))
write.csv(x = spectra_clean_large, file = "spectra_cleaned_large.csv", row.names = F) #save the data (csv file)

# Long format
spectra_clean_long <- pivot_longer(spectra_clean_large, cols = - c(sample_id, specie), names_to = "wavelength", values_to = "val")
spectra_clean_long$wavelength <- as.numeric(spectra_clean_long$wavelength) #change wvl to numeric
write.csv(x = spectra_clean_long, file = "spectra_cleaned_long.csv", row.names = F) #save the data (csv file)

## Plotting spectra ----
# Visualize spectra (with species in different colors)
plot <- ggplot(spectra_clean_long,
                aes(x = wavelength, y = val, group = sample_id, color = specie)) +
  geom_line() +
  ggtitle("Leaf-level reflectance spectra of 167 samples") +
  xlab("Wavelength (nm)") +
  ylab("Reflectance") +
  theme(legend.position="right",
        legend.key.height = unit(0.2, 'cm'), #change legend key height
        legend.key.width = unit(0.2, 'cm')) #change legend key width
plot

# Save plot as a pdf
ggsave("leaf_spectra_clean_species.pdf",
       plot,
       height = 6, width = 12,
       units = "in")

## Brightness normalization----
spectra <- read.csv("spectra_cleaned_large.csv", check.names = F)
spec <- as_spectra(spectra, name_idx = 1, meta_idxs = 2)
spec_BN <- normalize(spec)
plot(spec_BN, main = "Brightness normalized spectra", xlab = "Wavelength (nm)", ylab = "Reflectance")
data_BN <- as.data.frame(spec_BN)
write.csv(data_BN, "spectra_cleaned_BN.csv", row.names = F)
