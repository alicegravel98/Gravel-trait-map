### Verification of trait data

## Install all packages and load libraries----
install.packages("sf")
library(tidyverse)
library(sf)

## Change working directory----
setwd("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Preprocessing")

## Data importation----
SLA <- read_csv("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/SLA_data.csv", col_names = TRUE)
Cfractions <- read_csv("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Carbon Fractions/Cfractions_data.csv", col_names = TRUE)
pigments <- read_csv("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/pigments/pigments_data.csv", col_names = TRUE)
CN <- read_csv("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/CN/CN_data.csv", col_names = TRUE)

## Polygons importation----
tree_data <- read_sf("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/GIS data/2022-POLYGON-DATA/tree_data.shp")

## Create labels----
labels <- list ("Larix laricina" = "LALA",
                "Betula alleghaniensis" = "BEAL",
                "Thuja occidentalis" = "THOC",
                "Populus grandidentata" = "POGR",
                "Acer rubrum" = "ACRU",
                "Acer pensylvanicum" = "ACPE",
                "Abies balsamea" = "ABBA",
                "Pinus strobus" = "PIST",
                "Acer saccharum" = "ACSA",
                "Picea mariana" = "PIMA",
                "Picea sp." = "Picea",
                "Fagus grandifolia"= "FAGR",
                "Tsuga canadensis" = "TSCA",
                "Betula papyrifera" = "BEPA",
                "Populus tremuloides" = "POTR",
                "Picea rubens" = "PIRU",
                "Quercus rubra" = "QURU",
                "Ostrya virginiana" = "OSVI")

SLA$label <- sapply(SLA$specie, function(x) labels[x])
SLA$label <- unlist(SLA$label)
SLA$label <- as.factor(SLA$label)
summary(SLA$label)

Cfractions$label <- sapply(Cfractions$specie, function(x) labels[x])
Cfractions$label <- unlist(Cfractions$label)
Cfractions$label <- as.factor(Cfractions$label)
summary(Cfractions$label)

pigments$label <- sapply(pigments$specie, function(x) labels[x])
pigments$label <- unlist(pigments$label)
pigments$label <- as.factor(pigments$label)
summary(pigments$label)

CN$label <- sapply(CN$specie, function(x) labels[x])
CN$label <- unlist(CN$label)
CN$label <- as.factor(CN$label)
summary(CN$label)

## Verification SLA----
# Specific leaf area
plot_SLA <- ggplot(data = SLA,
                   aes(x = label, y = specific_leaf_area_m2_kg, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(), #remove labels of x axis
        axis.text.x = element_blank())
plot_SLA

out <- boxplot.stats(SLA$specific_leaf_area_m2_kg)$out #check outliers. None.

# Leaf mass per area
plot_LMA <- ggplot(data = SLA,
                   aes(x = label, y = leaf_mass_per_area_g_m2, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_LMA

out <- boxplot.stats(SLA$leaf_mass_per_area_g_m2)$out #check outliers
out_ind <- which(SLA$leaf_mass_per_area_g_m2 %in% c(out)) #identify rows
out_ind #check result 
# Picea! More xeric, matches with hypothesis that LMA is higher in that sort of drainage. They are in the range of CABO data.

# Leaf dry matter content
plot_LDMC <- ggplot(data = SLA,
                    aes(x = label, y = leaf_dry_matter_content_mg_g, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_LDMC

out <- boxplot.stats(SLA$leaf_dry_matter_content_mg_g)$out #check outliers
out_ind <- which(SLA$leaf_dry_matter_content_mg_g %in% c(out)) #identify rows
out_ind #check result
# Acer pensylvanicum! Falls within range of CABO data

# Actual Leaf dry matter content
plot_aLDMC <- ggplot(data = SLA,
                     aes(x = label, y = actual_leaf_dry_matter_content_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_aLDMC

out <- boxplot.stats(SLA$actual_leaf_dry_matter_content_perc)$out #check outliers
out_ind <- which(SLA$actual_leaf_dry_matter_content_perc %in% c(out)) #identify rows
out_ind #check result
# Acer pensylvanicum!

# Leaf water content
plot_LWC <- ggplot(data = SLA,
                   aes(x = label, y = leaf_water_content_mg_g, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_LWC

out <- boxplot.stats(SLA$leaf_water_content_mg_g)$out #check outliers
out_ind <- which(SLA$leaf_water_content_mg_g %in% c(out)) #identify rows
out_ind #check result
# Acer pensylvanicum!

# Leaf relative water content
plot_LrWC <- ggplot(data = SLA,
                    aes(x = label, y = leaf_relative_water_content_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_LrWC

out <- boxplot.stats(SLA$leaf_relative_water_content_perc)$out #check outliers
out_ind <- which(SLA$leaf_relative_water_content_perc %in% c(out)) #identify rows
out_ind #check result
# Tsuga canadensis, Abies balsamea, Fagus grandifolia et Larix laricina.
# Falls within range of CABO data

# Equivalent water thickness
plot_EWT <- ggplot(data = SLA,
                   aes(x = label, y = equivalent_water_thickness_cm, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_EWT

out <- boxplot.stats(SLA$equivalent_water_thickness_cm)$out #check outliers. None.

## Verification Cfractions----
# Hemicellulose
plot_hemicell <- ggplot(Cfractions,
                   aes(x = label, y = hemicellulose_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_hemicell

out <- boxplot.stats(Cfractions$hemicellulose_perc)$out #check outliers.
out_ind <- which(Cfractions$hemicellulose_perc %in% c(out)) #identify rows
out_ind #check result
# Betula papyrifera, more humid, matches with hypothesis that hemicell is higher in that sort of drainage.
# Quercus rubra. They are in the range of CABO data.

# Cellulose
plot_cell <- ggplot(Cfractions,
                        aes(x = label, y = cellulose_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_cell

out <- boxplot.stats(Cfractions$cellulose_perc)$out #check outliers.
out_ind <- which(Cfractions$cellulose_perc %in% c(out)) #identify rows
out_ind #check result
# Picea! Falls within range of CABO data.

# Lignin
plot_lignin <- ggplot(Cfractions,
                    aes(x = label, y = lignin_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_lignin

out <- boxplot.stats(Cfractions$lignin_perc)$out #check outliers.
out_ind <- which(Cfractions$lignin_perc %in% c(out)) #identify rows
out_ind #check result
# Populus grandidentata ! Falls within range of CABO data.

## Verification pigments----
# Chlorophyll a
plot_chla <- ggplot(data = pigments,
                 aes(x = label, y = chla_mg_g_disk_mass, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_chla

out <- boxplot.stats(pigments$chla_mg_g_disk_mass)$out #check outliers. None.

# Chlorophyll b
plot_chlb <- ggplot(data = pigments,
                    aes(x = label, y = chlb_mg_g_disk_mass, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_chlb

out <- boxplot.stats(pigments$chlb_mg_g_disk_mass)$out #check outliers. None.

# Carotenoids
plot_carot <- ggplot(data = pigments,
                    aes(x = label, y = carot_mg_g_disk_mass, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_carot

out <- boxplot.stats(pigments$carot_mg_g_disk_mass)$out #check outliers.
out_ind <- which(pigments$carot_mg_g_disk_mass %in% c(out)) #identify rows
out_ind #check result
# Populus grandidentata. Falls within range of CABO data.

# Chla/Chlb ratio
plot_ratio <- ggplot(data = pigments,
                     aes(x = label, y = chl_a_chl_b_ratio, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_ratio

out <- boxplot.stats(pigments$chl_a_chl_b_ratio)$out #check outliers. None.

## Verification Carbone----
plot_C <- ggplot(data = CN,
                 aes(x = label, y = c_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_C

out <- boxplot.stats(CN$c_perc)$out #verify outliers
out_ind <- which(CN$c_perc %in% c(out)) #identify rows
out_ind
# Acer saccharum! Falls within range of CABO data.

## Verification Nitrogen----
plot_N <- ggplot(data = CN,
                 aes(x = label, y = n_perc, fill = label)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
plot_N

out <- boxplot.stats(CN$n_perc)$out #verify outliers. None.
