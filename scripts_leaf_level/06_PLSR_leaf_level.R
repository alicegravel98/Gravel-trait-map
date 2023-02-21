## PLSR model fitting and testing

## Install all packages and load libraries----
install.packages(c("pls", "gridExtra"))
remotes::install_github("TESTgroup-BNL/spectratrait")
library(tidyverse)
library(spectratrait)
library(pls)
library(gridExtra)

## Set working directory----
outdir <- setwd("/Users/alicegravel/Desktop/DossieR/Gravel-trait-map/output_plsr_leaf_level")

# Import PLSR data----
plsr_data <- read.csv("/Users/alicegravel/Documents/École/MAÎTRISE/DATA/Leaf Spectra/plsr_data.csv", header = T, sep = ",", check.names = F)

## Set up data----
# Define target variable
inVar <- "SLA"

Start.wave <- 400
End.wave <- 2400
wv <- seq(Start.wave, End.wave, 1) #define wavelengths
Spectra <- as.matrix(plsr_data[, names(plsr_data) %in% wv]) #store spectra in a matrix
colnames(Spectra) <- c(paste0("Wave_", wv)) #change the names of columns
head(Spectra)[1:6, 1:10] #check result (only six rows and ten columns)

# Define metadata
sample_info <- plsr_data[, names(plsr_data) %notin% seq(400, 2400, 1)]
head(sample_info)

sample_info2 <- sample_info %>%
  dplyr::select(1:4) %>% #metadata and trait of interest
  rename("SLA" = "specific_leaf_area_m2_kg") #rename trait
head(sample_info2)

# Join spectra data and metadata
plsr_data <- data.frame(sample_info2, Spectra)

## Create cal/val datasets----
split_data <- spectratrait::create_data_split(dataset = plsr_data, approach = "dplyr", split_seed = 2356812, 
                                              prop = 0.7, group_variables = c("specie"))

### Choose a splitseed (random seed to use for splitting data) so that anyone else ###
### can recreate the exact same training and test sets by using the same seed. ###

names(split_data)

cal.plsr.data <- split_data$cal_data
head(cal.plsr.data)[1:8]

val.plsr.data <- split_data$val_data
head(val.plsr.data)[1:8]

## Format PLSR data for model fitting----
# Calibration dataset
cal_spec <- as.matrix(cal.plsr.data[, which(names(cal.plsr.data) %in% paste0("Wave_", wv))])
cal.plsr.data <- data.frame(cal.plsr.data[, which(names(cal.plsr.data) %notin% paste0("Wave_", wv))],
                            Spectra = I(cal_spec))
head(cal.plsr.data)[1:5]

# Validation dataset
val_spec <- as.matrix(val.plsr.data[, which(names(val.plsr.data) %in% paste0("Wave_", wv))])
val.plsr.data <- data.frame(val.plsr.data[, which(names(val.plsr.data) %notin% paste0("Wave_", wv))],
                            Spectra = I(val_spec))
head(val.plsr.data)[1:5]

# Check number of observations for each dataset
print(paste("Cal observations: ", dim(cal.plsr.data)[1], sep = ""))
print(paste("Val observations: ", dim(val.plsr.data)[1], sep = ""))

# Save data as csv
write.csv(cal.plsr.data, file = file.path(outdir, paste0(inVar, '_Cal_PLSR_Dataset.csv')),
          row.names = F)
write.csv(val.plsr.data, file = file.path(outdir, paste0(inVar, '_Val_PLSR_Dataset.csv')),
          row.names = F)

## Use permutation to determine the optimal number of components----
pdf(paste0(inVar,"_PLSR_Component_Selection.pdf")) #open a pdf device and choose file name

ncomps <- find_optimal_components(
  dataset = cal.plsr.data,
  targetVariable = inVar,
  method = "firstMin",
  maxComps = 20,
  iterations = 20,
  seg = 100,
  prop = 0.7,
  random_seed = 2356812
)
ncomps #show result

dev.off() #close the pdf device to save plot

## Fit final model----
segs <- 100 #define number of segments for cross-validation

plsr.out <- plsr(as.formula(paste(inVar, "~", "Spectra")), scale = F, ncomp = ncomps, validation = "CV", #If validation = "CV", cross-validation is performed.
                 segments = segs, segment.type = "random", trace = F, data = cal.plsr.data, method = "oscorespls")

fit <- plsr.out$fitted.values[, 1, ncomps] #store predicted values in R object

pls.options(parallel = NULL) #specify how the cross-validation (CV) should be performed. If NULL, the CV is done serially.

## External validation fit stats----
pdf(paste0(inVar,"_Validation_RMSEP_R2_by_Component.pdf")) #open a pdf device

par(mfrow = c(1, 2))

# Function to estimate the root mean squared error of prediction (RMSEP)
pls::RMSEP(plsr.out, newdata = val.plsr.data)

# Plot result
plot(pls::RMSEP(plsr.out, estimate = c("test"), newdata = val.plsr.data), main = "MODEL RMSEP",
     xlab = "Number of Components", ylab = "Model Validation RMSEP", lty = 1 ,col = "black", cex = 1.5, lwd = 2)
box(lwd = 2.2) #box around plot with line width 2.2

# Function to estimate the coefficient of multiple determination (R2)
pls::R2(plsr.out, newdata = val.plsr.data)

# Plot result
plot(pls::R2(plsr.out, estimate = c("test"), newdata = val.plsr.data), main = "MODEL R2",
     xlab = "Number of Components", ylab = "Model Validation R2", lty = 1, col = "black", cex = 1.5, lwd = 2)
box(lwd = 2.2)

dev.off() #close the pdf device to save plot

## PLSR fit observed vs. predicted plot data----
# Calibration
cal.plsr.output <- data.frame(cal.plsr.data[, which(names(cal.plsr.data) %notin% "Spectra")], 
                              PLSR_Predicted = fit,
                              PLSR_CV_Predicted = as.vector(plsr.out$validation$pred[, , ncomps]))
cal.plsr.output <- cal.plsr.output %>%
  mutate(PLSR_CV_Residuals = PLSR_CV_Predicted-get(inVar))
head(cal.plsr.output)

cal.R2 <- round(pls::R2(plsr.out, intercept = F)[[1]][ncomps], 2)
cal.RMSEP <- round(sqrt(mean(cal.plsr.output$PLSR_CV_Residuals^2)), 2)

val.plsr.output <- data.frame(val.plsr.data[, which(names(val.plsr.data) %notin% "Spectra")],
                              PLSR_Predicted = as.vector(predict(plsr.out, 
                                                                 newdata = val.plsr.data, 
                                                                 ncomp = ncomps, type = "response")[, , 1]))
val.plsr.output <- val.plsr.output %>%
  mutate(PLSR_Residuals = PLSR_Predicted-get(inVar))
head(val.plsr.output)

val.R2 <- round(pls::R2(plsr.out, newdata = val.plsr.data,intercept = F)[[1]][ncomps], 2)
val.RMSEP <- round(sqrt(mean(val.plsr.output$PLSR_Residuals^2)), 2)

rng_quant <- quantile(cal.plsr.output[, inVar], probs = c(0.001, 0.999))

cal_scatter_plot <- ggplot(cal.plsr.output,
                           aes(x = PLSR_CV_Predicted, y = get(inVar))) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "dark grey", linetype = "dashed", linewidth = 1) +
  xlim(rng_quant[1], rng_quant[2]) + 
  ylim(rng_quant[1], rng_quant[2]) +
  labs(x = paste0("Predicted ", paste(inVar), " (m2/kg)"),
       y = paste0("Observed ", paste(inVar), " (m2/kg)"),
       title = paste0("Calibration: ", paste0("Rsq = ", cal.R2), "; ", paste0("RMSEP = ", 
                                                                              cal.RMSEP))) +
  theme(plot.title = element_text(size = 8),
        axis.text = element_text(size = 6), legend.position ="none",
        axis.title = element_text(size = 6, face = "bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, linewidth = 1))

cal_scatter_plot #show plot

cal_resid_histogram <- ggplot(cal.plsr.output, aes(x = PLSR_CV_Residuals)) +
  geom_histogram(alpha = .5, position = "identity") + 
  geom_vline(xintercept = 0, color = "black", 
             linetype = "dashed", linewidth = 0.8) +
  theme_bw() + 
  theme(axis.text = element_text(size = 6), legend.position="none",
        axis.title = element_text(size = 6, face = "bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, linewidth = 1))

cal_resid_histogram #show plot

rng_quant <- quantile(val.plsr.output[, inVar], probs = c(0.001, 0.999))

val_scatter_plot <- ggplot(val.plsr.output, aes(x = PLSR_Predicted, y = get(inVar))) + 
  theme_bw() + geom_point() + geom_abline(intercept = 0, slope = 1, color = "dark grey", 
                                          linetype = "dashed", linewidth = 1) + xlim(rng_quant[1], 
                                                                                rng_quant[2]) + 
  ylim(rng_quant[1], rng_quant[2]) +
  labs(x = paste0("Predicted ", paste(inVar), " (m2/kg)"),
       y = paste0("Observed ", paste(inVar), " (m2/kg)"),
       title = paste0("Validation: ", paste0("Rsq = ", val.R2), "; ", paste0("RMSEP = ", 
                                                                             val.RMSEP))) +
  theme(plot.title = element_text(size = 8),
        axis.text = element_text(size = 6), legend.position = "none",
        axis.title = element_text(size = 6, face = "bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, linewidth = 1))

val_scatter_plot #show plot

val_resid_histogram <- ggplot(val.plsr.output, aes(x = PLSR_Residuals)) +
  geom_histogram(alpha = .5, position = "identity") + 
  geom_vline(xintercept = 0, color = "black", 
             linetype = "dashed", linewidth = 0.8) +
  theme_bw() + 
  theme(axis.text = element_text(size = 6), legend.position = "none",
        axis.title = element_text(size = 6, face = "bold"), 
        axis.text.x = element_text(angle = 0,vjust = 0.5),
        panel.border = element_rect(linetype = "solid", fill = NA, linewidth = 1))

val_resid_histogram #show plot

# Plot cal/val side-by-side
pdf(paste0(inVar,"_Cal_Val_Scatterplots.pdf")) #open a pdf device

scatterplots <- grid.arrange(cal_scatter_plot, val_scatter_plot, cal_resid_histogram, 
                             val_resid_histogram, nrow = 2, ncol = 2)

dev.off() #close the pdf device to save plot

## Generate Coefficient and VIP plots----
vips <- spectratrait::VIP(plsr.out)[ncomps,]

# Plot results
pdf(paste0(inVar,"_Coefficient_VIP_plot.pdf")) #open a pdf device

par(mfrow = c(2, 1))

plot(plsr.out$coefficients[, , ncomps], x = wv, xlab = "Wavelength (nm)",
     ylab = "Regression coefficients", lwd = 2, type = 'l', ylim = c(-2, 2))

plot(seq(Start.wave, End.wave,1), vips, xlab = "Wavelength (nm)", ylab = "VIP", cex = 0.01, ylim = c(0, 2))

lines(seq(Start.wave, End.wave, 1), vips, lwd = 3)
abline(h = 0.8, lty = 2, col = "dark grey")

dev.off() #close the pdf device to save plot

## Model uncertainty analysis----
seg <- 100

jk.plsr.out <- pls::plsr(as.formula(paste(inVar, "~", "Spectra")), scale = F, 
                         center = T, ncomp = ncomps, validation = "CV", 
                         segments = seg, segment.type = "random", trace = F, 
                         jackknife = T, data = cal.plsr.data)
pls.options(parallel = NULL)

Jackknife_coef <- spectratrait::f.coef.valid(plsr.out = jk.plsr.out, 
                                             data_plsr = cal.plsr.data, 
                                             ncomp = ncomps, inVar = inVar)
Jackknife_intercept <- Jackknife_coef[1, , , ]
Jackknife_coef <- Jackknife_coef[2:dim(Jackknife_coef)[1], , , ]

interval <- c(0.025, 0.975)

Jackknife_Pred <- val.plsr.data$Spectra %*% Jackknife_coef + 
  matrix(rep(Jackknife_intercept, length(val.plsr.data[,inVar])), byrow = T, 
         ncol = length(Jackknife_intercept))
Interval_Conf <- apply(X = Jackknife_Pred, MARGIN = 1, FUN = quantile, 
                       probs = c(interval[1], interval[2]))

sd_mean <- apply(X = Jackknife_Pred, MARGIN = 1, FUN = sd)
sd_res <- sd(val.plsr.output$PLSR_Residuals)
sd_tot <- sqrt(sd_mean^2 + sd_res^2)
val.plsr.output$LCI <- Interval_Conf[1, ]
val.plsr.output$UCI <- Interval_Conf[2, ]
val.plsr.output$LPI <- val.plsr.output$PLSR_Predicted - 1.96*sd_tot
val.plsr.output$UPI <- val.plsr.output$PLSR_Predicted + 1.96*sd_tot
head(val.plsr.output)

# JK regression coefficient plot
pdf(paste0(inVar,"_Jackknife_Regression_Coefficients.pdf")) #open a pdf device

par(mfrow = c(1, 1))

spectratrait::f.plot.coef(Z = t(Jackknife_coef), wv = wv, 
                          plot_label = "Jackknife regression coefficients", position = "bottomleft")
abline(h = 0, lty = 2, col = "grey50")
box(lwd = 2.2)

dev.off() #close the pdf device to save plot

# JK validation plot
rmsep_percrmsep <- spectratrait::percent_rmse(plsr_dataset = val.plsr.output, 
                                              inVar = inVar, 
                                              residuals = val.plsr.output$PLSR_Residuals, 
                                              range = "full")
RMSEP <- rmsep_percrmsep$rmse
perc_RMSEP <- rmsep_percrmsep$perc_rmse
r2 <- round(pls::R2(plsr.out, newdata = val.plsr.data, intercept = F)$val[ncomps], 2)
expr <- vector("expression", 3)
expr[[1]] <- bquote(R^2 ==. (r2))
expr[[2]] <- bquote(RMSEP ==. (round(RMSEP, 2)))
expr[[3]] <- bquote("%RMSEP" ==. (round(perc_RMSEP,2)))
rng_vals <- c(min(val.plsr.output$LPI), max(val.plsr.output$UPI))

# Plot results
pdf(paste0(inVar,"_PLSR_Validation_Scatterplot.pdf")) #open a pdf device

par(mfrow = c(1, 1), mar = c(4.2, 5.3, 1, 0.4), oma = c(0, 0.1, 0, 0.2))

plotrix::plotCI(val.plsr.output$PLSR_Predicted,val.plsr.output[, inVar], 
                li = val.plsr.output$LPI, ui = val.plsr.output$UPI, gap = 0.009, sfrac = 0.004, 
                lwd = 1.6, xlim = c(rng_vals[1], rng_vals[2]), ylim = c(rng_vals[1], rng_vals[2]), 
                err = "x", pch = 21, col = "black", pt.bg = scales::alpha("grey70", 0.7), scol = "grey50",
                cex = 2, xlab = paste0("Predicted ", paste(inVar), " (m2/kg)"),
                ylab = paste0("Observed ", paste(inVar), " (m2/kg)"),
                cex.axis = 1, cex.lab = 1.2)
abline(0, 1, lty = 2,lw = 2)
legend("topleft", legend = expr, bty = "n", cex = 1.5)

dev.off() #close the pdf device to save plot

## Output jackknife results----
# JK Coefficents
out.jk.coefs <- data.frame(Iteration = seq(1, seg, 1), Intercept = Jackknife_intercept, t(Jackknife_coef))
head(out.jk.coefs)[1:6]
write.csv(out.jk.coefs,file = file.path(outdir,paste0(inVar,"_Jackkife_PLSR_Coefficients.csv")),
          row.names = F) #save data

## Export Model Output----
# Observed versus predicted
write.csv(cal.plsr.output, file = file.path(outdir, paste0(inVar, "_Observed_PLSR_CV_Pred_", ncomps,
                                                       "comp.csv")), row.names = F)

# Validation data
write.csv(val.plsr.output, file = file.path(outdir, paste0(inVar, "_Validation_PLSR_Pred_", ncomps,
                                                       "comp.csv")), row.names = F)

# Model coefficients
coefs <- coef(plsr.out, ncomp = ncomps, intercept = T)
write.csv(coefs, file = file.path(outdir, paste0(inVar, "_PLSR_Coefficients_", ncomps, "comp.csv")),
          row.names = T)

# PLSR VIP
write.csv(vips, file = file.path(outdir, paste0(inVar, "_PLSR_VIPs_", ncomps,"comp.csv")))
