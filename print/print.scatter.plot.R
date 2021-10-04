### DESCRIPTION ###################################################################################

# Generate linear models of all the nuclear features and associated data. Saves the p value.
# Saves the beta coefficient. Saves the adjuted p-value.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/');
library('tidyr');
library('dplyr');
library('lme4');

### MAKE LINEAR MODELS ############################################################################

format.CCLE.data();

LMEM <-  lm(depmap.data[ ,3648] ~ tma.medians[,100]);

plot(tma.medians[,109], depmap.data[ ,3648], main = "Correlation of Chromatin Heterogenity and Cisplatin",
     xlab = "Nuclear Feature NE-F57: Chromatin Heterogenity", ylab = "Drug Sensitivity: Cisplatin",
     pch = 19, frame = FALSE, xlim = c(0,0.35))
abline(lm(depmap.data[ ,3648] ~ tma.medians[,109], data = mtcars), col = "blue")

?plot()
getwd()

plot(tma.medians$NH.F58, depmap.data$cisplatin..BRD.BRD.K69172251.001.08.9., main = "Cisplatin Sensitivity vs Chromatin Margination",
     xlab = "Nuclear Feature NH-F58: Chromatin Margination", ylab = "Drug Sensitivity: Cisplatin",
     pch = 19, frame = FALSE)
abline(lm(depmap.data$cisplatin..BRD.BRD.K69172251.001.08.9. ~ tma.medians$NH.F58), col = "blue")

### DATA ANALYSIS ##################################################################################

### END ############################################################################################