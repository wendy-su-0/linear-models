### DESCRIPTION ###################################################################################

# Analyze the results from the linear models. Find the models with significance.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Orsulic Lab Things/TMA data/drug/');
library('tidyr');
library('dplyr');
library('BoutrosLab.plotting.general');

### FIND SIGNIFICANT MODELS #######################################################################

find.significant.models <- function() {
  
  ### LOAD DATA ###################################################################################
  adjusted.p.values <- read.table('TMA_drug_CTD_lin_models.csv');
  
  ### NA IF NOT SIGNIFICANT #######################################################################
  filtered.p.adjust <- adjusted.p.values %>%
    filter_all(any_vars(. < 0.1));
  filtered.p.adjust[filtered.p.adjust > 0.1] <- NA;
  filtered.p.adjust <- filtered.p.adjust[,colSums(is.na(filtered.p.adjust))<nrow(filtered.p.adjust)];
  
  ### SAVE DATA ###################################################################################
  write.table(filtered.p.adjust, '2021-07-26_NM_significant.models.txt', sep = '\t');
  
};

### DATA ANALYSIS #################################################################################

  ### CTD DATASET #################################################################################
  getwd()

  p.values <- read.table('C:/Users/wsu31/Downloads/2021-08-26_NM_p.values.txt');
  adjusted.p.values <- p.values
  
  for (i in 1:ncol(adjusted.p.values)) {
    adjusted.p.values[,i] = p.adjust(adjusted.p.values[,i], method = "fdr", n = length(adjusted.p.values[,i]))
  }
  
  write.table(adjusted.p.values, '2021-08-09_NM_adj.p.values.txt', sep = '\t');
  
  min(adjusted.p.values)
  
  filtered.p.adjust <- adjusted.p.values %>% 
    filter_all(any_vars(. < 0.1));
  filtered.p.adjust[filtered.p.adjust > 0.1] <- NA;
  filtered.p.adjust <- filtered.p.adjust[,colSums(is.na(filtered.p.adjust))<nrow(filtered.p.adjust)];
  
  sum(!is.na(filtered.p.adjust))
  
  onlyCisplatins = adjusted.p.values[c(31, 83, 344, 345, 364, 377, 387, 390, 397),]
  
  min(onlyCisplatins[2,]) 
  filtered.only.cisplatins = onlyCisplatins %>%
    #select_all(any < 0.1) %>%
    filter_all(any_vars(. < 0.57))
  
  filtered.only.cisplatins[filtered.only.cisplatins > 0.57] = NA
  filtered.only.cisplatins = filtered.only.cisplatins[,colSums(is.na(filtered.only.cisplatins))<nrow(filtered.only.cisplatins)]
  
### END ###########################################################################################