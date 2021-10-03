### DESCRIPTION ###################################################################################

# Formats data from TMA. Removes rows w/o associated DepMap data
# Renames the TMA rows so it lines up with DepMap names. Saves the formatted data

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/');
library('tidyr');
library('dplyr');

#### FORMAT DATA #########################################################################################

format.TMA.data <- function(TMA.file.name, date, TMA.data.type) {
  ### LOAD DATA ###################################################################################
  tma.data <- read.csv(file.path("raw-data", TMA.file.name), fileEncoding = 'UTF-8-BOM');
  
  ### REMOVE ROWS W/O DEPMAPDATA ##########################################################################
  tma.data <- subset(tma.data, tma.data$RegionID == 'Cell_lines_TMA_Core_CaoV3-R11' | 
                       tma.data$RegionID == 'Cell_lines_TMA_Core_DU145-R23' |
                       tma.data$RegionID == 'Cell_lines_TMA_Core_S2-R5' | 
                       tma.data$RegionID == 'Cell_lines_TMA_Core_Hec1A-R6' |
                       tma.data$RegionID == 'Cell_lines_TMA_Core_HEK293T-R13' | 
                       tma.data$RegionID == 'Cell_lines_TMA_Core_ZR-75-1-R3' |
                       tma.data$RegionID == 'Cell_lines_TMA_Core_HepG2-R26' | 
                       tma.data$RegionID == 'Cell_lines_TMA_Core_IGROV-R21' |
                       tma.data$RegionID == 'Cell_lines_TMA_Core_OV90-R9' | 
                       tma.data$RegionID == 'Cell_lines_TMA_Core_OVCAR8-R8' |
                       tma.data$RegionID == 'Cell_lines_TMA_Core_TOV112D-R17' | 
                       tma.data$RegionID == 'Cell_lines_TMA_Core_TOV21G-R20');
  
  ### REMOVE COLUMN ##########################################################################
  tma.data = subset(tma.data, select = -c(CaseID));
  
  ### RENAME ROWS ###################################################################################
  tma.data[tma.data == 'Cell_lines_TMA_Core_CaoV3-R11'] <- 'CAOV3';
  tma.data[tma.data == 'Cell_lines_TMA_Core_DU145-R23'] <- 'DU145';
  tma.data[tma.data == 'Cell_lines_TMA_Core_S2-R5'] <- 'ES2';
  tma.data[tma.data == 'Cell_lines_TMA_Core_Hec1A-R6'] <- 'HEC1A';
  tma.data[tma.data == 'Cell_lines_TMA_Core_HEK293T-R13'] <- 'HEKTE';
  tma.data[tma.data == 'Cell_lines_TMA_Core_HepG2-R26'] <- 'HEPG2';
  tma.data[tma.data == 'Cell_lines_TMA_Core_IGROV-R21'] <- 'IGROV1';
  tma.data[tma.data == 'Cell_lines_TMA_Core_LNCAP-R24'] <- 'LNCAPCLONEFGC';
  tma.data[tma.data == 'Cell_lines_TMA_Core_MCF7-R2'] <- 'MCF7';
  tma.data[tma.data == 'Cell_lines_TMA_Core_OV90-R9'] <- 'OV90';
  tma.data[tma.data == 'Cell_lines_TMA_Core_OVCAR8-R8'] <- 'OVCAR8';
  tma.data[tma.data == 'Cell_lines_TMA_Core_PC3-R25'] <- 'PC3';
  tma.data[tma.data == 'Cell_lines_TMA_Core_T47D-R1'] <- 'T47D';
  tma.data[tma.data == 'Cell_lines_TMA_Core_TOV112D-R17'] <- 'TOV112D';
  tma.data[tma.data == 'Cell_lines_TMA_Core_TOV21G-R20'] <- 'TOV21G';
  tma.data[tma.data == 'Cell_lines_TMA_Core_ZR-75-1-R3'] <- 'ZR751';
  
  ### SAVE DATA ####################################################################################
  write.table(tma.data, file.path("outputs", paste(date, TMA.data.type, 'formatted.txt', sep = '-')), sep = '\t');
  
};

### DATA ANALYSIS ##################################################################################

format.TMA.data('Cell_lines_TMA_HE.csv', '2021.10.02', 'tma.data.seven.ovarian');

### END ############################################################################################