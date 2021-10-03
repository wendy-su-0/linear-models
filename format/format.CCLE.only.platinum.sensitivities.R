### DESCRIPTION ###################################################################################

# Saves the platinum based drug sensitivity data.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/');
library('tidyr');
library('dplyr');

### TITLE #########################################################################################

format.CCLE.data <- function(depmap.file.name, date, DepMap.data.type) {
  
  ### LOAD DATA #################################################################################
  depmap.data <- read.csv(file.path("raw-data", depmap.file.name));
  
  ### FORMAT DATA ###############################################################################
  depmap.data <- subset(depmap.data, select = -c(depmap_id, lineage_2, lineage_3, lineage_4));
  
  ### SAVE DATA #################################################################################
  write.table(depmap.data, file.path("outputs", paste(date, DepMap.data.type, 'formatted.txt', sep = '-')), sep = '\t');
};

### DATA ANALYSIS #################################################################################

format.CCLE.data('Drug_sensitivity_(PRISM_Repurposing_Primary_Screen)_19Q4_subsetted.csv', '2021.10.02', 'drug.sensitivity.seven.ovarian');

### END ###########################################################################################




### DESCRIPTION ###################################################################################

# Saves the top 20% most variable DepMap measurements.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/')
library(tidyr);
library(dplyr);
library(Hmisc);

### GENERATE HISTOGRAMS ############################################################################

generate.histograms <- function(file.name) {
  
  ### READ DATA ####################################################################################
  tma.data.formatted <- read.table(file.name);
  
  ### MAKE PDF #####################################################################################
  pdf(file = 'nfHistograms.pdf', onefile = TRUE);
  
  ### MAKE HISTOGRAMS ##############################################################################
  hist.data.frame(tma.data.formatted[c(-1)]);
  
  ### SAVE PDF #####################################################################################
  dev.off(); 
  
}

### GENERATE SUMMARY STATISTICS ####################################################################

assess.summary.statistics <- function() {
  
  ### LOAD DATA ####################################################################################
  tma.data.formatted <- read.table('2021-07-26_NM_depmap.tma.data.formatted.txt');
  depmap.data.formatted <- read.table('2021-07-26_NM_depmap.drug.screen.data.formatted.txt');
  
  ### PREPARE MEDIAN DATAFRAME #####################################################################
  column.names <- colnames(tma.data.formatted)[2:length(tma.data.formatted)];
  cell.lines <- depmap.data.formatted$cell_line_display_name;
  tma.medians <- tma.data.formatted[cell.lines, column.names];
  rownames(tma.medians) <- cell.lines;
  
  ### PREPARE SUMMARY DATAFRAME ####################################################################
  tma.summary.statsitics <- NULL;
  
  ### ASSESS SUMMARY STATISTICS #####################################################################
  for (col in 2:ncol(tma.data.formatted)) {
    
    for (row in 1:length(cell.lines)) {
      
      cell.line <- filter(tma.data.formatted, RegionID == cell.lines[row]);
      cell.line.median <- median(as.vector(cell.line[, col]));
      tma.medians[row, col-1] <- cell.line.median;
      
    }
    
    tma.mean <- mean(as.vector(tma.data.formatted[, col]));
    tma.median <- median(as.vector(tma.data.formatted[, col]));
    tma.iqr <- IQR(as.vector(tma.data.formatted[, col]));
    
    temp.df <- c(tma.mean, tma.median, tma.iqr);
    
    tma.summary.statsitics <- rbind(tma.summary.statsitics, temp.df);
    
  }
  
  ### RENAME ROWS OF SUMMARY STATISTICS ############################################################
  colnames(tma.summary.statsitics) <-  c('mean', 'median', 'IQR');
  rownames(tma.summary.statsitics) <- colnames(tma.data.formatted[ , 2:length(tma.data.formatted)]);
  
  ### SAVE DATA ####################################################################################
  write.table(tma.medians, '2021-07-26_NM_tma.medians.txt', sep = '\t');
  write.table(tma.summary.statsitics, '2021-07-26_NM_tma.summary.statistics.txt', sep = '\t');
  
};

### FILTER TOP 20% #################################################################################

filter.top.twenty <- function(summary.statistics.file.name, tma.medians.file.name) {
  
  ### LOAD DATA ####################################################################################
  tma.summary.statistics <- read.table('2021-07-26_NM_tma.summary.statistics.txt');
  tma.medians <- read.table(tma.medians.file.name);
  
  ### FILTER TOP 20% ###############################################################################
  most.variable <- tma.summary.statistics %>% slice_max(IQR, prop = 0.2)
  features.to.keep <- rownames(most.variable)
  top.twenty <- tma.medians[features.to.keep]
  
  ### SAVE DATA ####################################################################################
  write.table(top.twenty, '2021-07-26_NM_top.twenty.tma.medians.txt', sep = '\t');
  
};

### DATA ANALYSIS ##################################################################################

generate.histograms();
assess.summary.statistics();

### END ############################################################################################