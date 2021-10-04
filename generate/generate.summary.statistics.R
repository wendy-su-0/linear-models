### DESCRIPTION ###################################################################################

# Assesses the medians of the formatted TMA data. Assesses other summary statistics of the TMA data.
# Saves those medians. Saves summary statistics.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/')
library(tidyr);
library(dplyr);
library(Hmisc);

### GENERATE HISTOGRAMS ############################################################################

generate.histograms <- function(nuclear.features.file.name, date) {
  
  ### READ DATA ####################################################################################
  tma.data.formatted <- read.table(file.path("outputs/data", nuclear.features.file.name));
  
  ### MAKE PDF #####################################################################################
  pdf(file = file.path('outputs/plots', paste(date, 'nuclear.feature.histograms.pdf', sep = '-')), onefile = TRUE);
  
  ### MAKE HISTOGRAMS ##############################################################################
  hist.data.frame(tma.data.formatted[c(-1)]);
  
  ### SAVE PDF #####################################################################################
  dev.off(); 
  
}

### GENERATE SUMMARY STATISTICS ####################################################################

assess.summary.statistics <- function(tma.data.name, depmap.data.name) {
  
  ### LOAD DATA ####################################################################################
  tma.data.formatted <- read.table(file.path("outputs/data", tma.data.name));
  depmap.data.formatted <- read.table(file.path("outputs/data", depmap.data.name));
  
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
  write.table(tma.medians, file.path("outputs/data", paste(date, 'tma.medians.txt', sep = '-')), sep = '\t');
  write.table(tma.summary.statsitics, file.path("outputs/data", paste(date, 'tma.summary.statsitics.txt', sep = '-')), sep = '\t');
  
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