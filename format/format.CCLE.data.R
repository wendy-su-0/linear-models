### DESCRIPTION ###################################################################################

# Fromats data downloaded from DepMap. Removes unnecessary columns: depmapid, cell 
# lineage information past cancer type. Saves the formatted data

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Orsulic Lab Things/TMA data/drug/');
library('tidyr');
library('dplyr');

### TITLE ##########################################################################################

format.CCLE.data <- function() {
    
    ### LOAD DATA ###################################################################################
    depmap.data <- read.csv('Drug_sensitivity_(PRISM_Repurposing_Primary_Screen)_19Q4_subsetted.csv');
    
    ### FORMAT DATA ##################################################################################
    depmap.data <- subset(depmap.data, select = -c(depmap_id, lineage_2, lineage_3, lineage_4));
    
    ### SAVE DATA ####################################################################################
    write.table(depmap.data, '2021-07-26_NM_depmap.drug.screen.data.formatted.txt', sep = '\t');
};

### DATA ANALYSIS ##################################################################################

format.CCLE.data();

### END ############################################################################################