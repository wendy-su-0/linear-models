### DESCRIPTION ###################################################################################

# Fromats data downloaded from DepMap. Removes unnecessary columns: depmapid, cell 
# lineage information past cancer type. Saves the formatted data

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/');
library('tidyr');
library('dplyr');

### TITLE ##########################################################################################

format.CCLE.data <- function(depmap.file.name, date, DepMap.data.type) {
    
    ### LOAD DATA ###################################################################################
    depmap.data <- read.csv(file.path("raw-data", depmap.file.name));
   
    ### FORMAT DATA ##################################################################################
    depmap.data <- subset(depmap.data, select = -c(depmap_id, lineage_2, lineage_3, lineage_4));
    
    ### SAVE DATA ####################################################################################
    write.table(depmap.data, file.path("outputs", paste(date, DepMap.data.type, 'formatted.txt', sep = '-')), sep = '\t');
};

### DATA ANALYSIS ##################################################################################

format.CCLE.data('Drug_sensitivity_(PRISM_Repurposing_Primary_Screen)_19Q4_subsetted.csv', '2021.10.02', 'drug.sensitivity.seven.ovarian');

### END ############################################################################################