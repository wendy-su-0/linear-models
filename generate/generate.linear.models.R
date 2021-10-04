### DESCRIPTION ###################################################################################

# Generate linear models of all the nuclear features and associated data. Saves the p value.
# Saves the beta coefficient. Saves the adjuted p-value.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Desktop/R Stuff/linear-models/');
library('tidyr');
library('dplyr');
library('lme4');

### MAKE LINEAR MODELS ############################################################################

generate.linear.models <- function(tma.medians.file.name, depmap.formatted.file.name, only.ovcar) {
  
  ### LOAD DATA ###################################################################################
  tma.medians <- read.table(file.path('outputs.data', tma.medians.file.name));
  depmap.data <- read.table(file.path('outputs.data', depmap.formatted.file.name));
  
  ### MAKE DATAFRAME FOR MODELS ###################################################################
  cols <- colnames(tma.medians)[2:length(tma.medians)];
  rows <- colnames(depmap.data)[3:length(depmap.data)];
  
  p.values <- tma.medians[rows, cols];
  adjusted.p.values <- NULL;
  beta.coefficients <- tma.medians[rows, cols]; 
  
  rownames(p.values) <- colnames(depmap.data)[3:length(depmap.data)];
  rownames(beta.coefficients) <- colnames(depmap.data)[3:length(depmap.data)];
  
  ### CREATE LINEAR MODELS ########################################################################
  for (nfCol in 1:ncol(tma.medians)) {
    
    for (depmapCol in 3:ncol(depmap.data)) {
      
      if(only.ovcar) {
        
        ### YOU MIGHT HAVE MADE ERROR: DEPENDENT ON LEFT AND INDEPENDENT ON RIGHT ################
        
        LMEM <-  lm(depmap.data[ ,depmapCol] ~ tma.medians[,nfCol]);
         ### insert null model of the nf with the age 
        
      } else {
          
        LMEM <- lmer(depmap.data[ ,depmapCol] ~ tma.medians[, nfCol] + (1|depmap.data$lineage_1));
        
      }
      
      ### CALCUALTE P AND B #######################################################################
      
      p.value <- summary(LMEM)$coefficients[2,4];
      beta.coef <- summary(LMEM)$coefficients[2,1];
      
      ### ADD TO DATAFRAME ########################################################################
      
      p.values[depmapCol - 2, nfCol] <- p.value;
      beta.coefficients[depmapCol - 2, nfCol] <- beta.coef;
      
    }
    
  }
  
  ### ADJUST P VALUES ############################################################################
  adjusted.p.values <- p.values;
  for (i in 1:ncol(adjusted.p.values)) {
    
    adjusted.p.values[,i] = p.adjust(adjusted.p.values[,i], method = "fdr",  n = (nrow(adjusted.p.values)));
  
  };
  
  write.table(p.values, file.path("outputs/statistics", paste(date, 'p.values.txt', sep = '-')), sep = '\t');
  write.table(adjusted.p.values, file.path("outputs/statistics", paste(date, 'adjusted.p.values.txt', sep = '-')), sep = '\t');
  write.table(beta.coefficients, file.path("outputs/statistics", paste(date, 'beta.coefficients.txt', sep = '-')), sep = '\t');
  
};

### DATA ANALYSIS ##################################################################################

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

### CISPLATINS ONLY ################################################################################
p.values <- NULL;

for (nfCol in 1:ncol(tma.medians)) {
  
   LMEM <- lm(depmap.data[ ,3648] ~ tma.medians[ ,33]);
      
    ### CALCUALTE P AND B #######################################################################
    
    p.values <- rbind(p.values, summary(LMEM$coefficients[2,4]));
  
};

### END ############################################################################################