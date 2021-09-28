### DESCRIPTION ###################################################################################

# Analyze the results from the linear models. Find the models with significance.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Orsulic Lab Things/TMA data/drug/');
library('tidyr');
library('dplyr');
library('BoutrosLab.plotting.general');

### VOLCANO PLOT ##################################################################################

generate.volcano.plot <- function () {
  
  ### LOAD DATA ###################################################################################
  p.values <- read.table('C:/Users/wsu31/Downloads/2021-08-26_NM_p.values.txt');
  adjusted.p.values <- read.table('2021-08-09_NM_adj.p.values.txt');
  beta.coefficients <- read.table('C:/Users/wsu31/Downloads/2021-08-26_NM_beta.coefficients.txt');
  #load adjusted p value
  #load beta coefficients
  
  ### CREATE DATA FRAME FOR VOLCANO PLOT ##########################################################
  volcano.plot.data <- NULL;
  
  for(j in 1:ncol(p.values)) {
    
    for(i in 1:nrow(p.values)) {
      
      p.val <- p.values[i,j];
      adjusted.p.val <- adjusted.p.values[i,j];
      beta.coef <- beta.coefficients[i,j];
      
      temp <- c(p.val, adjusted.p.val, beta.coef);
      
      volcano.plot.data <- rbind(volcano.plot.data, temp);
      
    };
    
  };
  
  colnames(volcano.plot.data) <- c("p.value", "adjusted.p.value", "beta.coefficient");
  
  volcano.plot.data <- as.data.frame(volcano.plot.data)
  
  write.table(volcano.plot.data, '2021-08-09_NM_volcano.plot.data.txt', sep = '\t')
  
  ### PREPARE VOLCANO PLOT DATA ###################################################################
  points.x <- c();
  points.y <- c();
  
  for (i in 1:527032){
    if( (volcano.plot.data$p.value[i] <= 0.05) & (volcano.plot.data$adjusted.p.value[i] <= 0.1) ){
      points.x <- c(points.x, volcano.plot.data$p.value[i])
      points.y <- c(points.y, volcano.plot.data$adjusted.p.value[i])
    }
  }
  
  log.FDR <- -log10(volcano.plot.data$adjusted.p.value);
  volcano.plot.data.log <- volcano.plot.data
  
  volcano.plot.data.log$adjusted.p.value <- log.FDR
  
  ### VOLCANO PLOT ################################################################################
  drug.volcano.plot <- create.scatterplot(
    formula = adjusted.p.value ~ beta.coefficient,
    data = volcano.plot.data,
    
    add.points = TRUE,
    points.x = points.x,
    points.y = points.y,
    points.col = "red",
    
  );
  
  drug.volcano.plot
  
  ?create.scatterplot
  
  ### SAVE PLOT ###################################################################################
  #save volcano plot
  
};

### FDR ONLY PLATINS ##############################################################################
only.platins <-  adjusted.p.values %>% slice(2724, 3648, 3793, 3975, 3976)
min(only.platins)

### END ###########################################################################################