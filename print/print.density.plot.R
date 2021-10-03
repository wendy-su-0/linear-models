### DESCRIPTION ###################################################################################

# Create density plot.

### PREAMBLE ######################################################################################

setwd('/Users/wsu31/OneDrive/Orsulic Lab Things/Pipeline/log regression/');
library(BoutrosLab.plotting.general)

### DENSITY PLOTS #################################################################################

print.density.plotS.all <- function(){
  ### LOAD DATA ###################################################################################
  tma.data <- read.table('2021-08-10_NM_tma.medians.txt');
  tma.data.zscore <- read.table('2021-08-10_NM_tma.medians.zscore.txt');
  
  ### CREATE DENSITY PLOT #########################################################################
  dens.plot.all <- create.densityplot(
    x = as.data.frame((tma.data))
  )
  
  dens.plot.all.zscore <- create.densityplot(
    x = as.data.frame((tma.data.zscore))
  )
  
  ### SAVE DENSITY PLOTS #########################################################################
  tiff("dens.plot.all.untransformed", compression = "zip")
  plot(dens.plot.all)
  dev.off()
  
  tiff("dens.plot.all.zscore", compression = "zip")
  plot(dens.plot.all.zscore)
  dev.off()
  
}

### PRINT SINGULAR PLOT ############################################################################

### SPREAD OF DENSITY PLOTS ########################################################################
tma.data.zscore <- read.table('2021-08-10_NM_tma.medians.zscore.txt');

pdf("dens.plot.each.nuclear.feature");
for(i in 1:ncol(tma.data.zscore)) {
  dens.plot <- create.densityplot(x = as.data.frame(tma.data.zscore[1:6, 6]))
   
  plot(dens.plot)
}
dev.off();

create.scatterplot(
  x = (as.data.frame(tma.data.zscore[, 6]))
)

### DATA ANALYSIS ##################################################################################

### END ############################################################################################