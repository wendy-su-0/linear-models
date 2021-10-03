### DENSITY PLOTS ###############################################################

print.density.plot <- function(){
  ### LOAD DATA #################################################################
  tma.data <- read.table('2021-08-10_NM_tma.medians.txt');
  tma.data.unfiltered <- read.table('2021-08-10_NM_depmap.tma.data.formatted.txt');
  
  ### CREATE DENSITY PLOT #######################################################
  create.densityplot(
    x = as.data.frame(t(tma.data))
  )
  
  create.densityplot(
    x = as.data.frame((tma.data))
  )
  
  create.densityplot(
    x = as.data.frame((tma.data.medians))
  )
  
  create.densityplot(
    x = as.data.frame((tma.data[1:6, 1:2])),
    
    col = default.colours(2)
  )
  
  create.densityplot(
    x = as.data.frame(t(tma.data.zscore[5:6,])),
    col = default.colours(2)
  )
  
  #just nf 5
  create.densityplot(
    x = as.data.frame((tma.data[1:6,5])),
    col = default.colours(1),
    xlab.label = "Minor/Major Axis Ratio"
  )
  
  #just nf 6
  create.densityplot(
    x = as.data.frame((tma.data[1:6,109])),
    col = 'seagreen',
    xlab.label = "Chromatin Heterogeneity"
  )
  
  ?create.densityplot
  
  create.densityplot(
    x = as.data.frame((tma.data.zscore))
  )
  
  create.densityplot(
    x = ((tma.data.unfiltered[,2:114]))
  )
  
}

?zscore
tma.data.zscore <- apply(tma.data, 1, zscore)

tma.data.zscore <- as.data.frame(tma.data.zscore)

#just nf 5
create.densityplot(
  x = as.data.frame((tma.data.zscore[1:6,5])),
  col = default.colours(1),
  xlab.label = "Minor/Major Axis Ratio"
)

#just nf 6
create.densityplot(
  x = as.data.frame((tma.data.zscore[1:6,6])),
  col = 'seagreen',
  xlab.label = "Area (??m²)"
)


rm(tma.data.vector)

?create.densityplot

##try taking the median of each of the things
tma.data.medians <- apply(tma.data, 2, median)

tma.data.zscore <- as.data.frame(t(tma.data.zscore))

write.table(tma.data.zscore, '2021-08-10_NM_tma.medians.zscore.txt', sep = '\t')

### CORRELATION MATRIX ##########################################################

### DOT MAP #####################################################################