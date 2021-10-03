install.packages("corrplot")
library(corrplot)

tma.medians <- read.table('2021-08-10_NM_tma.medians.txt');
tma.data <- read.table('2021-08-10_NM_depmap.tma.data.formatted.txt');
tma.data$RegionID <- NULL;

M<-cor(tma.data)
head(round(M,2))

corrplot(M, 
         method = "circle", 
         tl.cex = 1,
         cl.cex = 1.5
         )

?corrplot

tma.medians <- read.table('2021-08-10_NM_tma.medians.txt');
depmap.data <- read.table('2021-08-10_NM_depmap.drug.screen.data.formatted.txt');

plot(depmap.data$foretinib..BRD.BRD.K03449891.001.08.6., tma.medians$NE.F14)
abline(lm(depmap.data$asenapine..BRD.BRD.K95260951.050.03.1. ~ tma.medians$NE.F57))
