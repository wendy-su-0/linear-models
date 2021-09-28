### PREAMBLE ######################################################################################
library(BoutrosLab.utilities);
library(BoutrosLab.plotting.general);

### PLOTTING FUNCTIONS ############################################################################
# VOLCANO PLOT: Volcano plots are used to show/compare the results consisting of multiple tests
print.volcano.plot <- function(p.threshold = 0.1) {
    
    ### LOAD DATA #################################################################################
    model.results <- data.frame(
        beta = runif(n = 1000, min = -500, max = 500),
        pval = runif(n = 1000, min = 0.000000001, max = 0.15) # The max/min values are artificial for teaching purposes only
        );
    
    ### CORRECT FOR MULTIPLE TESTING ##############################################################
    model.results$fdr <- p.adjust(model.results$pval, method = 'fdr');
    
    # Transform FDR values for plot
    model.results$log.fdr <- -log10(model.results$fdr);
    
    ### VOLCANO PLOT #############################################################################
    # Set the effect size here, or place it as a function argument if you want to assess different values
    effect.size.cols <- c('beta');
    p.cols           <- c('log.fdr');
    
    # Set dot color to highlight hits
    dot.colors <- vector(length = length(model.results[, p.cols]));
    dot.colors[model.results[, p.cols] > -log10(p.threshold) &  model.results[, effect.size.cols] > 0] <- 'dodgerblue';
    dot.colors[model.results[, p.cols] > -log10(p.threshold) &  model.results[, effect.size.cols] < 0] <- 'darkorange1';
    dot.colors[model.results[, p.cols] <= -log10(p.threshold)] <- 'black';
    dot.colors[FALSE == dot.colors] <- 'black';
    
    # Automatic Labeling
    interesting.rho     <- (abs(model.results[, effect.size.cols]) > 0);
    interesting.p.value <- model.results[, p.cols] > 1;
    interesting.points  <- interesting.rho & interesting.p.value;
    
    text.x <- as.numeric(as.vector(na.omit(model.results[, effect.size.cols][interesting.points])))  * runif(sum(interesting.points, na.rm = T), 0.8, 1.2); # Shuffle the location
    text.y <- as.numeric(as.vector(na.omit(model.results[, p.cols][interesting.points]))) * runif(sum(interesting.points, na.rm = T), 0.8, 1.4);
    text.labels <- paste0(na.omit(model.results$gene[interesting.points])); # This would be the rownames or another variable
    
    create.scatterplot(
        formula = model.results[, p.cols] ~ model.results[, effect.size.cols],
        data = model.results,
        col = dot.colors,
        alpha = 0.5,
        cex = 2,
        xlimits = c(-500, 500),
        xat = seq(-500, 500, 250),
        ylimits =  c(0, 2),
        yaxis.lab = expression(1, 10^-1, 10^-2),
        yat = c(0, 1, 2),
        yaxis.cex = 1.5,
        xaxis.cex = 1.5,
        xlab.label = 'Effect size',
        ylab.label = expression('q'),
        xlab.cex = 1.5,
        ylab.cex = 1.5,
        abline.h = -log10(0.1),
        #abline.v = c(-0.2, 0.2),
        abline.col = 'gray20',
        abline.lwd = 0.7,
        abline.lty = 'dashed',
        resolution = 200,
        add.text = FALSE, #Change this to TRUE if you have the labels
        text.x = text.x,
        text.y = text.y,
        text.labels = text.labels
        );
}

### DATA ANALYSIS #################################################################################
untar('C:/Users/wsu31/Downloads/BoutrosLab.statistics.general_2.1.3.tar.gz', list = TRUE)
install.packages('C:/Users/wsu31/Downloads/BoutrosLab.statistics.general_2.1.3.tar.gz', repos = NULL, type = 'source')

library(BoutrosLab.statistics.general)

print.volcano.plot()

### END ###########################################################################################