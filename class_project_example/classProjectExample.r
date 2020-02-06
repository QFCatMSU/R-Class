### Class Project example code ###
# Note: This script meets Project Requirements a,b,c,d, g, and h
# It does not have enough for() or if() to meet d or e
# Skills used from the class project rubric are labelled SKILL xx
# -- only some of the skills used in script are listed in the table!
##################################


# enclose full script in brackets to improve
# debugging performance
{
  # remove any objects from the environment
  rm(list=ls());
  
  # turn on error line numbers
  options(show.error.locations = TRUE);
    
  # Set the working directory (this is specific to user)
  # setwd('');
  
  # Read in the scallop data. Columns are:
  # SAMPLE: scallop sample ID number
  # LOC: whether it was caught in East or West region
  # SH: scallop shell height (umbo to shell margin; mm)
  # MW: scallop meat weight (g)
  sdat <- read.csv('scallopData.csv', header=TRUE);    # SKILL 14
  
  
  ###############################
  #### Variable declarations ####
  ###############################
  
  outCount <- NULL;  # counter for outliers
  scalE <- NULL;     # data frame for west region scallops
  scalW <- NULL;     # data frame for east region scallops
  logMWE <- NULL;    # logged MW from east region
  logSHE <- NULL;    # logged SH from east region
  logMWW <- NULL;    # logged MW from west region
  logSHW <- NULL;    # logged SH from west region
  lmE <- NULL;       # east region linear model
  lmW <- NULL;       # west region linear model
  residE <- NULL;    # residuals from east linear model
  residW <- NULL;    # residuals from west linear model
  coefE <- NULL;     # east model coefficients
  coefW <- NULL;     # west model coefficients
  coefmat <- NULL;   # matrix to hold all lm coefficients
  SHseq <- NULL;     # sequence of shell heights for predictions
  npred <- NULL;     # number of predictions to be made
  predE <- NULL;     # predictions from east model
  predW <- NULL;     # predictions from west model
  
  
  #############################################
  #### Part 1: initial examination of data ####
  #############################################
  
  # Check whether the distribution of shell heights differs between
  # the east and west regions using a boxplot
  boxplot(SH ~ LOC, data=sdat,                        # SKILL 29, 51
          main='Regional Shell Height Comparison',
          xlab='Region', ylab='Shell Height (mm)');
  
  
  # Check whether the meat weights differ
  boxplot(MW ~ LOC, data=sdat, 
          main='Regional Meat Weight Comparison',
          xlab='Region', ylab='Shell Height (mm)');
  
  # Distribution of shell heights very similar but meat weights
  # are not ... probably a difference in the shell height to meat
  # weight relationship by region - investigate this further
  
  
  ###################################
  #### Part 2: meat weight plots ####
  ###################################
  
  
  # Look first at the overall relationship
  plot(MW ~ SH, data=sdat);
  
  # There are outliers, which here I'm defining as scallops
  # with SH < 80 and MW > 40 - these should be
  # NAs. Find them using a loop and change the
  # values. Also count up the number of outliers
  outCount <- 0;   # start counter at zero
  for(i in 1:nrow(sdat))                                  # SKILL 18, 20
  {
    if(sdat[i,'SH'] < 80 && sdat[i,'MW'] > 40)            # SKILL 11, 22
    {
      sdat[i,'MW'] <- NA;
      outCount <- outCount + 1;   # increase counter by 1
    }
  }
  
  # Print out the number of outliers (if any). Use the paste()
  # function to paste together string text with the value of a
  # variable (in this case outCount).
  if(outCount > 0)
  {
    print(paste('There were', outCount, 'outliers'));
  }
  else
  {
    print('there were no outliers');
  }
  
  
  # Plot SH vs MW for each region.  First subset out
  # the east and west scallops because those will be
  # plotted separately in different colors
  scalE <- sdat[sdat[,'LOC']=='EAST',];
  scalW <- sdat[sdat[,'LOC']=='WEST',];
  
  # plot the data separately. Initially use all the data at 
  # once but type='n' so that you don't plot any points.
  # This way the x and y ranges will be appropriately sized 
  # so all the points will be on the plot when we plot them 
  # separately in different colors.
  plot(MW ~ SH, data=sdat, type='n',
       xlab='Shell height (mm)',
       ylab='Meat weight (g)',
       main='Shell height - meat weight regional comparison');
  
  # now add the points -- east region in red, west in blue
  # use pch=3 to plot crosses and cex=1.25 to increase the
  # size a little bit
  points(MW ~ SH, data=scalE, col='red', 
         pch=3, cex=1.25);
  points(MW ~ SH, data=scalW, col='blue', 
         pch=3, cex=1.25);
  
  # Seems to be a difference in the relationship by region.
  
  
  ###################################
  #### Part 3: modeling SH vs MW ####
  ###################################
  
  
  # Fit a model to each region.  The general form of this type of
  # length-weight relationship is 
  # W = aL^b 
  # which can be converted
  # to a linear model using logs: 
  # log(W) = log(a) + b*log(L)
  # We will use this linear model to estimate parameters
  # and then create predictions for both models.
  
  # Get logged paramters to use in regression
  logMWE <- log(scalE[,'MW']);
  logSHE <- log(scalE[,'SH']);
  logMWW <- log(scalW[,'MW']);
  logSHW <- log(scalW[,'SH']);
  
  # Run the regression models
  lmE <- lm(logMWE ~ logSHE);     
  lmW <- lm(logMWW ~ logSHW);
  
  # Print model summaries
  print(summary(lmE));
  print(summary(lmW));
  
  # check the residuals from these models to make sure they
  # look OK
  residE <- resid(lmE);
  residW <- resid(lmW);
  hist(residE, main='Distribution of Eastern model residuals');  # SKILL 33
  hist(residW, main='Distribution of Western model residuals');
  
  
  ###########################################
  #### Part 4: make predictions and plot ####
  ###########################################
  
  
  # Make predictions for Eastern and Western scallop meat
  # weights at shell height.  Steps are:
  # (1) get the correct parameters out of the models
  # (2) create a vector of shell heights to predict 
  #     meat weight at SH
  # (3) use a for loop to make the predictions
  
  # create a function for the predictions of the form
  # MW = a * SH^b (i.e., weight=a*length^b)
  # a and b are parameters and SH is the height of the shell
  MWhat <- function(a, b, SH)    # SKILL 37
  {
    return(a * SH^b);        # SKILL 3
  }
  
  # Get out the model coefficients
  coefE <- coef(lmE);
  coefW <- coef(lmW);
  
  # create a matrix of the parameter values where the rows
  # represent the two sites and the columns represent the
  # two parameters. Back-transform the "a" parameters (note when
  # we transfered the model to logs a became log(a) but we
  # don't need to back-transform b because b was not inside
  # a logarithm).
  coefmat <- matrix(data=c(exp(coefE[1]),
                           exp(coefW[1]),
                           coefE[2],
                           coefW[2]),
                    nrow=2,
                    dimnames=list(c('East', 'West'),
                                  c('a', 'b')));
  print(coefmat);
  
  # create a model prediction vector.  There will be MW
  # predictions for each of these shell heights.
  SHseq <- 0:200;
  
  # number of predictions we will make
  npred <- length(SHseq);
  
  # create containers of appropriate length to hold 
  # the predictions for both models
  predE <- numeric(npred);
  predW <- numeric(npred);
  
  # use a for() loop to make predictions for the east
  # and west models
  for(i in 1:npred)
  {
    # use the MWhat function to make predictions
    predE[i] <- MWhat(a=coefmat['East', 'a'],   # SKILL 36
                      b=coefmat['East', 'b'],
                      SH=SHseq[i]);
    predW[i] <- MWhat(a=coefmat['West', 'a'], 
                      b=coefmat['West', 'b'],
                      SH=SHseq[i]);
  }
  
  
  # plot the results (this plot with the points is copied
  # from earlier in the code)
  plot(MW ~ SH, data=sdat, type='n', # don't plot points
       xlab='Shell height (mm)',
       ylab='Meat weight (g)',
       main='Shell height - meat weight regional comparison');
  points(MW ~ SH, data=scalE, col='red', pch=3, cex=1.25);
  points(MW ~ SH, data=scalW, col='blue', pch=3, cex=1.25);
  
  # include the prediction lines using the lines() function
  # For this to look correct the x values (SHseq) must be
  # in order (which they are).  Increase the size of the
  # line a little by using lwd=2.
  lines(predE ~ SHseq, col='red', lwd=2);           # SKILL 57
  lines(predW ~ SHseq, col='blue', lwd=2);
  
  # add a legend.  line width (lwd) 2 indicates lines 
  # should be plotted in the legend (and be thicker than the
  # default of lwd=1. cex 1.25 increases the legend size.
  legend('topleft',
         legend = c('East model', 'West model'),
         lwd = 2,
         col = c('red', 'blue'),
         cex = 1.25);

}
