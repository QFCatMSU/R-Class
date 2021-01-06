## We want to find the relationship between
## length at time t and length at time t+1 for
## a (simulated) population (e.g., Walford plot).
## This requires a linear model of length(t)
## versus length(t+1).  We will fit the model
## with length(t) as the independent variable
## and length(t+1) as the dependent variable.

#prompt user for csv file, read in the data from the csv file
gdat <- read.csv(file="data/Intro-linearModel.csv");

#Look at part of the data
print(head(gdat))

#Fit the linear model
wmod <- lm(L1 ~ L0, data=gdat)

#look at a summary of the results
print(summary(wmod))

#extract the slope and intercept
coefs <- coef(wmod)

#Make a plot of the data with the model
plot(L1 ~ L0, data=gdat, 
     ylab="Length at time t+1", 
     xlab="Length at time t")
abline(wmod) #the model

#Get the residuals
resid <- wmod$residuals

#check to see if any of the residuals are unusually large
if(any(abs(resid) > 40))
{
  warning("Large residuals")  #output a warning to the user
}

#histogram of the residuals
hist(resid, main="Residuals")
