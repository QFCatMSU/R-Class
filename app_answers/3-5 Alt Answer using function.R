{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  lansingWeather = read.csv(file = "data/formattedLansingWeather.csv"); 
  windSpeed = lansingWeather[,"AWND"];
  precip = lansingWeather[,"PRCP"];
  
  #### Part 1
  # take 50 random values from the wind vector -- allow for repeats
  randomWindVal = sample( x=windSpeed, size=50, replace=TRUE);
  
  # take 30 random values from the precipitation vector -- don't allow repeats
  randomPrecipVal = sample( x=precip, size=30, replace=FALSE);
  
  #### Part 2 and 3
  ## defaults to a sample size of 20 and no seed value if the user does not provide these values
  checkDistribution = function(data, sampleSize=20, seed=NULL)
  {
    set.seed(seed);  # set the seed (NULL means a seed will not be used)

    # create the distribution from the input data and sample size
    dist = rnorm(n=sampleSize, mean=mean(data), sd=sd(data));
    
    # create a histogram of the distribution
    hist(x=dist, col=c("blue", "green"));
    abline(v=mean(dist), col="red", lty=4, lwd=3);
  }
  
  # execute the functin for different sample sizes and seed values
  checkDistribution(data=windSpeed);   # default sample size for function is 20
  checkDistribution(data=windSpeed, sampleSize=50);
  checkDistribution(data=windSpeed, sampleSize=200);
  
  checkDistribution(data=windSpeed, seed=15);
  checkDistribution(data=windSpeed, sampleSize=50, seed=15);
  checkDistribution(data=windSpeed, sampleSize=200, seed=15);
}