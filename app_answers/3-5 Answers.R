{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  lansingWeather = read.csv(file = "data/formattedLansingWeather.csv"); 
  windSpeed = lansingWeather[,"AWND"];
  precip = lansingWeather[,"PRCP"];
  
  # take 50 random values from the wind vector -- allow for repeats
  randomWindVal = sample( x=windSpeed, size=50, replace=TRUE);
  
  # take 30 random values from the precipitation vector -- don't allow repeats
  randomPrecipVal = sample( x=precip, size=30, replace=FALSE);
  
  windDist20 = rnorm(n=20, mean=mean(windSpeed), sd=sd(windSpeed));
  hist(x=windDist20, col=c("blue", "green"));
  abline(v=mean(windDist20), col="red", lty=4, lwd=3);

  windDist50 = rnorm(n=50, mean=mean(windSpeed), sd=sd(windSpeed));
  hist(x=windDist50, col=c("blue", "green"));
  abline(v=mean(windDist50), col="red", lty=4, lwd=3);

  windDist200 = rnorm(n=200, mean=mean(windSpeed), sd=sd(windSpeed));
  hist(x=windDist200, col=c("blue", "green"));
  abline(v=mean(windDist200), col="red", lty=4, lwd=3);
  
  set.seed(seed=15);
  windDist20 = rnorm(n=20, mean=mean(windSpeed), sd=sd(windSpeed));
  hist(x=windDist20, col=c("blue", "green"));
  abline(v=mean(windDist20), col="red", lty=4, lwd=3);
  
  windDist50 = rnorm(n=50, mean=mean(windSpeed), sd=sd(windSpeed));
  hist(x=windDist50, col=c("blue", "green"));
  abline(v=mean(windDist50), col="red", lty=4, lwd=3);
  
  windDist200 = rnorm(n=200, mean=mean(windSpeed), sd=sd(windSpeed));
  hist(x=windDist200, col=c("blue", "green"));
  abline(v=mean(windDist200), col="red", lty=4, lwd=3);
}