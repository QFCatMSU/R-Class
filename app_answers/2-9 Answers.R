{
  rm(list=ls()); options(show.error.locations = TRUE);
  
  weatherData = read.csv("data/LansingWeather3.csv");
  highTempData = weatherData[ ,"highTemp"];
  lowTempData = weatherData[ ,"lowTemp"];
  day=length("highTemp")
  dateData = weatherData[, "date"];
  
  humidityData = weatherData[ , "humidity"];
  precipData = weatherData[ ,"precipitation"]; 
  
  ###Part 1
  changeTempData = highTempData - lowTempData; 
  
  hist(changeTempData, labels=TRUE, breaks=8, right=FALSE,     
      xlab="Change in Temp", col=c("purple", "lightblue"),
      main="Change in Temperature over 14 days"); 
  
  meanRange = mean(changeTempData);
  
  abline(v=meanRange, col="red", lty=4); 
  
  ###Part 2
 
  allTempData = c(highTempData, lowTempData);
  
  barplot(allTempData,
      main = "High and Low Temperature Over 14 Days",
      xlab = "Dates", ylab = "Temperature (F)",
      angle=60, density=30,
      col=c("purple", "black", "orange"));

  meanTemp = mean(allTempData); 
  abline(h=meanTemp, col="dark green", lty=2);
    
  
  
  #Part 3
  boxplot((humidityData/100), precipData,
         names=c("Humidity", "Precipitation"));
        
 
  
  
}