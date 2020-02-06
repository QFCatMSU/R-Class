{
  rm(list=ls()); options(show.error.locations = TRUE);

  weatherData = read.csv("data/LansingWeather3.csv");
  
  precipData = weatherData[,5];
  humidityData = weatherData[,6];
  highTempData = weatherData[ ,3];
  lowTempData = weatherData[,4];
  dateData = weatherData[ ,"dayOfYear"];
  
  # Plot precipitation vs. humidity
  plot(formula=humidityData~precipData, 
       type = "p", col = "darkgreen", main = "Precipitation vs Humidity", 
       xlab = "precipitation (inches)", ylab = "Humidity (%)" );

  # need to declare changeInTemp as a vector
  changeInTemp = c();
  
  # Add values to the change in temp vector
  for(i in 1:length(highTempData))
  {
    changeInTemp[i] = highTempData[i] - lowTempData[i];
  }
  
  # Add change in temp top the data frame
  weatherData[, "change in temp"] = changeInTemp;
  
  # Plot the high temperature data
  plot(highTempData~dateData, type = "o", col = "red",
        main="Lansing high temperatures", 
        xlab="Day of Year", 
        ylab="temperature (F)",
        ylim=c(0,80));    # change y-axis limits
  
  # add the low temperature data to the plot
  points(lowTempData~dateData, 
         type = "o", col = "blue");
  
  # add the change in temp data to the plot
  points(changeInTemp~dateData, 
         type = "o", col = "purple");
  
  # put a legend on the plot
  legend(x="topleft",                   # position
          legend=c("High Temp", "Low Temp", "Change in Temp"), # labels
          lty=c(1,1,1),                    # line type
          pch=c(1,1,1),                    # point type
          col=c("red","blue", "purple"));  # colors

}
  
  