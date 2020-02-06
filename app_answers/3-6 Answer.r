{
  rm(list=ls()); options(show.error.locations = TRUE);  
  source("scripts/toolbox.r");
 
  ### Make sure you have included the timeConvert function in toolbox.r
  # timeConvert = function(hoursMinutes)
  # {
  #   returnVector = c();
  #   for(i in 1:length(hoursMinutes))
  #   {
  #     numDigits = nchar(as.character(hoursMinutes[i]));
  #     minutes = substr(hoursMinutes[i], numDigits-1, numDigits);
  #     minutes = as.numeric(minutes)*(1/60);
  #     hours = floor(hoursMinutes[i]/100);
  #     returnVector[i] = round(hours+minutes, 2);
  #   }
  #   return(returnVector)
  # }
  
  # Get the reformatted weather data for 2016 in Lansing, MI -- treat strings as strings
  lansing2016Weather = read.csv(file="data/LansingNOAA2016Formatted.csv",
                                stringsAsFactors = FALSE);
  
  
  # Extract the various parts of the weather data and save each to a vector
  date = lansing2016Weather[,"date"];
  eventData = lansing2016Weather[,"eventData"];
  avgTemp = lansing2016Weather[,"avgTemp"];
  tempDept = lansing2016Weather[,"tempDept"]; 
  precipitation = lansing2016Weather[,"precipitation"]; 
  humidity = lansing2016Weather[,"humidity"];
  barometer = lansing2016Weather[,"barometer"];
  dewPoint = lansing2016Weather[,"dewPoint"];
  avgWind = lansing2016Weather[,"avgWind"];
  maxWind = lansing2016Weather[,"maxWind"]; 
  windDirection = lansing2016Weather[,"windDirection"];
  sunrise = lansing2016Weather[,"sunrise"];
  sunset = lansing2016Weather[,"sunset"];
  
  ### Convert sunrise and sunset times to hours
  sunrise = timeConvert(sunrise);
  sunset = timeConvert(sunset);
  
  ### Create a new vector called hoursOfSun:
  hoursOfSun = sunset - sunrise;
  
  # create a scatterplot looking at hoursOFSun vs multiple variable
  # This is not as useful in this circumstance because we want to see more detail
  pairs(formula=hoursOfSun~avgTemp+humidity+precipitation);  
  

  #### focus on hoursOfSun vs dewPoint a
  plot(formula=hoursOfSun~dewPoint);
  model = lm(hoursOfSun~dewPoint);
   
  # do a summary of the linear regression model -- shows definite relationship
  print(summary(model)); 
  
  # add the regression line to the plot
  abline(model, col="blue");
  
  #### focus on hoursOfSun vs humidity
  plot(formula=hoursOfSun~humidity);
  model2 = lm(hoursOfSun~humidity);
  
  # do a summary of the linear regression model -- shows definite relationship
  print(summary(model2)); 
  
  # add the regression line to the plot
  abline(model2, col="blue");
  
  #### focus on hoursOfSun vs precipitation
  plot(formula=hoursOfSun~precipitation);
  model3 = lm(hoursOfSun~precipitation);
  
  # do a summary of the linear regression model -- shows definite relationship
  print(summary(model3)); 
  
  # add the regression line to the plot
  abline(model3, col="blue");
  
  #### focus on hoursOfSun vs windDirection
  plot(formula=hoursOfSun~windDirection);
  model4 = lm(hoursOfSun~windDirection);
  
  # do a summary of the linear regression model -- shows definite relationship
  print(summary(model4)); 
  
  # add the regression line to the plot
  abline(model4, col="blue");
  
}