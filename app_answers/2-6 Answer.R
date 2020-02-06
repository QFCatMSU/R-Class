{
  rm(list=ls());  options(show.error.locations = TRUE); 
  
  # saving data from CSV to a data frame
  weatherData = read.csv("data/LansingWeather2.csv");
  
  # save columns by name from the data frame to vectors
  highTempData = weatherData[ ,"highTemp"];
  lowTempData = weatherData[ ,"lowTemp"];
  precipData = weatherData[ ,"precipitation"];
  
  # new vectors 
  changeInTemp = c(); #use to declare change in temp vector
  avgHumidity = c(88,87,80,74,67,84,89,59,55,72,82,83,87,58); #use to declare avg humidity vector

  # subtract low temp from high temp to get change in temp for all 14 days
  for(day in 1:length(highTempData))  
  {
    changeInTemp[day] = highTempData[day] - lowTempData[day]; 
  }
  
  # add the vectors changeInTemp and avgHumidity to the weatherData data frame
  weatherData[, "deltaTemp"] = changeInTemp;
  weatherData[, "Humidity"] = avgHumidity;  
  
  ##Rearranging the data frame -- Part B
  newWeatherData = weatherData[ ,c(1,3,6,2,5,4)];
  
  #Removing a column (same as excluding it) -- Part C
  newWeatherData = weatherData[ ,c(1,3,4,5,6)];
  
  ##saving the data frame to a csv file -- Part D
  write.csv(x=newWeatherData, file="data/newLansingWeather.csv");
  
}