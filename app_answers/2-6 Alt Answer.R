{
  rm(list=ls());  options(show.error.locations = TRUE); 
  
  # saving data from CSV to a data frame
  weatherData = read.csv("data/LansingWeather2.csv");
  
  # save columns by number from the data frame to vectors
  highTempData = weatherData[ ,2];   # highTemp column
  lowTempData = weatherData[ ,3];    # lowTemp column
  precipData = weatherData[ ,4];     # precip column
  
  # new vectors 
  changeInTemp = c(); #use to declare change in temp vector
  avgHumidity = c(88,87,80,74,67,84,89,59,55,72,82,83,87,58); #use to declare avg humidity vector

  # subtract low temp from high temp to get change in temp for all 14 days
  changeInTemp = highTempData - lowTempData; 
  
  # add the vectors changeInTemp and avgHumidity to the weatherData data frame
  weatherData[, 5] = changeInTemp; # put in column 5
  weatherData[, 6] = avgHumidity;  # put in column 6
  
  ##Rearranging the data frame -- Part B
  newWeatherData = weatherData[ ,c(1,3,6,2,5,4)];
  
  #Removing a column (same as excluding it) -- Part C
  newWeatherData = weatherData[ ,c(1,3,4,5,6)];
  
  ##saving the data frame to a csv file -- Part D
  write.csv(x=newWeatherData, file="data/newLansingWeather.csv");
  
}