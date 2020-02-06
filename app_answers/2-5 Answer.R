{
  rm(list=ls()); options(show.error.locations = TRUE);
  
  # vector that will hold the difference in temperature values
  diffInHighTemp = c();  

  # read data from lansingWeather2.csv and save to the variable weatherData...
  weatherData = read.csv("data/lansingWeather2.csv");
  
  # get all values from the highTemp column in weatherData
  highTemp = weatherData[, "highTemp"]; 

  # get the number of values in the highTemp vector
  vectorLength = length(highTemp); 
 
  # the first value cannot be calculated because we don't have the previous day's high temp
  # So, we will say the value is NA
  diffInHighTemp[1] = NA;
  
  # go through the sequence 2:14 (already have the first value)
  for(i in 2:vectorLength)
  {
    # subtract highTemp of this day from highTemp of previous day
    diffInHighTemp[i] = highTemp[i] - highTemp[i-1];
  } 
  
  # add diffInHighTemp to the data frame
  weatherData[,"HighTempDiff"] = diffInHighTemp;
}