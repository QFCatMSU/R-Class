{
  rm(list=ls()); options(show.error.locations = TRUE);

  # read data from lansingWeather2.csv and save to the variable weatherData...
  weatherData = read.csv("data/lansingWeather2.csv");
  
  # get all values from the highTemp column, which is the second column in weatherData
  highTemp = weatherData[, 2]; 

  # declare a vector that will hold the difference in daily high temperatures
  diffInHighTemp = c(); 
 
  # go through the sequence 1:14 
  for(i in 1:14)
  {
    if(i == 1)
    {
      # no value to subtract so we say the value is NA
      diffInHighTemp[i] = NA;
    }
    else
    {
      # subtract highTemp of this day from highTemp of previous day
      diffInHighTemp[i] = highTemp[i] - highTemp[i-1];
    }
  } 
  
  # add diffInHighTemp to the data frame
  weatherData[,"HighTempDiff"] = diffInHighTemp;
}