{
  rm(list=ls());  options(show.error.locations = TRUE);

  # To add the third column, precipiation, you need to:
  # add "precipiation" to the end of the header row (don't forget the comma before)
  # add the 14 precipiation values to the end of each row (don't forget the comma before)
  
  # read data from LansingWeather.csv and save to the variable newWeatherData...
  newWeatherData = read.csv("data/LansingWeather.csv");
}