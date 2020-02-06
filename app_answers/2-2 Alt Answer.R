{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  ### Part A
  # read in the data from LAnsingWeather2.csv and save it to the data frame weatherData
  weatherData = read.csv("data/LansingWeather2.csv");
  
  ### Part B
  # output the 3rd and 10th values from the 4th column (precipitation)
  # inother words, output the precipitation for the 3rd and 10th days in weatherData
  cat("\nThe precipitation for day three is:", weatherData[3,"precipitation"]);
  cat("\nThe precipitation for day ten is:", weatherData[10,"precipitation"]);
  

  ### Part C
  # save every value from the precipitation column of data frame weatherData to the vector precipData
  precipData = weatherData[, "precipitation"];  
  
  ### Part D
  # Output the 5th and 13th values from precipData (i.e., the 5th and 13th days' precipitation values)
  cat("\nThe precipitation on the third day was", precipData[5]);
  cat("\nThe precipitation on the tenth day was", precipData[13]);

  ### Part E
  # Subtract the 4th and 6th days' lowTemp column value from the highTemp column value 
  #  -- save answers to variables
  day4Diff = weatherData[4,2] - weatherData[4,3];
  day6Diff = weatherData[6,2] - weatherData[6,3];
  
  # Equivalently:
  # day4Diff = weatherData[4,"highTemp"] - weatherData[4,"lowTemp"];
  # day6Diff = weatherData[6,"highTemp"] - weatherData[6,"lowTemp"];
}


  