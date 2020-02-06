{
  rm(list=ls()); options(show.error.locations = TRUE);  

  ### Part A
  # read in the data from LAnsingWeather2.csv and save it to the data frame weatherData
  weatherData = read.csv("data/LansingWeather2.csv");
  
  ### Part B
  # output the 3rd and 10th values from the 4th column
  # inother words, output the precipitation for the 3rd and 10th days in weatherData
  cat("\nThe precipitation for day three is:", weatherData[3,4]);
  cat("\nThe precipitation for day ten is:", weatherData[10,4]);
  
  # The following commented lines are functionally equivalent to the previous 2 lines
  # cat("\nThe precipitation for day three is:", weatherData[3,"precipitation"]);
  # cat("\nThe precipitation for day ten is:", weatherData[10,"precipitation"]); 
  
  ### Part C
  # save every value from the 4th column of data frame weatherData to the vector precipData
  precipData = weatherData[, 4];   # could also be precipData = weatherData[,"precipitation"]
  
  ### Part D
  # Output the 5th and 13th values from precipData (i.e., the 5th and 13th days' precipitation values)
  cat("\nThe precipitation on the third day was", precipData[5]);
  cat("\nThe precipitation on the tenth day was", precipData[13]);

  ### Part E
  # Save the data from the highTemp and lowTemp columns in weatherData to the vectors highTemp and lowTemp
  highTemp = weatherData[,2];
  lowTemp = weatherData[,3];
  # Subtract the 4th and 6th days' lowTemp from the highTemp, save the answer to a variable
  day4Diff = highTemp[4] - lowTemp[4];
  day6Diff = highTemp[6] - lowTemp[6];
}

  