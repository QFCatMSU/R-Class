{
  rm(list=ls());  options(show.error.locations = TRUE);  
  
  ### Part A
  vector1 = 45:167; # could also be: seq(from=45, to=167, by=1)
  vector2 = 25:-7;  # could also be: seq(from=25, to=7, by=-1)
  
  ### Part B
  # This sequence will create the vector: 17,14,11,8,5
  seq1 = seq(from=17, to=5, by=-3);
  
  # The for() will iterate 5 times, for each value in the seq1 vector
  for(i in seq1)  # i will iteratively take the values 17,14,11,8,5
  {
    cat("\n",i, "cubed is", i^3);
    cat("\n",i, "cube rooted is", i^(1/3));
  }
  
  ### Part C
  # save the data from LansingWeather2.csv to the data frame weatherData
  weatherData = read.csv("data/LansingWeather2.csv", stringsAsFactors = FALSE);
  
  # create a manual sequence with the values: 13,3,5,8,7
  manualSeq = c(13,3,5,8,7);
  
  # use manualSeq to iterate through a for() and index weatherData
  for(i in manualSeq)  # i will iteratively take the values 13,3,5,8,7
  {
    cat("\nOn",weatherData[i,"date"], "the precipiatation was", weatherData[i,4]);
  }
  
  ### Part D
  # Go through all 14 values in the weatherData precipitation column
  for(i in 1:14)
  {
    # did it rain more than one inch on this day
    if(weatherData[i,"precipitation"] > 1) 
    {
      # output the date that it rained
      cat("\nOn", weatherData[i,"date"], "it rained more than one inch"); 
    }
  }
}
