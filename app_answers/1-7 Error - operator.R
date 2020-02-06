{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  # Part A: get high temperature and weather condition from the user
  highTemp = readline("What was the high temperature today? ");
  weatherCond = readline("What was the weather like (cloudy, sunny, or rainy)? ");
  
  # Part B: give message if temperatures was less than 30
  if(highTemp < 30)
  {
    cat("The high temperature was less than 30.\n");
  }

  # Part C: give message if temperatures is greater than or equal to 80
  if(highTemp >= 80)
  {
    cat("The high temperature was greater than or equal to 80.\n");
  }
  
  # Part D: give message if weather is cloudy
  if(weatherCond = "cloudy")
  {
    cat("It was a cloudy day.\n");
  }

  # Part E: give message if weather was not rainy
  if(weatherCond != "rainy")
  {
    cat("It did not rain today.\n");
  }
  
  # line 26 uses the assignment operator ( = ) instead of the conditional operator ( == )
  # R gives the error "unexpected '='
  # You are not assigning "cloudy" to weatherCond, you are comparing weatherCond to "cloudy"
}