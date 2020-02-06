{
  rm(list=ls()); options(show.error.locations = TRUE);  

  # Get the temperature from the user
  userTemp = readline("What was the temperature? ");

  # We have forgotten to "convert" the input from the user 
  # into a number -- userTemp = as.numeric(userTemp);
  # This code will run but with unexpected results!
  # Try the temperature 120.
  
  # check if temp is less than or equal to -20
  if(userTemp <= -20)
  {
    cat("Invalid Temperature -- too low");
  }
  # check if temp is less than 30 (but temps <= -20 have already been excluded by the above condition)
  else if(userTemp < 30)
  {
    cat("Cold!");
  }
  # check if temp is equal to 30 
  else if(userTemp == 30)
  {
    cat("Cold-ish");
  }
  # check if temp is less than 60 (but temps <= 30 have already been excluded by the above conditions)
  else if(userTemp < 60)
  {
    cat("Warm");
  }
  # check if temp is equal to 60
  else if(userTemp == 60)
  {
    cat("Hot-ish");
  } 
  # check if temp is less than or equal to 100 (but temps <= 60 have already been excluded by the above conditions)
  else if(userTemp <= 100)
  {
    cat("Hot");
  } 
  # all temperature not covered by above conditions (so temps > 100)
  else
  {
    cat("Invalid Temperature -- too high")
  }
  
  # The tempertuare 120 gives the answer "Cold!"
  # In other words, (userTemp < 30) was TRUE
  # This is because the numbers 120 is treated as a string
  # so "120" is less than "30" because the first character
  # "1" is less than "3"
}