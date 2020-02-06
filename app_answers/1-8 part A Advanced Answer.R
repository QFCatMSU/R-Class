{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  # Get the temperature from the user
  userTemp = readline("What was the temperature? ");
  userTemp = as.numeric(userTemp);

  # The answer here uses the information taught in lesson 1-9.
  # It is a more robust answer in that every condition here is
  # truly mutually exclusive.  In other words, there is not a temperature
  # that meets more than one of the conditions below.
  
  # check if temp is greater than -20 but less than 30
  if(userTemp > -20 && userTemp < 30)
  {
    cat("Cold!");
  }
  # check if temp is equal to 30 
  else if(userTemp == 30)
  {
    cat("Cold-ish");
  }
  # check if temp is greater than 30 but less than 60 
  else if(userTemp > 30 && userTemp < 60)
  {
    cat("Warm");
  }
  # check if temp is equal to 60
  else if(userTemp == 60)
  {
    cat("Hot-ish");
  } 
  # check if temp is greater than 60 but less than or equal to 100
  else if(userTemp > 60 && userTemp <= 100)
  {
    cat("Hot");
  } 
  # all temperature not covered by above conditions (so temps > 100)
  else
  {
    cat("Invalid Temperature")
  }
  
}