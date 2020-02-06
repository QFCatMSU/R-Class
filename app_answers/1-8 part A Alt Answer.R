{
  rm(list=ls()); options(show.error.locations = TRUE);  

  # Get the temperature from the user
  userTemp = readline("What was the temperature? ");
  userTemp = as.numeric(userTemp);

  # The first answer went in ascending order -- we can also
  # go in descending order
  
  # check if temp is greater than 100
  if(userTemp > 100)
  {
    cat("Invalid Temperature -- too high");
  }
  # check if temp is greater than 60 (but temps > 100 have already been excluded by the above condition)
  else if(userTemp > 60)
  {
    cat("Hot");
  }
  # check if temp is equal to 60 
  else if(userTemp == 60)
  {
    cat("Hot-ish");
  }
  # check if temp is greater than 30 (but temps >= 60 have already been excluded by the above conditions)
  else if(userTemp > 30)
  {
    cat("Warm");
  }
  # check if temp is equal to 30
  else if(userTemp == 30)
  {
    cat("Cold-ish");
  } 
  # check if temp is greater than or equal to -20 (but temps >= 30 have already been excluded by the above conditions)
  else if(userTemp > -20)
  {
    cat("Cold!");
  } 
  # all temperature not covered by above conditions (so temps <= -20)
  else
  {
    cat("Invalid Temperature -- too cold")
  }
  
  # Once again, in an if-else-if structure, the first codeblock
  # attached to a TRUE condition gets executed.  Once a codeblock 
  # is executed, the program exits the if-else-if structure even if
  # there are codeblock below that are attached to a TRUE condition
  
  # so if the temperature is 50
  # the codeblock attached to (userTemp > -20) will NOT be executed
  # because the above codeblock attached to (userTemp > 30) has 
  # already been executed
}