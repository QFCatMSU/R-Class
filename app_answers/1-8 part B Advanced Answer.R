{
  rm(list=ls()); options(show.error.locations = TRUE);  

  # Get the grade from the user
  userGrade = readline("What was the grade (A-E)? ");

  # This script uses the information taught in lesson 1-9
  # It merges conditions that have the same codeblock (like "a" and "A")
  # using the OR conditional operator
  
  # check if the grade is "A" or "a"
  if(userGrade == "A" || userGrade == "a")
  {
    cat("90-100");
  }
  # check if the grade is "B" or "b"
  else if(userGrade == "B" || userGrade == "b")
  {
    cat("80-89");
  }
  # check if the grade is "C" or "c"
  else if(userGrade == "C" || userGrade == "c")
  {
    cat("70-79");
  }
  # check if the grade is "D" or "d"
  else if(userGrade == "D" || userGrade == "d")
  {
    cat("60-69");
  }
  # check if the grade is "E" oe "e"
  else if(userGrade == "E" || userGrade == "e")
  {
    cat("0-59");
  }
  # All the above conditions were FALSE
  else
  {
    cat("Invalid grade entered"); 
  }
}