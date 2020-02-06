{
  rm(list=ls()); options(show.error.locations = TRUE);  

  # Get the grade from the user
  userGrade = readline("What was the grade ("A"-"E")? ");
  
  # line 9 causes an unexpected symbol error because R believes
  # the first double quote before the A is an operator that ends 
  # a string output.  However, you just want to output the double quote
  # and not treat is an an operator
  
  # You can make line 9 work by changing it to:
  # userGrade = readline("What was the grade (\"A\"-\"E\")? ");
  
  # The backslash (\) is called an escape character and it tells
  # R to treat the next character as output instead of an operation

  # check if the grade is "A"
  if(userGrade == "A")
  {
    cat("90-100");
  }
  # check if the grade is "a"
  else if(userGrade == "a")
  {
    cat("90-100");
  }
  # check if the grade is "B"
  else if(userGrade == "B")
  {
    cat("80-89");
  }
  # check if the grade is "b"
  else if(userGrade == "b")
  {
    cat("80-89");
  }
  # check if the grade is "C"
  else if(userGrade == "C")
  {
    cat("70-79");
  }
  # check if the grade is "c"
  else if(userGrade == "c")
  {
    cat("70-79");
  }
  # check if the grade is "D"
  else if(userGrade == "D")
  {
    cat("60-69");
  }
  # check if the grade is "d"
  else if(userGrade == "d")
  {
    cat("60-69");
  }
  # check if the grade is "E"
  else if(userGrade == "E")
  {
    cat("0-59");
  }
  # check if the grade is "e"
  else if(userGrade == "e")
  {
    cat("0-59");
  }
  # All the above conditions were FALSE
  else
  {
    cat("Invalid grade entered"); 
  }
}