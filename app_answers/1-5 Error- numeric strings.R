{
  rm(list=ls()); options(show.error.locations = TRUE);  

  # get location from user - assign  to location
  location = readline("What is your location ?");
  
  # get 5 temperatures from user, assign to temp1, temp2...
  temp1 = readline("What is the 1st temperature? ");
  temp2 = readline("What is the 2nd temperature? ");
  temp3 = readline("What is the 3rd temperature? ");
  temp4 = readline("What is the 4th temperature? ");
  temp5 = readline("What is the 5th temperature? ");

  # add up the 5 temperatures, 
  totalTemp = temp1+temp2+temp3+temp4+temp5;
  # divide the total temperature by 5 to get the mean temp
  meanTemp = totalTemp/5;
  
  # line 26 will cause the error: non-numeric argument to binary operator
  # This is because the 5 temperatures are still seen as strings instead of numbers
  # And R cannot use the "+" operator on strings
}