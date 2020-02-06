{
  rm(list=ls()); options(show.error.locations = TRUE);  

  # get location from user - assign  to location
  location = readline("What is your location? ");
  
  # get 5 temperatures from user and declare the temperatures as numeric
  temp1 = readline("What is the 1st temperature? ");  temp1 = as.numeric(temp1);
  temp2 = readline("What is the 2nd temperature? ");  temp2 = as.numeric(temp2);
  temp3 = readline("What is the 3rd temperature? ");  temp3 = as.numeric(temp3);
  temp4 = readline("What is the 4th temperature? ");  temp4 = as.numeric(temp4);
  temp5 = readline("What is the 5th temperature? ");  temp5 = as.numeric(temp5);

  # add up the 5 temperatures, 
  totalTemp = temp1+temp2+temp3+temp4+temp5;
  # divide the total temperature by 5 to get the mean temp
  meanTemp = totalTemp/5;
  
  # Using just one cat() and adding in the paramter sep to take out the extra space.
  cat("The location is: ", location, "\n", 
      "The five temperatures are: ", 
           temp1, ", ", temp2, ", ", temp3, ", ", temp4, ", and ", temp5, "\n",
      "The mean temperature is: ", meanTemp, sep = "");
}