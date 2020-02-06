{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  # get location from user - assign  to location
  location = readline("What is your location ?");
  
  # get 5 temperatures from user,
  # declare the temperatures as numeric
  # assign the value to temp1, temp2...
  # basically, we combined reading in the value and declaring it as a number in one line
  temp1 = as.numeric(readline("What is the 1st temperature? "));
  temp2 = as.numeric(readline("What is the 2nd temperature? "));
  temp3 = as.numeric(readline("What is the 3rd temperature? "));
  temp4 = as.numeric(readline("What is the 4th temperature? "));
  temp5 = as.numeric(readline("What is the 5th temperature? "));

  # add up the 5 temperatures and divide by 5 to get mean (skipping the total step)
  meanTemp = ( temp1+temp2+temp3+temp4+temp5 ) /5;
}