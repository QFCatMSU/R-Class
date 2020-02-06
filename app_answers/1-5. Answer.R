{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  # get location from user - assign  to variable location
  location = readline("What is your location ?");
  
  # get the first temperatures from user- assign to variable temp1
  temp1 = readline("What is the 1st temperature? ");
  # tell R to treat temp1 as a numeric value
  temp1 = as.numeric(temp1);
  
  # do the same for the other 4 temperature values
  temp2 = readline("What is the 2nd temperature? ");
  temp2 = as.numeric(temp2);
  temp3 = readline("What is the 3rd temperature? ");
  temp3 = as.numeric(temp3);
  temp4 = readline("What is the 4th temperature? ");
  temp4 = as.numeric(temp4);
  temp5 = readline("What is the 5th temperature? ");
  temp5 = as.numeric(temp5);
  
  # add up the 5 temperatures, save to totalTemp
  totalTemp = temp1+temp2+temp3+temp4+temp5;
  
  # divide the total temperature by 5 to get the mean temp
  meanTemp = totalTemp/5;
}