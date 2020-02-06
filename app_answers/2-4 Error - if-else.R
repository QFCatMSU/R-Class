{
  rm(list=ls()); options(show.error.locations = TRUE);
  
  # Boolean values used in for() -- initialize to FALSE
  greaterThan100 = FALSE;
  lessThan70 = FALSE;
  equalTo78 = FALSE;
  equalTo87 = FALSE;
  
  ### Set up vector and vector length
  myVector = c(94, 102, 89, 105, 78, 85);
  lengthVector = length(myVector);
  
  ### State variables
  minValue = myVector[1]; # initialize to the first value in the vector
  maxValue = myVector[1]; # initialize to the first value in the vector
  sumValue = 0            # initialize to 0
  
  # go through each value in the vector
  for(i in 1:lengthVector)
  {
    # if the current value is greater than the max -- make this value the max
    if(myVector[i] > maxValue)
    {
      maxValue = myVector[i];
    }
    
    # if the current value is less than the min -- make this value the min
    else if(myVector[i] < minValue)
    {
      minValue = myVector[i];
    }
    # check the value against all the Boolean conditions
    else if(myVector[i] > 100)
    {
      greaterThan100 = TRUE;
    }
    else if(myVector[i] < 70)
    {
      lessThan70 = TRUE;
    }
    else if(myVector[i] == 78)
    {
      equalTo78 = TRUE;
    }
    else if(myVector[i] == 87)
    {
      equalTo87 = TRUE;
    }
    
    # add the current value to the sum of all the values
    sumValue = sumValue + myVector[i];
  }
  
  # mean is the sum divided by the number of values
  meanValue = sumValue/lengthVector;
}