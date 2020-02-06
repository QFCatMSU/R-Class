{
  rm(list=ls());                        # clean out the environment
  options(show.error.locations = TRUE); # give line numers on errors
  
  # the five lengths (in centimeters)
  length1 = 25;
  length2 = 26;
  length3 = 20;
  length4 = 22;
  length5 = 32;
  
  # solving for the mean (add the five lengths and divide by 5)
  meanlength = (length1 + length2 + length3 + length4 + length5)/5;
  
  # solving for the variance 
  variance = ((length1 - meanlength)^2 + (length2 - meanlength)^2 + (length3 - meanlength)^2 + (length4 - meanlength)^2 + (length5 - meanlength)^2) /5;
  
  # solving for the standard deviation (square root of the variance)
  stanDev = variance^(1/2);
}