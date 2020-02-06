{
  rm(list=ls());                        # clean out the environment
  options(show.error.locations = TRUE); # give line numers on errors

  # In this example, I break down the equations in multiple parts. 
  # The reason to do this is that it makes it much easier to isolate and fix errors
  # The variables used in the intermediate steps show up in the Environment Window.
    
  # the five lengths (in centimeters)
  length1 = 25;
  length2 = 26;
  length3 = 20;
  length4 = 22;
  length5 = 32;
  
  # solving for the mean 
  totallength = length1 + length2 + length3 + length4 + length5;
  meanlength = totallength/5;
  
  # solving for the variance 
  sum1 = (length1 - meanlength)^2;
  sum2 = (length2 - meanlength)^2;
  sum3 = (length3 - meanlength)^2;
  sum4 = (length4 - meanlength)^2;
  sum5 = (length5 - meanlength)^2;
  sumTotal = sum1 + sum2+ sum3 + sum4 + sum5;
  variance = sumTotal /5;
  
  # solving for the standard deviation (square root of the variance)
  stanDev = variance^(1/2);
}