{
  rm(list=ls());                        # clean out the environment
  options(show.error.locations = TRUE); # give line numers on errors

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
  sum3 = (length3 - meanlength^2;
  sum4 = (length4 - meanlength)^2;
  sum5 = (length5 - meanlength)^2;
  sumTotal = sum1 + sum2+ sum3 + sum4 + sum5;
  variance = sumTotal /5;
  
  # solving for the standard deviation (square root of the variance)
  stanDev = variance^(1/2);
  
  # line 19 gives an error: unexpected ';'
  # because R was expecting an end parenthesis before the end-of-command semicolon
  
  # If you did not use semicolon in your code the error would be: unexpected symbol -- pointing to sum4 on line 20
  # Take out the semicolons and try the code to see this error -- then go back to using semicolons!
}