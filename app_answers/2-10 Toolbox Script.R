{
  pythagoras = function(a,b)
  {
    c = (a^2 + b^2)^(1/2);
    return(c); 
  }
   
  counter = function(vector, compareVal, conditionalOp=">")
  {
    vecLength = length(vector);  # get the length of the vector
    countVal = 0;                # initialize the count to 0
    for(val in 1:vecLength)      # go through each value in vector
    {
      # if the conditional operator is not equal to any of
      # the six valid operators 
      if (conditionalOp != ">" && conditionalOp != "<" &&
           conditionalOp != ">=" && conditionalOp != "<=" &&
           conditionalOp != "==" && conditionalOp != "!=")
      {
        cat("Error: Invalid conditional operator given");
        # since it is not one of the 6 values, there is no point
        # in checking any other part of the if-else-if structure.
      }
      else if(conditionalOp == ">" && vector[val] > compareVal)
      {
        countVal = countVal + 1; # add one to the count
      }
      else if(conditionalOp == "<" && vector[val] < compareVal)
      {
        countVal = countVal + 1; # add one to the count
      }
      else if(conditionalOp == "==" && vector[val] == compareVal)
      {
        countVal = countVal + 1; # add one to the count
      }
      else if(conditionalOp == ">=" && vector[val] >= compareVal)
      {
        countVal = countVal + 1; # add one to the count
      }
      else if(conditionalOp == "<=" && vector[val] <= compareVal)
      {
        countVal = countVal + 1; # add one to the count
      }
      else if(conditionalOp == "!=" && vector[val] != compareVal)
      {
        countVal = countVal + 1; # add one to the count
      }
      # else  # attempt to do an error condition
      # {
      #   This would not work as an error condition because it would also
      #   capture all the times that the caller used a valid conditional
      #   operator but the comparison was FALSE.  Instead, you need to be
      #   explicit as is done in the if() above.
      # }
    }
    return(countVal);    # return the count value to the caller
  }

  checkValue = function(vector, compareVal, conditionalOp=">")
  {
    vecLength = length(vector);  # get the length of the vector
    checkPassed = FALSE;         # state variable set to FALSE initially
    for(val in 1:vecLength)      # go through each value in vector
    {
      if (conditionalOp != ">" || conditionalOp != "<" ||
          conditionalOp != ">=" || conditionalOp != "<=" ||
          conditionalOp != "==" || conditionalOp != "!=")
      {
        # conditional operator is not one of the 6 values above
        cat("Error: Invalid conditional operator given");
        # since it is not one of the 6 values, there is no point
        # in checking any other part of the if-else-if structure.
      }
      else if(conditionalOp == ">" && vector[val] > compareVal)
      {
        checkPassed = TRUE; 
      }
      else if(conditionalOp == "<" && vector[val] < compareVal)
      {
        checkPassed = TRUE; 
      }
      else if(conditionalOp == "==" && vector[val] == compareVal)
      {
        checkPassed = TRUE; 
      }
      else if(conditionalOp == ">=" && vector[val] >= compareVal)
      {
        checkPassed = TRUE; 
      }
      else if(conditionalOp == "<=" && vector[val] <= compareVal)
      {
        checkPassed = TRUE; 
      }
      else if(conditionalOp == "!=" && vector[val] != compareVal)
      {
        checkPassed = TRUE; 
      }
    }
    return(checkPassed);    # return the checkPassed Boolean to the caller
  }
}
