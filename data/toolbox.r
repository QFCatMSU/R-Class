{
  myToken = "put your 32 character token here"
  
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
      if(conditionalOp == ">" && vector[val] > compareVal)
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
    }
    return(countVal);    # return the count value to the caller
  }
  
  timeConvert = function(hoursMinutes)
  {
    returnVector = c();
    for(i in 1:length(hoursMinutes))
    {
      numDigits = nchar(as.character(hoursMinutes[i]));
      minutes = substr(hoursMinutes[i], numDigits-1, numDigits);
      minutes = as.numeric(minutes)*(1/60);
      hours = floor(hoursMinutes[i]/100);
      returnVector[i] = round(hours+minutes, 2);
      
    }
    return(returnVector)
  }
}
