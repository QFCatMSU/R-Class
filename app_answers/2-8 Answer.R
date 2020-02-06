{
   rm(list=ls());  options(show.error.locations = TRUE); 
 
   #### ConvertWeight takes three values: 
   #     value: the value to be converted
   #     fromUnit: the unit of the value
   #     toUnit: the unit to convert the value to
   convertWeight = function(value, fromUnit, toUnit)  
   {
      if(fromUnit == "kg" && toUnit == "g")
      {
         convertedVal = value * 1000;  
      }
      else if(fromUnit == "g" && toUnit == "kg")
      {
         convertedVal = value / 1000;  
      }
      else if(fromUnit == "lb" && toUnit == "g")
      {
         convertedVal = value * 454;  
      }
      else if(fromUnit == "g" && toUnit == "lb")
      {
         convertedVal = value / 454;  
      }
      else if(fromUnit == "lb" && toUnit == "kg")
      {
         convertedVal = value * .454;  
      }
      else if(fromUnit == "kg" && toUnit == "lb")
      {
         convertedVal = value / .454;  
      }
      else
      {
         cat("Sorry, this function cannot handle that conversion");
         convertedVal = NULL;
      }
      
      return(convertedVal);
   }
   
   tempDifferences = function(tempVector)
   {
      diffVector = c();
      
      ## Start with the second value because there is no "0th" value
      #  to subtract from the first value -- this also means
      #  the return vector will have one fewer value
      for(i in 2:length(tempVector))
      {
         diffVector[i-1] = tempVector[i] - tempVector[i-1];
      } 
      
      return(diffVector);
   }
   
   ### Testing the functions
   ans1 = convertWeight(100, "kg", "g");
   ans2 = convertWeight(100, "g", "kg");
   ans3 = convertWeight(100, "lb", "g");
   ans4 = convertWeight(100, "g", "lb");
   ans5 = convertWeight(100, "lb", "kg");
   ans6 = convertWeight(100, "kg", "lb");
   
   ans7 = tempDifferences(c(40,45,35,42));
}