{
   rm(list=ls());  options(show.error.locations = TRUE); 
 
   #### ConvertWeight takes three values: 
   #     value: the value to be converted
   #     fromUnit: the unit of the value
   #     toUnit: the unit to convert the value to
   
   ### This answer is a little more robust as it checks the fromUnit and toUnits
   #   separately and give errors that are more specific
   convertWeight = function(value, fromUnit, toUnit)  
   {
      if(fromUnit == "kg")
      {
         if(toUnit == "g")
         {
            convertedVal = value * 1000;  
         }
         else if(toUnit == "lb")
         {
            convertedVal = value / .454;
         }
         else
         {
            cat("Sorry, this function cannot handle conversions to ", toUnit);
            convertedVal = NULL;
         }
      }
      else if(fromUnit == "g")
      {
         if(toUnit == "kg")
         {
            convertedVal = value / 1000;  
         }
         else if(toUnit == "lb")
         {
            convertedVal = value / 454;
         }
         else
         {
            cat("Sorry, this function cannot handle conversions to ", toUnit);
            convertedVal = NULL;
         }
      }
      else if(fromUnit == "lb")
      {
         if(toUnit == "kg")
         {
            convertedVal = value * 0.454;  
         }
         else if(toUnit == "g")
         {
            convertedVal = value * 454;
         }
         else
         {
            cat("Sorry, this function cannot handle conversions to ", toUnit);
            convertedVal = NULL;
         }
      }
      else
      {
         cat("Sorry, this function cannot handle conversions from ", fromUnit);
         convertedVal = NULL;
      }
      
      return(convertedVal);
   }
   

   tempDifferences = function(tempVector)
   {
      diffVector = c();
      
      ### This answer is more explicit about what is happening to the first
      #   iteration in the for loop, which gets ignored because there is no
      #   value to subtract the first value from (i.e., there is no 0th value)
      for(i in 1:length(tempVector))
      {
         if(i != 1)  # do for all values except the 1st
         {
            diffVector[i-1] = tempVector[i] - tempVector[i-1];
         }
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