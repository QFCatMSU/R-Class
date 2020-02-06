{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  ### Part A: Get the fish's age, weight, location, and gender from the user
  fishAge = readline("What is the fish's age? ");
  fishLocation = readline("What is the fish's location (north or south)? ");

  # convert the age and weight from strings to numeric values
  fishAge = as.numeric(fishAge);

  ### Part B: categorize fish based on multiple conditions
  # fish from north port and between 3 and 5 years old (inclusive of 3 and 5)
  if( ( fishLocation == "north" )  &&
      ( fishAge >= 3 && fishAge <= 5 ) )  
  {
    cat("Category I\n");
  }
  # fish from south port and between 2 and 6 years old (inclusive of 2 and 6)
  else if( ( fishLocation == "south" )  &&
           ( fishAge >= 2 && fishAge <= 6 ) )
  {
    cat("Category II\n");
  }
  # check for fish from both ports that are too old
  else if (( fishLocation == "north" && fishAge > 5 ) ||
           ( fishLocation == "south" && fishAge > 6 ) )
  {
    cat("Category III\n");
  }
  # check for fish from both ports that are too young
  else if (( fishLocation == "north" && fishAge < 3 ) ||
           ( fishLocation == "south" && fishAge < 2 ) )
  {
    cat("Category IV\n");
  }
  # fish is not from the north or south ports 
  else if ( fishLocation != "south" && fishLocation != "north" )
  {
    cat("Error -- fish location is invalid");
  }
  
  # line 45-47 could have been: 
  # else
  # {
  #   cat("Error -- fish location is invalid");
  # }
  # because all ages of fish from both the north and south are already accounted for.
  # So, all you can have at this point is fish from ports other than north or south.
}