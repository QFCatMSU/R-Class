{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  ### Part A: Get the fish's age, weight, location, and gender from the user
  fishAge = readline("What is the fish's age? ");
  fishWeight = readline("What is the fish's weight? ");
  fishLocation = readline("What is the fish's location (north or south)? ");

  # convert the age and weight from strings to numeric values
  fishAge = as.numeric(fishAge);
  fishWeight = as.numeric(fishWeight);
  
  ### Part B: categorize fish based on multiple conditions
  # fish from north port and between 3 and 5 years old (inclusive of 3 and 5)
  if( ( fishLocation == "north" )  &&
      ( fishAge >= 3 && fishAge <= 5 ) )   # test 1
  {
    cat("Category I\n");
  }
  # fish from south port and between 2 and 6 years old (inclusive of 2 and 6)
  else if( ( fishLocation == "south" )  &&
      ( fishAge >= 2 && fishAge <= 6 ) )   # test 2
  {
    cat("Category II\n");
  }
  # fish is not from the north or south ports 
  else if ( fishLocation != "south" && fishLocation != "north" )  # test 3
  {
    cat("Error -- fish location is invalid");
  }
  # At this point we know the fish is from the north (age: under 3 or over 5)
  # or south port (age: under 2 or over 6).
  # Generically, we can say all fish left over 4 are too old
  else if( fishAge > 4 )   # test 4
  {
    cat("Category III\n");
  }
  # And those under 4 are too young
  else if( fishAge < 4 )   # test 5
  {
    cat("Category IV\n");
  }
  
  # So, 
  # test 1 gets all fish from the north between 3 and 5
  # test 2 gets all fish from the south between 2 and 6
  # test 3 gets all fish that are not from the north nor the south
  # test 4 gets all fish older than 5 from the north and older than 6 from the south
  #     -- think about this ... you need to remove the fish from all previous tests
  # test 5 gets all fish younger than 3 from the north and younger than 2 from the south 
  #     -- again, we are considering the results of the previous tests
}