{
  # these two lines of code should always be in your script
  rm(list=ls()); 
  options(show.error.locations = TRUE); 
  
  # number of fish in the north and south ports
  numFishNorth = 1000;
  numFishSouth = 500;
  
  # numer of fish caught per day for both ports
  fishPerDayNorth = numFishNorth/daysNorth;
  fishPerDaySouth = numFishSouth/daysSouth;  
  
  # numer of days to catch the fish at each port
  daysNorth = 8;
  daysSouth = 10;

  # line 11 will cause an error: object 'daysNorth' not found
  # because 'daysNorth' is not assigned a value until line 15
}