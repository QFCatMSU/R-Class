{
  # these two lines of code should always be in your script
  rm(list=ls()); 
  options(show.error.locations = TRUE); 
  
  # number of fish in the north and south ports
  numFishNorth = 1000;
  numFishSouth = 500;
  
  # numer of days to catch the fish at each port
  daysNorth = 8;
  daysSouth = 10;
  
  # numer of fish caught per day for both ports
  fishPerDayNorth = numFishNorth/daysnorth;
  fishPerDaySouth = numFishSouth/daysSouth;
  
  # line 15 will cause an error:
  # object 'daysnorth' not found
  # because 'daysnorth' is not the same as 'daysNorth'
}