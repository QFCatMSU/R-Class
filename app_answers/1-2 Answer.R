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
  
  # numer of fish caught per day calculated for both ports
  fishPerDayNorth = numFishNorth/daysNorth;
  fishPerDaySouth = numFishSouth/daysSouth;
}