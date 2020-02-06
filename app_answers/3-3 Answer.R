{
  rm(list=ls());  options(show.error.locations = TRUE);
  
  
  source("scripts/toolbox.r");
  library("rnoaa");    # include functions from the rnoaa package
  library("reshape2"); # include functions from the reshape2 package
  
  # collecting average wind from January 2016 
  JanWinds =  ncdc(datasetid='GHCND', 
                   datatypeid=c("AWND"),
                   stationid='GHCND:USW00014836', 
                   startdate = '2016-01-01', enddate ='2016-01-31',
                   token=myToken,
                   limit=40 );

  # collecting average wind from February 2016 
  FebWinds =  ncdc(datasetid='GHCND', 
                   datatypeid=c("AWND"),
                   stationid='GHCND:USW00014836', 
                   startdate = '2016-02-01', enddate ='2016-02-29',
                   token=myToken,
                   limit=40 );
  
  # collecting average wind from March 2016 
  MarWinds =  ncdc(datasetid='GHCND', 
                   datatypeid=c("AWND"),
                   stationid='GHCND:USW00014836', 
                   startdate = '2016-03-01', enddate ='2016-03-31',
                   token=myToken,
                   limit=40 );
  
  # collecting average wind from April 2016 
  AprWinds =  ncdc(datasetid='GHCND', 
                   datatypeid=c("AWND"),
                   stationid='GHCND:USW00014836', 
                   startdate = '2016-04-01', enddate ='2016-04-30',
                   token=myToken,
                   limit=40 );
  
  JanData = JanWinds[["data"]];
  FebData = FebWinds[["data"]];
  MarData = MarWinds[["data"]];
  AprData = AprWinds[["data"]];
  
  JanVector = JanData[,"value"];
  FebVector = FebData[,"value"];
  MarVector = MarData[,"value"];
  AprVector = AprData[,"value"];
  

  # Add the remaining days:
  FebVector[30] = NA;
  FebVector[31] = NA;
  AprVector[31] = NA;

  allMonthsVector = c(JanVector, FebVector, MarVector, AprVector);
  
  windsMatrix = matrix(data=allMonthsVector, nrow=31, ncol=4);
  
  
  meanFeb = mean(windsMatrix[,2], na.rm=TRUE);
  sdFeb = sd(windsMatrix[,2], na.rm=TRUE);
  varFeb = var(windsMatrix[,2], na.rm=TRUE);
  
  meanMar = mean(windsMatrix[,3], na.rm=TRUE);
  sdMar = sd(windsMatrix[,3], na.rm=TRUE);
  varMar = var(windsMatrix[,3], na.rm=TRUE);
  
  windsMeanMonth=c();
  maxVal = 0;
  maxMon = 0;
  for(i in 1:4)
  {
    windsMeanMonth[i] = mean(windsMatrix[,i], na.rm=TRUE);
    if(windsMeanMonth[i] > maxVal)
    {
      maxVal = windsMeanMonth[i];
      maxMon = i;
    }
  }
  
  windsMeanDays = c();
  maxVal2 = 0;
  maxMon2 = 0;
  for(i in 1:31)
  {
    windsMeanDays[i] = mean(windsMatrix[i,], na.rm=TRUE); 
    if(windsMeanDays[i] > maxVal2)
    {
      maxVal2 = windsMeanDays[i];
      maxMon2 = i;
    }
  }

  
}