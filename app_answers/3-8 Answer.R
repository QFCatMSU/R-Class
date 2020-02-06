{
  rm(list=ls()); options(show.error.locations = TRUE);  
  source("scripts/toolbox.r");

  # Get weather data for 2016 in Lansing, MI
  lansing2016Weather = read.csv(file="data/LansingNOAA2016Formatted.csv",
                                stringsAsFactors = FALSE);
  
  # Extract the various parts of the weather data and save each to a vector
  date = lansing2016Weather[,"date"];
  eventData = lansing2016Weather[,"eventData"];
  avgTemp = lansing2016Weather[,"avgTemp"];
  tempDept = lansing2016Weather[,"tempDept"];  
  precipitation = lansing2016Weather[,"precipitation"];  
  humidity = lansing2016Weather[,"humidity"];
  barometer = lansing2016Weather[,"barometer"];
  dewPoint = lansing2016Weather[,"dewPoint"];
  avgWind = lansing2016Weather[,"avgWind"];
  maxWind = lansing2016Weather[,"maxWind"];  
  windDirection =  lansing2016Weather[,"windDirection"];
  sunrise = lansing2016Weather[,"sunrise"];
  sunset = lansing2016Weather[,"sunset"];
  
  # index of date values that have "02-" or "03-" in it 
  # you need to include the dash otherwise you will get Jan 2, April 2, May 2,... 
  febMarSubset = grep("02-|03-", date);
  ### an equivalent (and more robust) answer:
  #   febMarSubset = grep("0[2|3]-", date);
  
  # index of the days with humidity greater than 80%
  highWind = which(maxWind > 40);
  
  # Get the number of days with high humidity, which is the 
  # same as number of value in highHumidity)
  numDaysHighWind = length(highWind);
  
  # Get the dates with high humidity (there will be 15)
  dateHighWind = date[highWind];
  # Get the precipitation on the days with high humidity  
  precipHighHum = precipitation[highWind];
  # Get the max wind on the days with high humidity 
  windHighHum = maxWind[highWind];
  
  # print dates with northerly winds (90 - 270 degrees)
  northWindIndex = which(windDirection > 90 & windDirection < 270);
  dateNorthWind = date[northWindIndex];
  cat("\nDays with northerly winds:\n");
  print(dateNorthWind);
  
  # print dates with northerly winds (90 - 270 degrees) and high precipitation
  northWindIndex2 = which( (windDirection > 90 & windDirection < 270) 
                          & (precipitation > 0.5) ) ;
  dateNorthWind2 = date[northWindIndex2];
  cat("\nDays with northerly winds and high precipitation:\n");
  print(dateNorthWind2);
  
  hoursOfSun = timeConvert(sunset) - timeConvert(sunrise);

  # Do a linear model of avgTemp vs hoursOfSun for January and February
  JanFebIndex = grep("01-|02-", date);
  plot(formula=avgTemp[JanFebIndex]~hoursOfSun[JanFebIndex]);
  model1 = lm(formula=avgTemp[JanFebIndex]~hoursOfSun[JanFebIndex]);
  abline(model1, col="blue");    # add the regression line to the plot
  cat("\n***January and February***\n");
  print(summary(model1));        # summary does not shows a relationship

  # Do a linear model of avgTemp vs hoursOfSun for March and April
  MarAprIndex = grep("03-|04-", date);
  plot(formula=avgTemp[MarAprIndex]~hoursOfSun[MarAprIndex]);
  model2 = lm(formula=avgTemp[MarAprIndex]~hoursOfSun[MarAprIndex]);
  cat("\n***March and April***\n");
  abline(model2, col="blue");    # add the regression line to the plot
  print(summary(model2));        # summary shows a relationship

  # Do a linear model of avgTemp vs hoursOfSun for May and June
  MayJunIndex = grep("05-|06-", date);
  plot(formula=avgTemp[MayJunIndex]~hoursOfSun[MayJunIndex]);
  model3 = lm(formula=avgTemp[MayJunIndex]~hoursOfSun[MayJunIndex]);
  abline(model3, col="blue");    # add the regression line to the plot
  cat("\n***May and June***\n");
  print(summary(model3));        # summary shows a relationship
  
  # Do a linear model of avgTemp vs hoursOfSun for July and August
  JulAugIndex = grep("07-|08-", date);
  plot(formula=avgTemp[JulAugIndex]~hoursOfSun[JulAugIndex]);
  model4 = lm(formula=avgTemp[JulAugIndex]~hoursOfSun[JulAugIndex]);
  abline(model4, col="blue");    # add the regression line to the plot
  cat("\n***July and August***\n");
  print(summary(model4));        # summary does not shows a relationship
  
  # Do a linear model of avgTemp vs hoursOfSun for September and October
  SepOctIndex = grep("09-|10-", date);
  plot(formula=avgTemp[SepOctIndex]~hoursOfSun[SepOctIndex]);
  model5 = lm(formula=avgTemp[SepOctIndex]~hoursOfSun[SepOctIndex]);
  abline(model5, col="blue");    # add the regression line to the plot
  cat("\n***September and October***\n");
  print(summary(model5));        # summary shows a relationship
  
  # Do a linear model of avgTemp vs hoursOfSun for November and December
  NovDecIndex = grep("11-|12-", date);
  plot(formula=avgTemp[NovDecIndex]~hoursOfSun[NovDecIndex]);
  model6 = lm(formula=avgTemp[NovDecIndex]~hoursOfSun[NovDecIndex]);
  abline(model6, col="blue");    # add the regression line to the plot
  cat("\n***November and December***\n");
  print(summary(model6));        # summary shows a relationship
}