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
  
  hoursOfSun = timeConvert(sunset) - timeConvert(sunrise);
  
  hoursLT10 = which(hoursOfSun < 10)
  tempDaysLT10 = mean(avgTemp[hoursLT10]);
  
  hoursGT14 = which(hoursOfSun > 14)
  tempDaysGT14 = mean(avgTemp[hoursGT14]); 
    
  humidityGT90 = which(humidity > 90);
  numDaysHumGT90 = length(humidityGT90);
  datesHumGT90 = date[humidityGT90];
  precipHumGT90 = precipitation[humidityGT90];
  maxWindHumGT90 = maxWind[humidityGT90];
  
  exclusivelyHazy = which(eventData == "HZ");
  datesExclHazy = date[exclusivelyHazy];
  
  inclusivelyHazy = grep("HZ", eventData);
  datesIncHazy = date[inclusivelyHazy];
}