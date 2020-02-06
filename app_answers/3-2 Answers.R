{
  rm(list=ls());  options(show.error.locations = TRUE);
  
  source("scripts/toolbox.r");
  library("rnoaa");    # include functions from the rnoaa package
  library("reshape2"); # include functions from the reshape2 package
  
  # collecting about 450 values (90 days * 5 values/days)
  lansingWeather =  ncdc(datasetid='GHCND', 
                         datatypeid=c("TMAX", "TAVG", "TMIN", 
                                      "PRCP", "SNOW"),
                         stationid='GHCND:USW00014836', 
                         startdate = '2016-12-01', enddate ='2017-02-28',
                         token=myToken,
                         limit=500 );
  
  lansingWinterWeather = lansingWeather[["data"]];
  
  lansingWeatherRS = dcast(data = lansingWinterWeather,
                           formula = date ~ datatype,
                           value.var = "value");
  
  lansingWeatherRS = lansingWeatherRS[,c("date", "TMIN", "TMAX", 
                                         "TAVG", "PRCP", "SNOW")];
  
  colnames(x=lansingWeatherRS)[2] = "minTemp";
  colnames(x=lansingWeatherRS)[3] = "maxTemp";
  colnames(x=lansingWeatherRS)[4] = "avgTemp";
  
  lansingWeatherRS[,c(2:6)] = lansingWeatherRS[,c(2:6)]/10;
  
  lansingWeatherRS[,c(5,6)] = lansingWeatherRS[,c(5,6)]/25;
  
  lansingWeatherRS[,c(5,6)] = round(x=lansingWeatherRS[,c(5,6)],digits=2);

  lansingWeatherRS[,1] = substr(x=lansingWeatherRS[,1], start=3, stop=10);

  maxTempMean = mean(lansingWeatherRS[,"maxTemp"]); 
  maxTempSD = sd(lansingWeatherRS[,"maxTemp"]);
  maxTempVar = var(lansingWeatherRS[,"maxTemp"]);
  
  minTempMean = mean(lansingWeatherRS[,"minTemp"]); 
  minTempSD = sd(lansingWeatherRS[,"minTemp"]);
  minTempVar = var(lansingWeatherRS[,"minTemp"]);
  
  changeInTemp = lansingWeatherRS[,"maxTemp"] - lansingWeatherRS[,"minTemp"];
  
  lansingWeatherRS[,"changeTemp"] = changeInTemp;
}