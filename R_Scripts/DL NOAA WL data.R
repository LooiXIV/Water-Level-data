graphics.off();
rm(list=ls());

# set the working directory
setwd("C:/Users/Alex Looi/Desktop/Demostration")

directory = "C:/Users/Alex Looi/Desktop/Demostration"

library(RCurl)

#Site.WLname = "Alex Bay"
Site.WLname = "Oswego"
#Site.WLname = "Cape Vincent"

#site_no = "8311062" # Alexandria Bay
site_no = "9052030" # Oswego NY
#site_no = "9052000" # Cape Vincenet, NY
#start_date = "20130101"
#end_date = "20131231"

start = as.Date("1900/01/01")
end = as.Date("2014/12/31")

dates = seq.Date(start, end, by = "year")

dates = as.character(dates)

interval = "daily_mean"
#interval = "hourly_height"
TZ = "LST" # goes with the "daily_mean" data set
#TZ = "GMT" # goes with the "hourly_height" data set
############################################################
# Do not edit code past this point #
############################################################

# function to process the user inputs and download the raw HTML
getWLcsv.NOAA = function(site_no, start_date, end_date, TimeZone){
  
  web = paste("http://tidesandcurrents.noaa.gov/api/datagetter?product=",interval ,
              "&application=NOS.COOPS.TAC.WL&station=",
              site_no, "&begin_date=",
              start_date, "&end_date=", end_date,
              "&datum=IGLD&units=metric&time_zone=",TimeZone,"&format=csv", sep = "")
  
  x = getURL(web)

  output = read.csv(textConnection(x))

  return(output)

}

# sort the data by year and print them out to seperate CSV files.
for(d in 1:length(dates)){

  begin = paste(substr(dates[d], 1, 4), substr(dates[d], 6, 7), substr(dates[d], 9, 10), sep = "")
  end = paste(substr(dates[d], 1, 4), "12", "31", sep = "")
  
  data = getWLcsv.NOAA(site_no, begin, end, TZ)
  
  Year = substr(dates[d], 1, 4)
  
  name.file = paste("NOAA ", Site.WLname, " ", interval, " WL", "_", Year, ".csv", sep = "")
  
  write.csv(data, file = name.file)
  
}