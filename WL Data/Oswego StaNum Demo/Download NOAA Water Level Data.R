graphics.off();
rm(list = ls());
require(lubridate)
############################################################################################
dir = "C:/Users/Alexander Looi/Google Drive/Dropbox/NOAA_Wetlands_Ceili-Alex/Alex's Folder/Water level files/WL Data"
setwd(dir)

folder.name = "Oswego StaNum Demo" # Bugged method
if(dir.exists(folder.name) != T){
  dir.create(folder.name)  
}

dest.folder = paste(dir, "/", folder.name, sep = "")

setwd(dest.folder)

StaNum = 9052030
SiteName = "Oswego"
S.Year = 1900
E.Year = 2014
datum = "IGLD"
units = "metric"
tz = "LST"
file.type = "csv"
product = "daily_mean"

Years = seq(S.Year, E.Year)

WL.files = rep("", length(Years))

################################################################################
# Download the data #
################################################################################
for(Y in 1:length(Years)){
  
  S.Date = paste(Years[Y], "0101", sep = "")
  E.Date = paste(Years[Y], "1231", sep = "")
  
  URL = paste("https://tidesandcurrents.noaa.gov/api/datagetter?",
              "product=", product,"&application=NOS.COOPS.TAC.WL&station=", StaNum,"&",
              "begin_date=",S.Date,"&end_date=",E.Date,"&",
              "datum=",datum,"&units=",units,"&time_zone=",tz,"&format=",file.type, sep = "")
  
  WL.filename = paste("NOAA ", SiteName, " ", product, " WL_", Years[Y],
                      ".", file.type, sep = "")
  WL.files[Y] = WL.filename
  
  download.file(URL, WL.filename)
}

#############################################################
# 1) Combine all the data files into one large table #
# 2) Find missing data and flag and replace          #
#############################################################

# Combine all the data files into a large table
for (f in 7:length(Years)){
  
  WL.file = read.csv(WL.files[f], header = T)
  
  Date.Time.T = ymd_hm(WL.file$Date.Time)
  Date.Time.T = format(Date.Time.T, "%Y-%m-%d")
  
  CDateS = as.POSIXct(paste(Years[f], "-01-01", sep = ""))
  CDateE = as.POSIXct(paste(Years[f], "-12-31", sep = ""))
  
  check_dates = format(seq(CDateS, CDateE, by = "day"), "%Y-%m-%d")
  
  check.vec = which(check_dates %in% Date.Time.T)

  WL.T = rep(NA, length(check_dates))
  
  WL.T[check.vec] = WL.file$Water.Level[check.vec]
  
  if(f==7) {
    Date.Time = check_dates
    WL = WL.T
  } else {
    Date.Time = c(Date.Time, check_dates)
    WL = c(WL, WL.T)
  }
  
}

WL.Sta = data.frame(Date.Time, WL)

# Find missing data and replace it with the average of two
# adjoining days.

WL.na = which(is.na(WL))
x = 0
x1 = 1
n = 1
xi = WL.na[1]

test.WL = WL


for(n in 1:length(WL.na)){
  
  x = WL.na[n]
  x1 = WL.na[n + 1]
  
  if(n == length(WL.na)) {
    WL[xi:x] = (WL[xi-1]+WL[x+1])/2
  } else {
    
    if(x1 != (x+1)){
      WL[xi:x] = (WL[xi-1]+WL[x+1])/2
      
      xi = x1
    }
  }
  
}

# check to see if things look okay.

WL[WL.na]
