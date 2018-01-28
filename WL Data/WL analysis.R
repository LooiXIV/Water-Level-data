# River water level quantiles and mean
graphics.off()
rm(list = ls())

w.dir = paste("C:/Users/Alexander Looi/Dropbox/",
          "NOAA_Wetlands_Ceili-Alex/Alex's Folder",
          "/Water level files/WL Data", sep = "")

setwd(w.dir)

file.name = list.files(pattern = "Fixed Oswego")

WL.Oswego = read.csv(file.name)

# Dates for building MSD

SE.dates = as.POSIXlt(c("1-1-1954","31-12-1958"), format = "%d-%m-%Y")

WL.Oswego$Dates = as.POSIXlt(as.character(WL.Oswego$Dates)
                  , format = "%m/%d/%Y", tz = "EST")

# Pre-MSD WL's
# Grab data just before 1954

MSD.s = which(SE.dates[1] == WL.Oswego$Dates)

Pre.MSD = WL.Oswego[1:MSD.s,]

# Post-MSD WL's
# Grab data just after 1958

MSD.e = which(SE.dates[2] == WL.Oswego$Dates)
LD = length(WL.Oswego$Dates)

Post.MSD = WL.Oswego[MSD.e:LD,]

# Boxplots
box1 = boxplot(Pre.MSD$WL, Post.MSD$WL)

pre.q = quantile(Pre.MSD$WL)
post.q = quantile(Post.MSD$WL)

post.q-pre.q

SL.q = quantile(WL.Oswego$WL)

# Take model parameters, and look at river height
# is backflow related to river water level and then compare 
# Marsh distance to river.
# Volume of backflow or outflow, 
# when is it occuring? (high WL or low WL?)
###########################################################################
main.dir = paste("C:/Users/Alexander Looi/Dropbox/NOAA_Wetlands_Ceili-Alex/",
                 "Alex's Folder/Wetland Model/Watersheds", sep = "")
S.Years = c(1933, 1943, 1973, 2005)
E.Years = c(1942, 1951, 1982, 2014)

sites.TIBS = c("Carpenters", "Deferno", "Delaney", 
               "Plum Tree", "Butterfield", "DorrRoad")

Wetland.Sites = c(c("Carpenters", "CB"),c("Deferno", "DB"), 
                  c("Delaney", "DL"), c("Plum Tree", "PT"),
                  c("Butterfield", "BF"), c("DorrRoad", "DR"))

ptm = proc.time()


# Read in Lake Ontario Oswego WL data
list.files()

for(year.int in 1:4){
  setwd(main.dir)
  Start.Year = S.Years[year.int]
  End.Year = E.Years[year.int]
  
  WL.file.name = paste("Wetland Model Data ", Start.Year,"-",End.Year, sep = "")
  
  setwd(paste(main.dir, "/Wetland Model Data/", sep = ""))
  model.data = read.csv(list.files(pattern = WL.file.name), header = T)
  
  for (sw in 1:6) {
    
    Site.name = sites.TIBS[sw]
    
    setwd(main.dir)
    
    site.pos = which(Wetland.Sites == Site.name)
    site.abv = Wetland.Sites[site.pos + 1]
    
    # set the wd to the wetland sites
    setwd(paste(main.dir, "/Wetland Sites/", sep = ""))
    
    # Read in All necessary data to calculate: 
    # potential evaporation and water levels
    cur.WL.file = paste(Site.name, "TIBS")
    
    setwd(paste(main.dir, "/Wetland Sites/", cur.WL.file, sep = ""))
    
    model.output.file = list.files(pattern = paste(site.abv, " ",
                               Start.Year, "-", End.Year, sep = ""))
    
    model.output = read.csv(model.output.file, header = T)
    
    SL.WL = model.data$WL[2:length(model.data$WL)]
    
    #WS.WL = model.output$peak.WL
    WS.WL = model.output$avg.WL
    
    # plot(c(SL.WL-WS.WL), model.output$outflow, xlab = "", 
    #      main = paste(site.abv, " ", Start.Year, "-", End.Year, sep = ""))
    plot(model.output$m.prcp, c(WS.WL), xlab = "", 
         main = paste(site.abv, " ", Start.Year, "-", End.Year, sep = ""))
    
  }
  
}