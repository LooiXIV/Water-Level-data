graphics.off()
rm(list = ls())
# Partition the data by different waterlevel "seasons" (high, low, and mid)
require(lubridate)

# get a package only available at this URL
source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")
# set working directory
wd = paste("C:/Users/Alexander Looi/Google Drive/Dropbox/", 
           "NOAA_Wetlands_Ceili-Alex/Alex's Folder/",
           "Water level files/",sep = "")

setwd(paste(wd,"WL Data", sep = ""))

# specifcy file name
F.N = list.files(pattern = "Fixed Oswego Water Level 1906-2014.csv")

######################################################################
# Do not edite code past this point #
######################################################################

# read in data
WLs = read.csv(F.N, header = T)
# format dates
dates = mdy(WLs$Dates, tz = "EST")
# grab just the years
Years = as.numeric(format(dates, "%Y"))
# print quartiles
quantile(WLs$WL)

# Find the quantiles for each simulated period
Pre.MSD.Low = c(1933, 1942)
Pre.MSD.High = c(1943, 1951)
Post.MSD.Low = c(1973, 1982)
Post.MSD.High = c(2005, 2014)

# name the time periods
all.time = c("Pre.MSD.Low", "Pre.MSD.High", 
             "Post.MSD.Low", "Post.MSD.High")
all.time.names = c("Pre-MSD Low Precip.", "Pre-MSD High Precip.", 
                   "Post-MSD Low Precip.", "Post-MSD High Precip.")
# create letters for each time period (used in labelling figures
All.Letters = c("A", "B", "C", "D")
QL = rep(0, 5)

Q.table = data.frame(QL, QL, QL, QL, QL)

colnames(Q.table) = c(all.time, "All")

# Get all quantile scenarios data
for(p in 1:4){
  
  TP = get(all.time[p])
  
  Y = seq(TP[1], TP[2])
  
  Q.WL = WLs$WL[which(Years %in% Y)]
  
  Q.table[,p] = quantile(Q.WL)
  
}

# quartile of each quartile

for(p in 1:4){
  
  Q.WL = WLs$WL
  
  Q.table[,p] = quantile(Q.WL)
  
}

BP.table = NA

# seperate Pre and Post MSD only via when MSD was built
Pre.MSD.Low = c(1933, 1951)
Pre.MSD.High = c(1933, 1951)
Post.MSD.Low = c(1958, 2014)
Post.MSD.High = c(1958, 2014)

#  Calculate quartiles for each year of water level data
for(p in 1:4){
  
  TP = get(all.time[p])
  
  Y = seq(TP[1], TP[2])
  
  Q.WL = WLs$WL[which(Years %in% Y)]
  
  if(p == 1){
    A = Q.WL
    
  } else if (p == 3){
    B = Q.WL 
  }
  
  Q.table[,p] = quantile(Q.WL)
  
}

BP.table = list(A, B)
######################################################
# create a box plot of the best ranges of 
# water level data for the methods section
######################################################
png(filename = "Quartiles of water levels.png",
    width = 700, height = 850)

par(mar = c(3, 4, 1, 1), cex = 1.75)
boxplot(BP.table, xaxt = "n", ylab = "m IGLD", outline = F)
mtext(side = 1, line = 0.25, at = c(1,2),
      text = c("Pre-MSD", "Post-MSD"), cex = 1.5)
mtext(side = 1, line = 1.25, at = c(1,2),
      text = c("1933-1951", "1973-2014"), cex = 1.5)

dev.off()

#######################################
# the following code creates stack plots of inflows and outflow 
# sources of water to the different wetland sites at different 
# waterlevel quartiles specified by pre or post MSD and high and 
# low precip. scenarios. The years specified for these quartiels 
# are shown just below.

Pre.MSD.Low = c(1933, 1942)
Pre.MSD.High = c(1943, 1951)
Post.MSD.Low = c(1973, 1982)
Post.MSD.High = c(2005, 2014)


Q.table[,5] = quantile(WLs$WL)

High.WL = Q.table[4,]
Low.WL = Q.table[2,]
Med.WL = Q.table[3,]

# specify sites
sites = c("BF", "CB", "DB", "DR", "DL", "PT")

# find the work directory
Q.wd = paste(wd, "Quantile Analysis", sep  = "")

outputs = c("outflow", "EV", "ET")
inputs = c("backflow", "precip", "Runoff")
WL_stage = c("m", "h", "l")
mag = 1.5


setwd(Q.wd)

for(w in 1:6){
  
  for(p in 1:4){
    
    # read in wetland model data 
    WL.wd = paste("C:/Users/Alexander Looi/Google Drive/Dropbox/",
                  "NOAA_Wetlands_Ceili-Alex/Alex's Folder/Wetland Model/",
                  "Watersheds/Wetland Model Data", sep = "")
    setwd(WL.wd)
    
    years = get(all.time[p])
    
    year.csv = paste("Wetland Model Data ", 
                     years[1], "-", years[2], ".csv", 
                     sep = "")
    
    SL.WL = read.csv(year.csv, header = T)
    
    # Read in the model data for each wetland site
    setwd(paste(Q.wd, "/", sites[w], sep  = ""))
    
    TP.files = list.files(pattern="model output")
    
    TP.WS = read.csv(TP.files[p], header = T)
    
    #Find all the High water level (75% and <)
    # the Low water level (25% and >)
    # and between (between 25% and 75%)
    ML.WL.v = which(SL.WL$WL < Med.WL[1,p] & SL.WL$WL > Low.WL[1,p])
    MH.WL.v = which(SL.WL$WL < High.WL[1,p] & SL.WL$WL > Med.WL[1,p])
    H.WL.v = which(SL.WL$WL >= High.WL[1,p])
    L.WL.v = which(SL.WL$WL <= Low.WL[1,p])
    
    # number of days for each water level percentile
    nd.ml = length(ML.WL.v)
    nd.mh = length(MH.WL.v)
    nd.h = length(H.WL.v)
    nd.l = length(L.WL.v)
  
    # parition out takes for each wl stage
    TP.WS.ml = TP.WS[ML.WL.v,]
    TP.WS.mh = TP.WS[MH.WL.v,]
    TP.WS.h = TP.WS[H.WL.v,]
    TP.WS.l = TP.WS[L.WL.v,]
    
    # All outputs for each water level percentile group
    # note: at this point the outflow has both backflow and
    # outflow
    # Positive outflow days
    vec = which(TP.WS.mh$outflow >= 0)
    mh.out = TP.WS.mh$outflow[vec]
    
    vec = which(TP.WS.ml$outflow >= 0)
    ml.out = which(TP.WS.ml$outflow >= 0)
    
    vec = which(TP.WS.h$outflow >= 0)
    h.out = TP.WS.h$outflow[vec]
    
    vec = which(TP.WS.l$outflow >= 0)
    l.out = TP.WS.l$outflow[vec]
      
    # Evapotranspiration
    mh.ET = TP.WS.mh$ET
    ml.ET = TP.WS.ml$ET
    h.ET = TP.WS.h$ET
    l.ET = TP.WS.l$ET
    
    # Evaporation
    mh.EV = TP.WS.mh$EV
    ml.EV = TP.WS.ml$EV
    h.EV = TP.WS.h$EV
    l.EV = TP.WS.l$EV
      
    mh.outputs = c(sum(mh.out, na.rm = T), sum(mh.ET, na.rm = T), sum(mh.EV, na.rm = T))/10^6
    ml.outputs = c(sum(ml.out, na.rm = T), sum(ml.ET, na.rm = T), sum(ml.EV, na.rm = T))/10^6
    h.outputs = c(sum(h.out, na.rm = T), sum(h.ET, na.rm = T), sum(h.EV, na.rm = T))/10^6
    l.outputs = c(sum(l.out, na.rm = T), sum(l.ET, na.rm = T), sum(l.EV, na.rm = T))/10^6
    
    outputs = matrix(c(l.outputs,ml.outputs, mh.outputs, h.outputs), 3, 4, byrow = F)
    
    # All inputs for each water level percentile group
    # note: at this point the outflow has both backflow and
    # outflow
    # Backflows or zero days
    vec = which(TP.WS.mh$outflow <= 0)
    mh.back = abs(TP.WS.mh$outflow[vec])
    
    vec = which(TP.WS.ml$outflow <= 0)
    ml.back = abs(TP.WS.ml$outflow[vec])
      
    vec = which(TP.WS.h$outflow <= 0)
    h.back = abs(TP.WS.h$outflow[vec])
      
    vec = which(TP.WS.l$outflow <= 0)
    l.back = abs(TP.WS.l$outflow[vec])
      
    # Runoff
    mh.runoff = TP.WS.mh$Daily.inflow
    ml.runoff = TP.WS.ml$Daily.inflow
    h.runoff = TP.WS.h$Daily.inflow
    l.runoff = TP.WS.l$Daily.inflow
      
    # Precip
    mh.prcp = TP.WS.mh$m.prcp
    ml.prcp = TP.WS.ml$m.prcp
    
    h.prcp = TP.WS.h$m.prcp
    l.prcp = TP.WS.l$m.prcp
    
    # Sum data and put into a matrix for plotting
    mh.inputs = c(sum(mh.back, na.rm = T), sum(mh.runoff, na.rm = T), sum(mh.prcp, na.rm = T))/10^6
    ml.inputs = c(sum(ml.back, na.rm = T), sum(ml.runoff, na.rm = T), sum(ml.prcp, na.rm = T))/10^6
    h.inputs = c(sum(h.back, na.rm = T), sum(h.runoff, na.rm = T), sum(h.prcp, na.rm = T))/10^6
    l.inputs = c(sum(l.back, na.rm = T), sum(l.runoff, na.rm = T), sum(l.prcp, na.rm = T))/10^6
    
    inputs = matrix(c(l.inputs, ml.inputs, mh.inputs, h.inputs), 3, 4, byrow = F)
    
    in.outpre = data.frame(c(inputs[,1], 0,0,0),
               c(0,0,0, outputs[,1]),
               c(inputs[,2], 0,0,0),
               c(0,0,0, outputs[,2]),
               c(inputs[,3], 0,0,0),
               c(0,0,0, outputs[,3]),
               c(inputs[,4], 0,0,0),
               c(0,0,0, outputs[,4]))
    
    colnames(in.outpre) = c(1,2,3,4,5,6,7,8)
    
    in.out = as.matrix(in.outpre)
    
    # Normalize over number of days
    # outputs[,1] = outputs[,1]/nd.h
    # outputs[,2] = outputs[,2]/nd.mh
    # outputs[,3] = outputs[,3]/nd.ml
    # outputs[,4] = outputs[,4]/nd.l
    # 
    # inputs[,1] = inputs[,1]/nd.h
    # inputs[,2] = inputs[,2]/nd.mh
    # inputs[,3] = inputs[,3]/nd.ml
    # inputs[,4] = inputs[,4]/nd.l
    
    # if(sites[w] == "DR"){
    #   max.y = 0.002
    # } else if (sites[w] == "BF") {
    #   max.y = 0.005
    # } else if (sites[w] == "DB") {
    #   max.y = 0.018
    # } else if (sites[w] == "CB") {
    #   max.y = 0.005
    # } else if (sites[w] == "PT") {
    #   max.y = 0.008
    # } else if (sites[w] == "DL") {
    #   max.y = .01
    # }
    
    # set the x and y labels
    y.lab1 = expression("daily inflows (10"^6*"  m"^3*")")
    y.lab2 = expression("daily outflows (10"^6*"  m"^3*")")
    if(sites[w] == "DR"){
      max.y = 1.5
    } else if (sites[w] == "BF") {
      max.y = 3.5
    } else if (sites[w] == "DB") {
      max.y = 12
    } else if (sites[w] == "CB") {
      max.y = 3.5
    } else if (sites[w] == "PT") {
      max.y = 3.8
    } else if (sites[w] == "DL") {
      max.y = 7
    }
    y.lab1 = expression("Total inflows (10"^6*"  m"^3*")")
    y.lab2 = expression("Total outflows (10"^6*"  m"^3*")")
    
    setwd(Q.wd)
    
    # Plot the data 
    if(p == 1){
      graphics.off()
      title.png = paste("Water Level Stack Plots ",
                        sites[w], ".png", sep = "")
      png(filename = title.png
          ,width = 2200, height = 1700)
      layout(matrix(c(1,2,3,4,5,5),3, 2, byrow = T), heights = c(3, 3, 0.85))
    }
    
    if(p %% 2 == 0){
      margins = c(4, 1.5, 4.5, 6.5)
    } else {
      margins = c(4, 6.5, 4.5, 1.5) 
    }
    
    y.tick.int = (max.y - 0)/4
    y.ticks.left = seq(0, max.y, by = y.tick.int)
    y.ticks.right = seq(max.y, 0, by = -y.tick.int) 
    # Plot the newly chimped data
    par(cex = mag, mar = margins)
    # Name the figure
    # title.barplot = paste(all.time[p], " ", 
    #                       sites[w], sep = "")
    
    x.bar = barplot(in.out, ylab = "", axes = F, ylim = c(0, max.y),
                     col = c("#045a8d", "#74a9cf", "#f1eef6",
                             "#993404", "#d95f0e", "#fec44f"),
                     space = c(0.75, 0.05), xaxt= "n")
    x.bar1 = c((x.bar[1]+x.bar[2])/2, (x.bar[3]+x.bar[4])/2
               , (x.bar[5]+x.bar[6])/2, (x.bar[7]+x.bar[8])/2)
    y.text = max.y-0.05*max.y
    # Number of days per quartile labeled in the center of the figures
    text(y = y.text, x = c(x.bar1), cex = mag,
         c(paste(nd.l, "days"),
           paste(nd.ml, "days"),
           paste(nd.mh, "days"),
           paste(nd.h, "days")))
    
    # add the new outflows where they hang form the bottom
    # par(new = T)
    par(cex = mag, mar = margins)
    # barplot(outputs, ylim = rev(c(0, max.y)), axes = F,
    #         col = c("darkorchid", "dodgerblue", "lawngreen"))

    # add the y.labs to the left and right axis
    if(p %% 2 == 0){
      x.pos = par("usr")[2] + 0.45*(x.bar1[2]-x.bar1[1])
      y.pos = max.y/2
      text(x.pos, y.pos, labels = y.lab2, srt = -90, cex = mag+0.25, xpd = T)
      axis(side = 4, at = round(y.ticks.right, digits = 1), las = 2, cex.axis = mag)
      
    } else {
      x.pos = par("usr")[1] - 0.45*(x.bar1[2]-x.bar1[1])
      y.pos = max.y/2
      text(x.pos, y.pos, labels = y.lab1, srt = 90, cex = mag+0.25, xpd = T)
      axis(side = 2, at = round(y.ticks.right, digits = 1), las = 2, cex.axis = mag)
      
    }
    # show the show the percentiles
    mtext(side = 1, at = x.bar1, line = .65, lwd = 1.25,
          text = c("< 25%", "25% - 50%", "50% - 75%", "> 75%"), 
          cex = mag+.75)
    # top label showing the water level ranges
    if(p <= 2){
      mtext(side = 3, line = 2.75, lwd = 1.25,
            "Water Levels Ranges (m)", cex = mag + 1.1)
    } else {
      
      mtext(side = 1, line = 2.75, "Water Level Percentile", 
            cex = mag + 1.1, lwd = 1.25)
    }
    mtext(side = 3, at = c(x.bar1), line = 0.65, lwd = 1.25, cex = mag+0.75, 
          text = c(paste("< ",round(Low.WL[p], digits = 2)),
                   paste(round(Med.WL[p], digits = 2), "-", round(Low.WL[p], digits = 2)),
                   paste(round(High.WL[p], digits = 2), "-", round(Med.WL[p], digits = 2)),
                   paste("> ",round(High.WL[p], digits = 2))))
 
    # add a letter to for the figure description
    # text(par("usr")[4]-0*y.tick.int, par("usr")[1] - x.bar1[1]*1.5,
    #      labels = All.Letters[p], xpd = T, cex = 2)
  }
  # plot the figure legend
  source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")
  plot.new()
  par(mar = c(0,0,0,0))
  
  legend('center', cex = mag + 0.25, y.intersp = 3,
         c("Flow Reversals", "Runoff","Precipitation",
           "Outflow", "ET", "EV"), 
         fill = c("#045a8d", "#74a9cf", "#f1eef6",
                  "#993404", "#d95f0e", "#fec44f"),
         ncol = 2, bty ="n", box.cex = c(3, 2))
  dev.off()
  graphics.off()
}