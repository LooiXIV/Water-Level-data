 Create linear regressions between ABay WL and Site
rm(list = ls());
graphics.off();
require(lubridate)


# Site Water Level Data
WL.dir = paste("C:/Users/Alexander Looi/Google Drive/Dropbox/NOAA_Wetlands_Ceili-Alex/",
               "Alex's Folder/Water level files/WL Data", sep = "")

setwd(WL.dir)

dir.Sites = list.files(pattern = "sites.csv")

dl = length(dir.Sites)

file.names = substr(dir.Sites, 1, 2)

m.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
m.num = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

for(f in 1:dl) {
  
  data.temp = read.csv(dir.Sites[f], sep = ",", header = T)
  
  # create a date.time column
  
  date.time = rep(0, length(data.temp$IGLD.WL.m.))
  
  date = as.Date(as.character(data.temp$Date), 
                 format = "%m/%d/%Y")
  
  date.time = paste(date, as.character(data.temp$Hour))
  
  data.temp$date.time = as.POSIXlt(date.time, "%Y-%m-%d %H:%M", 
                                   tz = "America/New_York")
  
  data.temp$date.time = as.character(data.temp$date.time)
  
  file.name = file.names[f]
  
  assign(file.name, data.temp)
}

#ABay/Oswego data

# Get Necessary ABay Data
S.year = 1906
E.year = 2014

seq.year = as.character(seq(S.year, E.year))

dir.NOAAWL = list.files(pattern = "Fixed")
# Read in Oswego data
WL.data = read.table(dir.NOAAWL, header = T, sep = ",")

WL.data$Dates = as.character(as.Date(as.character(WL.data$Dates), format = "%m/%d/%Y"))

match.name = rep(0, 6)

for (f in 1:dl){
  
  # Calculate the mean water level for each site
  
  TsL = length(get(file.names[f])$WL)
  
  temp.site = get(file.names[f])
  
  temp.site$Date = as.character(as.Date(as.character(temp.site$Date), 
                                        format = "%m/%d/%Y"))
  
  D.S = as.character(temp.site$Date[1])
  D.E = as.character(temp.site$Date[TsL])
  
  D.S = as.Date(D.S)
  D.E = as.Date(D.E)
  
  Dates.sites = as.character(seq.Date(D.S, D.E, by = "day"))
  
  s.len = length(Dates.sites)
                 
  match.name[f] = paste(file.names[f], ".match", sep = "")
  
  temp.match = data.frame(rep(0, s.len), rep(0,s.len), rep(0,s.len))
  
  colnames(temp.match) = c("Date", "Site.WL", "NOAA.WL")
  
  temp.match$Date = Dates.sites
  
  for (d in 1:s.len){
    # add site daily mean WL
    d.vec = which(Dates.sites[d] == temp.site$Date)
    temp.match$Site.WL[d] = mean(temp.site$WL[d.vec])
    # Add Oswego WL
    n.vec = which(Dates.sites[d] == WL.data$Dates) 
    
    temp.match$NOAA.WL[d] = WL.data$WL[n.vec]
  }
  
  assign(match.name[f], temp.match)
  
}

# create scatter plots and  of the "matched" data

for (p in 1:dl){

  plot.data = get(match.name[p])
  
  site = substr(match.name[p], 1, 2)
  
  plot(plot.data$NOAA.WL, plot.data$Site.WL, main = site)

}

# Plot Carpenters Branch
##########################
#par(cex = 1.75)
par(cex = 1)
plot(CB.match$NOAA.WL, CB.match$Site.WL, 
     main = "Site with WCS: Carpenters Branch",
     ylab = "Site Water Level (IGLD m)",
     xlab = "Alexandria Bay Water Level (IGLD m)",
     xlim = c(74.2, 75.2), ylim = c(74.0,76.0))

# Remove Outliers From CC, DB, and DR
######################################
# x values
######################################
x = seq(73, 77, by = 0.01)

######################################
# CC
CC.remove = which(CC.match$Site.WL < 74.4)

CC.replot = CC.match[-CC.remove,]

plot(CC.replot$NOAA.WL, CC.replot$Site.WL)

lm.CC = lm(CC.replot$Site.WL~CC.replot$NOAA.WL)

m.CC = as.numeric(lm.CC$coefficients[2])
b.CC = as.numeric(lm.CC$coefficients[1])
y.CC = m.CC*x + b.CC

CC.parms = c(m.CC, b.CC)

summary(lm.CC)

# Carpenters Confluence

plot(CC.match$NOAA.WL, CC.match$Site.WL, 
     main = "Carpenters Confluence", 
     xlab = "ABay  Water Level (IGLD m)", 
     ylab = "CC Water Level (IGLD m)")
lines(x, y.CC, col = 2)
################################################
# DR #
################################################
DR.remove = which(DR.match$Site.WL < 74.4)

DR.replot = DR.match[-c(38:91),]

# DRabay.remove = which(DR.replot$NOAA.WL < 74.4)
# 
# DR.replot = DR.replot[-DRabay.remove,]

lm.DR = lm(DR.replot$Site.WL~DR.replot$NOAA.WL)

m.DR = as.numeric(lm.DR$coefficients[2])
b.DR = as.numeric(lm.DR$coefficients[1])
y.DR = m.DR*x + b.DR

DR.parms = c(m.DR, b.DR)
DR.sum = summary(lm.DR)


plot(DR.match$NOAA.WL, DR.match$Site.WL, 
     main = "Dorr Road", 
     xlab = "NOAA WL", ylab = "Site WL")


# DB
lm.DB = lm(DB.match$Site.WL~DB.match$NOAA.WL)
summary(lm.DB)

m.DB = as.numeric(lm.DB$coefficients[2])
b.DB = as.numeric(lm.DB$coefficients[1])
y.DB = m.DB*x + b.DB

DB.parms = c(m.DB, b.DB)

plot(DB.match$NOAA.WL, DB.match$Site.WL, 
     main = "Deferno Branch", 
     xlab = "NOAA WL (IGLD m)", ylab = "Site WL (m IGLD)")

lines(x, y.DB, col = 2)

# plot deferno Branch
plot(DB.match$NOAA.WL[1:2604], DB.match$Site.WL[1:2604])


lm.DBr = lm(DB.match$Site.WL~DB.match$NOAA.WL)

DBr.sum = summary(lm.DBr)

m.DBr = as.numeric(lm.DBr$coefficients[2])
b.DBr = as.numeric(lm.DBr$coefficients[1])
y.DBr = m.DBr*x + b.DBr

DBr.parms = c(m.DBr, b.DBr)
######################################################
mag = 1.4
lv = 2.75
WH = 700
######################################################
# WL vs. Abay Wl plotting function
######################################################
WLplotting = function(SWL, AWL, ax.min, ax.max,
                      x.line, S.reg,
                      y.lab, mag, WH, r2, p.val, 
                      plot.name, has.WCS){
  if(has.WCS) {
    margin = c(5.5, 5.5, 1, 1)
  } else {
    margin = c(5.5, 1, 1, 5.5)
  }
  ax.pos.y = (ax.max+ax.min)/2
  
  png(filename = plot.name,
      width = WH, height = WH)
  par(cex = mag, mar = margin)
  plot(AWL, SWL, pch = 21, 
       ylab = "", yaxt = "n",
       xlab = "", xaxt = "n",
       ylim = c(ax.min,ax.max), xlim = c(ax.min,ax.max))
  ax.ticks = (ax.max-ax.min)/5 
  if(has.WCS) {
    rot = 90
    ax.pos.x = par("usr")[1]-ax.ticks+(ax.ticks*0.15)
    axis.side = 2
  } else {
    rot  = -90
    ax.pos.x = par("usr")[2]+ax.ticks-(ax.ticks*0.15)
    axis.side = 4
  }
  ax.marks = round(seq(ax.min, ax.max, ax.ticks), digits = 1)
  
  text(ax.pos.x, ax.pos.y, y.lab, 
       xpd = T, srt = rot, cex = mag+0.2)
  
  axis(side = 1, at = seq(ax.min, ax.max, ax.ticks), 
       labels = ax.marks, cex.axis = mag-.1)
  axis(side = axis.side, at = seq(ax.min, ax.max, ax.ticks), 
       labels = ax.marks, las = 2, cex.axis = mag-.1)
  mtext(side = 1, text = "Alexandria Bay Water Level (IGLD m)", 
        line = 2.75, cex = mag+0.5)
  lines(x.line, S.reg, col = 2)
  r.s = r2
  p.v = p.val
  c(ax.max-ax.ticks)
  
  text(c(ax.max-ax.ticks), c(ax.min+ax.ticks*.1), r.s, cex = mag); 
  text(c(ax.max-ax.ticks), c(ax.min+ax.ticks*.3), p.v, cex = mag)
  dev.off()
  graphics.off()
}

######################################################
# Deferno Branch Main WL plot #
######################################################
# WLplotting = function(SWL, AWL, ax.min, ax.max,
#                       x.line, S.reg,
#                       y.lab, mag, WH, r2, p.val, 
#                       plot.name, has.WCS){
file.name = "ABay vs DB WL regression.png"
has.WCS = F
ABWL = DB.match$NOAA.WL
Site.WL = DB.match$Site.WL
ax.min = 74.25
ax.max = 75.50
WH = 700
r2 = paste("r-squared =",  round(DBr.sum$r.squared, 3))
p.val = paste("p-value < 0.001")
y.lab = "DB Water Level (m IGLD)"
WLplotting(Site.WL, ABWL, 74.25, 75.5,
           x, y.DBr,
           y.lab, mag, WH, r2, p.val, 
           file.name, has.WCS)

png("DB Water levels.png",
    width = 1500, height = 700)

par(mar = c(5,5,1,1), cex = 1.75)
DB.date = ymd(DB.match$Date[1:196], tz = "EST")

plot(DB.date,DB.match$Site.WL[1:196], type = "l",
     ylab = "", xlab = "")
mtext(side = 1, text = "Month", line = 3.75, cex = 2.25)
mtext(side = 2, text = "meters IGLD", line = 3.75, cex = 2.25)
dev.off()
graphics.off()
#####################################################
# Carpenters Branch Main WL plot #
#####################################################
CB.lm = lm(CB.match$Site.WL~CB.match$NOAA.WL)
CB.sum = summary(CB.lm)

m.CB = as.numeric(CB.lm$coefficients[2])
b.CB = as.numeric(CB.lm$coefficients[1])
y.CB = m.CB*x + b.CB
y.CB = rep(NA, length(x))
# WLplotting = function(SWL, AWL, ax.min, ax.max,
#                       x.line, S.reg,
#                       y.lab, mag, WH, r2, p.val, 
#                       plot.name, has.WCS){
file.name = "ABay vs CB WL regression.png"
has.WCS = T
ABWL = CB.match$NOAA.WL
Site.WL = CB.match$Site.WL
ax.min = 74.25
ax.max = 75.75
WH = 700
r2 = paste("r-squared =", round(CB.sum$r.squared, 3))
p.val = paste("p-value > 0.001")
y.lab = "CB Water Level (m IGLD)"
WLplotting(Site.WL, ABWL, ax.min, ax.max,
           x, y.CB,
           y.lab, mag, WH, r2, p.val, 
           file.name, has.WCS)

CB.Date = ymd(CB.match$Date, tz = "EST")
CB.WL = CB.match$Site.WL
WL.data$WL
AB.D
require(lubridate)
png("Compare Water levels.png",
    width = 1500, height = 700)

par(mar = c(6,5,1,1), cex = 1.75)

plot(CB.Date, CB.WL, type = "l",
     ylab = "", xlab = "", 
     xaxt = "n", yaxt = "n",
     ylim = c(74, 76), lwd = 2)
# CB.MY = my(CB.Date)
lines(AB.D, WL.data$WL, col = 6, lwd = 2)
lines(DB.date, DB.match$Site.WL[1:196], col = 4, lwd = 2)

xticks = CB.Date[seq(1, length(CB.Date), length.out = 20)]
x.m = month(CB.Date[seq(1, length(CB.Date), length.out = 20)], label = T)
x.y = substr(year(CB.Date[seq(1, length(CB.Date), length.out = 20)]), 3, 4)

x.dates = paste(x.m, "-", x.y, sep = "")
y.ticks = seq(74, 76, length.out = 10)
y.ax.labs = round(seq(74, 76, length.out = 10), digits = 1)
axis(side = 2, at = y.ticks, labels = y.ax.labs, las = 2)
axis(side = 1, at = xticks, labels = x.dates, las = 2)
mtext(side = 1, text = "Month", line = 4.5, cex = 2.5)
mtext(side = 2, text = "meters IGLD", line = 3.5, cex = 2.5)

dev.off()
graphics.off()

####################################################
# Dorr Road Main WL plot #
####################################################
# WLplotting = function(SWL, AWL, ax.min, ax.max,
#                       x.line, S.reg,
#                       y.lab, mag, WH, r2, p.val, 
#                       plot.name, has.WCS){
file.name = "ABay vs DR WL regression.png"
has.WCS = F
ABWL = DR.replot$NOAA.WL
Site.WL = DR.replot$Site.WL
ax.min = 74.2
ax.max = 74.8
WH = 700
r2 = paste("r-squared =", round(DR.sum$r.squared, 3))
p.val = paste("p-value < 0.001")
y.lab = "DR Water Level (m IGLD)"
WLplotting(Site.WL, ABWL, ax.min, ax.max,
           x, y.DR,
           y.lab, mag, WH, r2, p.val, 
           file.name, has.WCS)


########################################################
# Butterfield Main WL plot #
########################################################
lm.BF = lm(BF.match$Site.WL~BF.match$NOAA.WL)
BF.sum = summary(lm.BF)
y.BF = rep(NA, length(x))


file.name = "ABay vs BF WL regression.png"
has.WCS = T
ABWL = BF.match$NOAA.WL
Site.WL = BF.match$Site.WL
ax.min = 74.2
ax.max = 75.2
WH = 700
r2 = paste("r-squared = ", round(BF.sum$r.squared, 3), ".00", sep = "")
p.val = paste("p-value > 0.001")
y.lab = "BF Water Level (m IGLD)"
WLplotting(Site.WL, ABWL, ax.min, ax.max,
           x, y.BF,
           y.lab, mag, WH, r2, p.val, 
           file.name, has.WCS)
########################################################
# Delaney Branch Main WL plot #
########################################################
lm.DL = lm(DL.match$NOAA.WL~DL.match$Site.WL)
DL.sum = summary(lm.DL)
y.DL = rep(NA, length(x))
# WLplotting = function(SWL, AWL, ax.min, ax.max,
#                       x.line, S.reg,
#                       y.lab, mag, WH, r2, p.val, 
#                       plot.name, has.WCS){
file.name = "ABay vs DL WL regression.png"
has.WCS = T
ABWL = DL.match$NOAA.WL
Site.WL = DL.match$Site.WL
ax.min = 74.2
ax.max = 75.6
WH = 700
r2 = paste("r-squared =", round(DL.sum$r.squared, 3))
p.val = paste("p-value > 0.001")
y.lab = "DL Water Level (m IGLD)"
WLplotting(Site.WL, ABWL, ax.min, ax.max,
           x, y.CB,
           y.lab, mag, WH, r2, p.val, 
           file.name, has.WCS)
#############################################################
# plot ABay #
#############################################################

AB.D = ymd(WL.data$Dates, tz = "EST")
Y.D = year(AB.D)

year.labs = unique(Y.D)

png("ABay Water levels.png",
    width = 1500, height = 700)
par(mar = c(5.5, 5.5, 1, 1), cex = 1.5)
AB.D = ymd(WL.data$Dates, tz = "EST")
plot(AB.D, WL.data$WL, type = "l", lwd = 2, 
     ylab = "", xlab = "",
     yaxt = "n", xaxt = "n")

x.ticks = seq(AB.D[1], AB.D[length(AB.D)], length.out = 25)

int = length(x.ticks)

y.ticks = seq(max(WL.data$WL, na.rm = T), 
              min(WL.data$WL, na.rm = T), 
              length.out = 10)
y.ax.labs = round(seq(max(WL.data$WL, na.rm = T), 
                      min(WL.data$WL, na.rm = T), 
                      length.out = 10), digits = 1)

x.ax.labs = round(seq(Y.D[1], Y.D[length(Y.D)], length.out = int),digits = 0)
axis(side = 1, at = x.ticks, labels = x.ax.labs, las = 2, cex.axis = 1.25)
axis(side = 2, at = y.ticks, labels = y.ax.labs, las = 2, cex.axis = 1.25)
mtext(side = 1, line = 3.75, text = "Year", cex = 2.5)
mtext(side = 2, line = 3.75, text = "meters IGLD", cex = 2.5)

dev.off()
graphics.off()
#############################################################
# put Linear Regression parameters into a csv spread sheet #
# Below is the format of the .csv spread sheet             #
#############################################################
#             DB     CC     DR
# ABay.Slope 
# Intercept
# y.values

cols = rep(0, 2)

lm.parms = matrix(0, nrow = 2, ncol = 3)

rownames(lm.parms) = c("ABay.slope", "Intercept")
colnames(lm.parms) = c("DB", "CC", "DR")

lm.parms[,1] = DBr.parms
lm.parms[,2] = CC.parms
lm.parms[,3] = DR.parms

setwd("C:/Users/Alex Looi/Desktop/Watersheds")

# write.table(lm.parms, file = "Waterlevel lm parameters.csv", 
#             sep = ",", col.names = NA)

