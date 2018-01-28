# Water Volume Calculations #

# Saturated Core Pore water Volume #

## Parameters in (cm) ##
########################

Core = 30 # Core Depth
D = 6*2.54 # Diameter of Core; 6inches * 2.54cm/inch

# Pore Water Volume #
Pore.W = pi*Core*(D/2)^2
A = pi*(D/2)^2

# Surface Water Volume #
Day = seq(1, 15, 1)
Level = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 20, 20, 20, 20)
Tot.Vol = rep(0, 15)
Use.Vol = rep(0, 15)
Fill.Vol = rep(0, 15)

#Calculating volume
Tot.Vol = Level*A

Use.Vol = Level*A*0.25

Fill.Vol = (Use.Vol)+(Tot.Vol[2:15]-Tot.Vol[1:14])
Fill.Vol[15] = 0.00

Surf.Vol = data.frame(Day, Level, Tot.Vol, Use.Vol, Fill.Vol)

write.csv(Surf.Vol, file = "C:/Users/LooiXIV/Desktop/Surface.csv")

# adding water evenly through the two weeks #

x = 20/14

Level.2 = seq(0, 20, by = x)
Tot.Vol.2 = rep(0, 15)
Use.Vol.2 = rep(0, 15)
Fill.Vol.2 = rep(0, 15)

Tot.Vol.2 = Level.2*A

Use.Vol.2 = Level.2*A*0.25

Fill.Vol.2 = (Use.Vol.2)+(Tot.Vol.2[2:15]-Tot.Vol.2[1:14])
Fill.Vol.2[15] = 0.00

Surf.Vol.2 = data.frame(Day, Level.2, Tot.Vol.2, Use.Vol.2, Fill.Vol.2)

write.csv(Surf.Vol, file = "C:/Users/LooiXIV/Desktop/Surface2.csv")
