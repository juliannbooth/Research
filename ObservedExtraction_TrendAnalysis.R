#FIRST SCRIPT

#TREND ANALYSIS OF TEXAS - FIRST OBSERVATIONS

#This file was our first observations of the Texas data. This data is completely in 
#netCDF format. There is no "data mining" algorithms, simply opening and reading and 
#trend analyzing the netCDF Precipitation and Temperature data for Texas. 

#The first half is for precipitation, the last half is for temperature.

#This was the trend analysis we presented to the Provost and President.

#Please note that many of these functions may have been placed in a function/loop however
#most of it was us improvising and trying different things, so they were not written more efficiently.
#Also, most of this was written in October.

#---------------------------------------
#-------------------------------------
#OBSERVED VALUES
#--------------------------------------
#------------------------------------

library(chron)
library(ncdf4)
library(RNetCDF)
library(RColorBrewer)
library(lattice)
library(fields)
library(maptools)
library(Kendall)
library(SDMTools)
library(maps)

#Note, the file must be in same directory as R project
filename <- 'Extraction_Prcp_TX.nc'
ncid <- open.nc(filename)
ncid <- read.nc(ncid)

prcp <- ncid$Prcp
dim(prcp) #19x16x600

lon <- ncid$longitude
lat <- ncid$latitude

time <- ncid$time

#CREATING A COLOR VISUALIZATION OF THE AREA USING TIME CONSTANT AT MARCH 1951 (m=15)
m<-15
prcp.slice <- prcp[,,m] #all lon, lat, and only the "m"th time index (t[m]) 
image(lon, lat,prcp.slice, col = rev(brewer.pal(9, "Blues")), main = "March 1951", xlab = "longitude", ylab = "latitude")

#LEGEND:
#pnts is the location of the legend
#Did not find a way to standardize the color scheme. Used Tableau instead 
#later in research so did not look into this further.
pnts <- cbind(x=c(lon[14], lon[16], lon[16], lon[14]), y = c(lat[15], lat[15], lat[14], lat[14]))
legend.gradient(pnts, cols = brewer.pal(9, "Blues"), limits = c("High", "Low"),title = "Legend")

#---------------------------------------------------------------------
#CREATING A MAP OF THE AREA--------------------------------------------
#----------------------------------------------------------------------
#step 1: create empty vectors
latitudevec = 1
longitudevec = 1
prcpvec = 1
#step 2: setting time constant at March 1951 (k=15)
k=15
#step 3: finding the prcp value for each latitude/longitude pair with constant time 15
m=1
for (i in 1:length(lon)){
  j=1
  while (j < length(lat)+1){
    longitudevec[m] = lon[i]
    latitudevec[m] = lat[j]
    prcpvec[m] = prcp[i,j,k]
    j=j+1
    m=m+1
  } 
}

#step 4: create the data frame
mydata <- cbind(longitudevec, latitudevec, prcpvec)
mydata <- data.frame(mydata)

#step 5: Use the map function to create the map of the area:
map("county", xlim = range(mydata$longitudevec), ylim = range(mydata$latitudevec))
map.axes()
title("South Texas region")

#step 6 (optional): generate a random sample of prcp values and place them 
#on the map
set.seed(17)
randsamp = sample(nrow(mydata), round(0.07*(nrow(mydata)),0))
text(mydata$longitudevec[randsamp], mydata$latitudevec[randsamp],signif(mydata$prcpvec[randsamp], digits=3))


#Once again, the code above has not been used recently because we found 
#a better map visualization option using Tableau

#-------------------------------------------------
#TREND ANALYSIS---------------------------------
#-------------------------------------------------
#Finding the January indexes. To explain, the January values are in the 
#1st, 13th, 25th, 37th, etc slots of the prcp array
jan = 0
for (i in 0:48){
  jan[i+1] = (i*12) + 1 
}

#Holding area constant, all the January precip values for the whole time series
JanPrecip = prcp[1,1,jan] 

#Plotting the January Precipitation Values:
x<- seq(1950, 1998,1)
plot(x, JanPrecip, type = 'l', main = 'Average Precipitation for January 1950-1998' )

#Extracting information for all 12 months:
feb <- 0
for (i in 0:48){
  feb[i+1] = (i*12) + 2 
}
FebPrecip <- prcp[1,1,feb]


march <- 0
for (i in 0:48){
  march[i+1] = (i*12) + 3 
}
MarchPrecip <- prcp[1,1,march]

april <- 0
for (i in 0:48){
  april[i+1] = (i*12) + 4
}
AprilPrecip <- prcp[1,1,april]

may <- 0
for (i in 0:48){
  may[i+1] = (i*12) + 5
}
MayPrecip <- prcp[1,1,may]

june <- 0
for (i in 0:48){
  june[i+1] = (i*12) + 6
}
JunePrecip <- prcp[1,1,june]

july <- 0
for (i in 0:48){
  july[i+1] = (i*12) + 7
}
JulyPrecip <- prcp[1,1,july]


aug <- 0
for (i in 0:48){
  aug[i+1] = (i*12) + 8
}
AugPrecip <- prcp[1,1,aug]

sep <- 0
for (i in 0:48){
  sep[i+1] = (i*12) + 9
}

oct <- 0
for (i in 0:48){
  oct[i+1] = (i*12) + 10
}

nov <- 0
for (i in 0:48){
  nov[i+1] = (i*12) + 11
}

dec <- 0
for (i in 0:48){
  dec[i+1] = (i*12) + 12
}

#---------------------------------------------------------------
#Random Analysis Options
#--------------------------------------------------------------

#finding average over all 48 years, whole area, month constant.
JulyPrecip1 <- prcp[,,july] 
dim(JulyPrecip1) #9x6x48
average <- mean(JulyPrecip1, na.rm=TRUE) #Note there are NA values for Ocean and Mexico

#--------------------------------------------------------------
#Trend Analysis for each Month
#--------------------------------------------------------------


#Finding the total sum of precipitation for whole area, just January, for 1950-1998.

precTotJan <- 0
letstry <- prcp[,,jan] #9x6x48
for (i in 1:49){
  precTotJan[i] <- sum(letstry[,,i], na.rm=TRUE)}

plot(x,precTotJan, main = "Total Jan. Precip. in South Texas")
MannKendall(precTotJan) #2-sided p-val is 0.0894


#Finding the total sum of precipitation for whole area, just February, for 1950-1998.
precTotFeb <- 0
letstry1 <- prcp[,,feb] #9x6x48
for (i in 1:49){
  precTotFeb[i] <- sum(letstry1[,,i], na.rm=TRUE)}

plot(x,precTotFeb, main = "Total February Precipitation Values (mm)")
MannKendall(precTotFeb) #2-sided pvalue =0.2969


#Finding the total sum of precipitation for whole area, just March, for 1950-1998.
precTotMar <- 0
letstry2 <- prcp[,,march] 
for (i in 1:49){
  precTotMar[i] <- sum(letstry2[,,i], na.rm=TRUE)}

plot(x,precTotMar, main = "Total March Precipitation Values")
MannKendall(precTotMar) #2-sided pvalue =0.18152


#Finding the total sum of precipitation for whole area, just April, for 1950-1998.
precTotApr <- 0
letstry3 <- prcp[,,april] #9x6x48
for (i in 1:49){
  precTotApr[i] <- sum(letstry3[,,i], na.rm=TRUE)}

plot(x,precTotApr, main = "Total April Precipitation Values")
MannKendall(precTotApr) #2-sided pvalue =0.54053


#Finding the total sum of precipitation for whole area, just May, for 1950-1998.
precTotMay <- 0
letstry4 <- prcp[,,may] #9x6x48
for (i in 1:49){
  precTotMay[i] <- sum(letstry4[,,i], na.rm=TRUE)}

plot(x,precTotMay, main = "Total May Precipitation Values")
MannKendall(precTotMay) #2-sided pvalue =0.4328


#Finding the total sum of precipitation for whole area, just June, for 1950-1998.
precTotJune <- 0
letstry5 <- prcp[,,june] #9x6x48
for (i in 1:49){
  precTotJune[i] <- sum(letstry5[,,i], na.rm=TRUE)}

plot(x,precTotJune, main = "Total June Precipitation")
MannKendall(precTotJune) #2-sided pvalue =0.67275


#Finding the total sum of precipitation for whole area, just July, for 1950-1998.
precTotJuly <- 0
letstry6 <- prcp[,,july] #9x6x48
for (i in 1:49){
  precTotJuly[i] <- sum(letstry6[,,i], na.rm=TRUE)}

plot(x,precTotJuly, main = "Total July Precipitation Values")
MannKendall(precTotJuly) #2-sided pvalue =0.35636


#Finding the total sum of precipitation for whole area, just August, for 1950-1998.
precTotAug <- 0
letstry7 <- prcp[,,aug] #9x6x48
for (i in 1:49){
  precTotAug[i] <- sum(letstry7[,,i], na.rm=TRUE)}

plot(x,precTotAug, main = "Total August Precipitation Values")
MannKendall(precTotAug) #2-sided pvalue =0.4328


#Finding the total sum of precipitation for whole area, just Sep, for 1950-1998.
precTotSep <- 0
letstry8 <- prcp[,,sep] #9x6x48
for (i in 1:49){
  precTotSep[i] <- sum(letstry8[,,i], na.rm=TRUE)}

plot(x,precTotSep, main = "Total September Precipitation Values")
MannKendall(precTotSep) #2-sided pvalue =0.97937


#Finding the total sum of precipitation for whole area, just Oct, for 1950-1998.
precTotOct <- 0
letstry9 <- prcp[,,oct] #9x6x48
for (i in 1:49){
  precTotOct[i] <- sum(letstry9[,,i], na.rm=TRUE)}

plot(x,precTotOct, main = "Total October Precipitation Values")
MannKendall(precTotOct) #2-sided pvalue =0.39346


#Finding the total sum of precipitation for whole area, just Nov, for 1950-1998.
precTotNov <- 0
letstry10 <- prcp[,,nov] #9x6x48
for (i in 1:49){
  precTotNov[i] <- sum(letstry10[,,i], na.rm=TRUE)}

plot(x,precTotNov, main = "Total November Precipitation Values")
MannKendall(precTotNov) #2-sided pvalue =0.6981


#Finding the total sum of precipitation for whole area, just Dec, for 1950-1998.
precTotDec <- 0
letstry11 <- prcp[,,dec] 
for (i in 1:49){
  precTotDec[i] <- sum(letstry11[,,i], na.rm=TRUE)}

plot(x,precTotDec, main = "Total December Precipitation Values")
MannKendall(precTotDec) #2-sided pvalue =0.031846


#-----------------------------------------------------------------------
#----------------------------------------------------------------------
#Trend Analysis per year
#---------------------------------------------------------------------
#---------------------------------------------------------------------



yearlytotals <- 0
for (i in 0:49){
  yearlytotals[i+1] <- sum(prcp[,,((i*12)+1):(((i*12)+1)+11)], na.rm=TRUE)
}
#Do [i+1] because R cannot start an index with 0

plot(seq(1950,1999,1),yearlytotals, main = "Yearly Total Precipitation (mm)", xlab = "Year", ylab = "Precipitation (mm)")
MannKendall(yearlytotals) #2-sided pval = 0.056496, tau = 0.187



#----------------------------------------------------------------
#------------------------------------------------------------------
#Trend Analysis per Decade
#----------------------------------------------------------------
#--------------------------------------------------------------

decadetotals <- 0 
n<-1
for (i in 1:5){
  decadetotals[i] <- sum(prcp[,,n:(i*12*10)], na.rm=TRUE)
  n <- (i*12*10)+1
}


plot(seq(1,5,1), decadetotals, main = "Total Decadal Precipitation (mm)", xlab = "Decades from 1950-2000", ylab = "Precipitation (mm)")
MannKendall(decadetotals) #2-sided pval is 0.46243, tau = 0.4


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#TEMPERATURES
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filename1 <- 'Extraction_Tavg_TX.nc'

ncid1 <- open.nc(filename1)
ncid1 <- read.nc(ncid1)

tavg <- ncid1$Tavg
#the other variables are the same (time, lon and lat)

#-------------------------------------------------
#Trend Analysis---------------------------------
#-------------------------------------------------



JulyTemp1 <- tavg[,,july] #whole area, just July precipitation values for 48 years
dim(JulyTemp1) #9x6x48
average1 <- mean(JulyTemp1, na.rm=TRUE) #average precipitation fall in July, across whole area, for all 48 years
 

#Finding the average temperatures for whole area, just January, for 1950-1998.
TempTotJan <- 0
letstry <- tavg[,,jan] #9x6x48
for (i in 1:48){
  TempTotJan[i] <- mean(letstry[,,i], na.rm=TRUE)}

x<- seq(1950, 1997)
plot(x,TempTotJan, main = "Average January Temperature in South Texas")
MannKendall(TempTotJan) #2-sided p-val is 0.12414


TempTotFeb <- 0
letstry1 <- tavg[,,feb] #9x6x48
for (i in 1:48){
  TempTotFeb[i] <- mean(letstry1[,,i], na.rm=TRUE)}

plot(x,TempTotFeb, main = "Average February Temperature")
MannKendall(TempTotFeb) #2-sided pvalue =0.90801



TempTotMar <- 0
letstry2 <- tavg[,,march] 
for (i in 1:48){
  TempTotMar[i] <- mean(letstry2[,,i], na.rm=TRUE)}

plot(x,TempTotMar, main = "Average March Temperature")
MannKendall(TempTotMar) #2-sided pvalue =0.74226



TempTotApr <- 0
letstry3 <- tavg[,,april] #9x6x48
for (i in 1:48){
  TempTotApr[i] <- mean(letstry3[,,i], na.rm=TRUE)}

plot(x,TempTotApr, main = "Average April Temperature")
MannKendall(TempTotApr) #2-sided pvalue =0.056014



TempTotMay <- 0
letstry4 <- tavg[,,may] #9x6x48
for (i in 1:48){
  TempTotMay[i] <- mean(letstry4[,,i], na.rm=TRUE)}

plot(x,TempTotMay, main = "Average May Temperature")
MannKendall(TempTotMay) #2-sided pvalue =0.43937



TempTotJune <- 0
letstry5 <- tavg[,,june] #9x6x48
for (i in 1:48){
  TempTotJune[i] <- mean(letstry5[,,i], na.rm=TRUE)}

plot(x,TempTotJune, main = "Average June Temperature")
MannKendall(TempTotJune) #2-sided pvalue =0.32385



TempTotJuly <- 0
letstry6 <- tavg[,,july] #9x6x48
for (i in 1:48){
  TempTotJuly[i] <- mean(letstry6[,,i], na.rm=TRUE)}

plot(x,TempTotJuly, main = "Average July Temperature")
MannKendall(TempTotJuly) #2-sided pvalue = 0.48258



TempTotAug <- 0
letstry7 <- tavg[,,aug] #9x6x48
for (i in 1:48){
  TempTotAug[i] <- mean(letstry7[,,i], na.rm=TRUE)}

plot(x,TempTotAug, main = "Average August Temperature")
MannKendall(TempTotAug) #2-sided pvalue =0.1576



TempTotSep <- 0
letstry8 <- tavg[,,sep] #9x6x48
for (i in 1:48){
  TempTotSep[i] <- mean(letstry8[,,i], na.rm=TRUE)}

plot(x,TempTotSep, main = "Average September Temperature")
MannKendall(TempTotSep) #2-sided pvalue =0.38861



TempTotOct <- 0
letstry9 <- tavg[,,oct] #9x6x48
for (i in 1:48){
  TempTotOct[i] <- mean(letstry9[,,i], na.rm=TRUE)}

plot(x,TempTotOct, main = "Average October Temperature")
MannKendall(TempTotOct) #2-sided pvalue =0.52801



TempTotNov <- 0
letstry10 <- tavg[,,nov] #9x6x48
for (i in 1:48){
  TempTotNov[i] <- mean(letstry10[,,i], na.rm=TRUE)}

plot(x,TempTotNov, main = "Average November Temperature", ylab = "Average Temperature, degrees Celsius", xlab = "Year")
abline(lm(TempTotNov~x), col = "red")
MannKendall(precTotNov) #2-sided pvalue =0.0391, tau = 0.209



TempTotDec <- 0
letstry11 <- tavg[,,dec] 
for (i in 1:48){
  TempTotDec[i] <- mean(letstry11[,,i], na.rm=TRUE)}

plot(x,TempTotDec, main = "Average December Temperature")
MannKendall(TempTotDec) #2-sided pvalue =0.62495


#-----------------------------------------------------------------------
#----------------------------------------------------------------------
#Look at totals per YEAR
#---------------------------------------------------------------------
#---------------------------------------------------------------------



yearlytotals <- 0
for (i in 0:49){
  yearlytotals[i+1] <- mean(tavg[,,((i*12)+1):(((i*12)+1)+11)], na.rm=TRUE)
}
#Do [i+1] because R cannot start an index with 0

plot(seq(1950,1999,1),yearlytotals, main = "Yearly Average Temperature", xlab = "Year", ylab = "Average Temperature, degrees Celsius")
MannKendall(yearlytotals) #2-sided pval = 0.72535, tau = 0.0351


yearlytotals <- 0
for (i in 24:49){
  yearlytotals[i+1] <- mean(tavg[,,((i*12)+1):(((i*12)+1)+11)], na.rm=TRUE)
}
#Do [i+1] because R cannot start an index with 0




