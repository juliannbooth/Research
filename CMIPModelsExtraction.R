#SECOND SCRIPT

#-----------------------------------------------
#-----------------------------------------------
#MODELS - NEED TO BE IN SAME PROJECT AS TEXAS.R
#------------------------------------------------
#--------------------------------------------

#PRECIPITATION MODEL EXTRACTIONS
library(ncdf4)
library(RNetCDF)

filename2 <- 'Extraction_pr.nc'

ncid2 <- open.nc(filename2)
ncid2<- read.nc(ncid2)

time1 <- ncid2$time

lon1 <- ncid2$longitude
lon1 <- lon1-360
nlon1 <- dim(lon1) #19
head(lon1)

lat1 <- ncid2$latitude
nlat1 <- dim(lat1) #16


prcp1 <- ncid2$pr
dim(prcp1) #19 x 16 x 600 x 34 lon, lat, time, projection


prcp1[1,1,1,] #model values for first lon, lat, time. 34 models

#-----------------------------------------------------------
#TEMPERATURE MODEL EXTRACTIONS------------------------------
#-----------------------------------------------------------

filename3 <- 'Extraction_tas.nc'
ncid3 <- open.nc(filename3)
ncid3 <- read.nc(ncid3)

longitude1 <- ncid3$longitude
longitude1 <- longitude1-360
latitude1 <- ncid3$latitude
tas <- ncid3$tas
time2 <- ncid3$time
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#RANDOM HISTOGRAM ANALYSES - just improvising as we looked at our data for the first time.
#---------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
error = 1
for (i in 1:length(prcp1[1,1,2,])){
  error[i] = prcp[1,1,2]-prcp1[1,1,2,i]
} #like our residuals. actual - observed

sqerror = error**2
which.min(sqerror) #13 so model 13 best represents this specific observed value

#-----------------------------------
#latitude = 25.9375, longitude = -99.5625...260.4375
error = 1
bestmodel = 1
for(j in 1:500){
  for (i in 1:length(prcp1[1,1,j,])){
    error[i] = prcp[1,1,j]-prcp1[1,1,j,i]
  }
  sqerror = error**2
  bestmodel[j] = which.min(sqerror)
} #like our residuals. actual - observed
hist(x=bestmodel, breaks = 34, main = "lon[1], lat[1], all 50 years")

#latitude = 26.0625, longitude = -99.5625...260.4375
error = 1
bestmodel = 1
for(j in 1:500){
  for (i in 1:length(prcp1[1,2,j,])){
    error[i] = prcp[1,2,j]-prcp1[1,2,j,i]
  }
  sqerror = error**2
  bestmodel[j] = which.min(sqerror)
} #like our residuals. actual - observed
hist(x=bestmodel, breaks = 34, main = "lon[1], lat[2], all 50 years")

#latitude = 26.1875, longitude = -99.5625...260.4375
error = 1
bestmodel = 1
for(j in 1:500){
  for (i in 1:length(prcp1[1,3,j,])){
    error[i] = prcp[1,3,j]-prcp1[1,3,j,i]
  }
  sqerror = error**2
  bestmodel[j] = which.min(sqerror)
} #like our residuals. actual - observed
hist(x=bestmodel, breaks = 34, main = "lon[1], lat[3], all 50 years")


#------------------------------------------------------
#hold longitude constant at [1] (longitude = -99.5625...260.4375)

#recall prcp are the observed values, prcp1 are the 34 predicted values
#m is latitude index
#j is time index
#i is model index
error = 1
bestmodel = 1
for (m in 1:length(lat)){
  for(j in 1:500){
    for (i in 1:length(prcp1[1,m,j,])){
     error[i] = prcp[1,m,j]-prcp1[1,m,j,i]
    }
    sqerror = error**2
    bestmodel[j] = which.min(sqerror)
  }
  hist(x=bestmodel, breaks = 34, ylim = range(1,35), main = paste("(",lon[1],", ", lat[m],")"))
}

#doing the total precipitation (multiplying it by number of days in January)
error=1
for (i in 1:length(prcp1[1,1,1,])){
  error[i] = 31*prcp[1,1,1] - 31*prcp1[1,1,1,i]}
sqerror = error**2
#-----------------------------------------------------------------
#VISUALIZING THE MODELS
#-----------------------------------------------------------------

library(RColorBrewer)

names = c('access1-0.1', 
          'access1-3.1',    # only pr, tas (tasmin, tasmax not available)
          'bcc-csm1-1.1', 
          'bcc-csm1-1-m.1', 
          'canesm2.1', 
          'ccsm4.1', 
          'cesm1-bgc.1', 
          'cesm1-cam5.1', 
          'cmcc-cm.1', 
          'cnrm-cm5.1', 
          'csiro-mk3-6-0.1', 
          'fgoals-g2.1',
          'fio-esm.1', 
          'gfdl-cm3.1', 
          'gfdl-esm2g.1', 
          'gfdl-esm2m.1', 
          'giss-e2-h-cc.1', 
          'giss-e2-r.1', 
          'giss-e2-r-cc.1', 
          'hadgem2-ao.1', 
          'hadgem2-cc.1', 
          'hadgem2-es.1', 
          'inmcm4.1', 
          'ipsl-cm5a-lr.1', 
          'ipsl-cm5a-mr.1', 
          'ipsl-cm5b-lr.1', 
          'miroc-esm.1', 
          'miroc-esm-chem.1', 
          'miroc5.1', 
          'mpi-esm-lr.1', 
          'mpi-esm-mr.1', 
          'mri-cgcm3.1', 
          'noresm1-m.1', 
          'noresm1-me.1'    # only pr, tas (tasmin, tasmax not available)
)

#note: these are all rcp45 models

par(mfrow=c(3,3),mai=c(.3,.7,.7,.36)) #setting parameters of screen to show 9 plots
for(i in 1:34){
  temp.slice = prcp1[,,595,i]
  image(lon, lat,temp.slice, col = rev(brewer.pal(9, "Blues")), ylab = "latitude", xlab = "longitude")
  title(names[i])
}
  
temp.slice = prcp[,,595]
image(lon, lat, temp.slice, col=rev(brewer.pal(9,"Blues")))
title("Observed")

#reset the plot screen to normal:
dev.off()
