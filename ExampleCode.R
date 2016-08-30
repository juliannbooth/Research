#---------------------------------------
#-------------------------------------
#OBSERVED VALUES
#--------------------------------------
#------------------------------------
library(ncdf4)
library(RNetCDF)
library(RColorBrewer)
library(SDMTools)


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

#--------------------------------------------------------------------------
#PRECIPITATION CMIP5 MODEL EXTRACTIONS
#----------------------------------------------------------------------------


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

#-----------------------------------------------------------------
#VISUALIZING THE CMIP5 MODELS
#-----------------------------------------------------------------



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

#add in the observed
temp.slice = prcp[,,595]
image(lon, lat, temp.slice, col=rev(brewer.pal(9,"Blues")))
title("Observed")

#reset the plot screen to normal:
dev.off()

#-------------------------------------------------------------------------
#We used this code to create our data in csv form in order to run regression
#analysis on it. 


#------------------------------------------------------------------------
#Precipitation dataframe--------------------------------------------------
#------------------------------------------------------------------------
r = length(time)*length(lat)*length(lon) 
# we can use this instead
#((length(time)-1)*length(longitude)*length(latitude)) + ((length(latitude)-1)*length(longitude)) + length(longitude)
c = 38
data = matrix(0, r, c)
for(x in 1:length(time)){ #time
  for(y in 1:length(lon)){ #longitude
    for(z in 1:length(lat)){ #latitude
      i = ((x-1)*length(lon)*length(lat)) + ((z-1)*length(lon)) + y
      data[i,1] = prcp[y,z,x]
      data[i,2:35] = prcp1[y,z,x,1:34]
      data[i,36] = lat[z]
      data[i,37] = lon[y]
      data[i,38] = time[x]
    }
  }
}


colnames(data) = list('orig','acc101', 'acc131', 'bcc111', 'bcc11m1'
                      , 'cane', 'ccsm41', 'cesbgc', 'cesm1', 'ccmcc',
                      'cnrm', 'csiro', 'fgoals', 'fio', 'gfcm', 'gfesm2g',
                      'gfesm2m', 'gis2h', 'gis2r1', 'gis2rcc', 'hagao', 'hagcc',
                      'hages', 'inm', 'ipsl5alr', 'isplmr', 'ipsl5blr', 'miroc1',
                      'mirochem', 'miroc5', 'mpilr', 'mpimr', 'mri',
                      'noresmm', 'noresmme', 'lat', 'long', 't')


prcpData = data.frame(data)

#somehow need to get rid of all the na's in x
naData = data
naData = data.frame(naData)
naData=na.omit(naData)

write.csv(naData, file = "PrcpData.csv", row.names=FALSE)

#-------------------------------------------------------------------
#Adding columns for month and year from time variable
#-------------------------------------------------------------------
#This script was used for making our data more compatible with Tableau. However, we also used this
#ModifiedData in our data mining analysis because we liked having a column with the month names and the specific year
#rather than the time variable that we were given.

#In this script, we added a month column and a year column to our data. Note, the year code is
#much more efficient that the month code. The month code takes a while to run because of the loops.
#However, it did what was necessary and we only had to use it twice, once for prcp and once for temp
#that we didn't spend much more time making it more efficient.

#At the end, we created a CSV that included our models testing predictions. We then used this CSV
#file in Tableau.


times <- as.factor(prcpData$t)
is.factor(times)
timelevels <- levels(times)

month <- rep(1,nrow(prcpData))
PrcpData <- cbind(month, prcpData)
monthlevel <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                "October", "November", "December")

#This is how I added the months. Not very efficient and took some time but it worked.

#need a january and february... vector of time levels
january <- 1
for (i in 0:49){
  if (i==0){
    january[1] <- timelevels[1]}
  else{
    january[i+1] <- timelevels[12*i+1]}
}

#placing January in data:
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(january)){
    if (PrcpData$t[i] == january[j]){
      PrcpData$month[i] <- "January"
    }
  }
}

february <- 1
for (i in 0:49){
  if (i==0){
    february[1] <- timelevels[2]}
  else{
    february[i+1] <- timelevels[12*i+2]}
}

#placing February in data:
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(february)){
    if (PrcpData$t[i] == february[j]){
      PrcpData$month[i] <- "February"
    }
  }
}

march <- 1
for (i in 0:49){
  if (i==0){
    march[1] <- timelevels[3]}
  else{
    march[i+1] <- timelevels[12*i+3]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(march)){
    if (PrcpData$t[i] == march[j]){
      PrcpData$month[i] <- "March"
    }
  }
}

april<- 1
for (i in 0:49){
  if (i==0){
    april[1] <- timelevels[4]}
  else{
    april[i+1] <- timelevels[12*i+4]}
}

for (i in 1:nrow(PrcpData)){
  for (j in 1:length(april)){
    if (PrcpData$t[i] == april[j]){
      PrcpData$month[i] <- "April"
    }
  }
}

may<- 1
for (i in 0:49){
  if (i==0){
    may[1] <- timelevels[5]}
  else{
    may[i+1] <- timelevels[12*i+5]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(may)){
    if (PrcpData$t[i] == may[j]){
      PrcpData$month[i] <- "May"
    }
  }
}

june<- 1
for (i in 0:49){
  if (i==0){
    june[1] <- timelevels[6]}
  else{
    june[i+1] <- timelevels[12*i+6]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(june)){
    if (PrcpData$t[i] == june[j]){
      PrcpData$month[i] <- "June"
    }
  }
}
july<- 1
for (i in 0:49){
  if (i==0){
    july[1] <- timelevels[7]}
  else{
    july[i+1] <- timelevels[12*i+7]}
}

for (i in 1:nrow(PrcpData)){
  for (j in 1:length(july)){
    if (PrcpData$t[i] == july[j]){
      PrcpData$month[i] <- "July"
    }
  }
}

aug<- 1
for (i in 0:49){
  if (i==0){
    aug[1] <- timelevels[8]}
  else{
    aug[i+1] <- timelevels[12*i+8]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(aug)){
    if (PrcpData$t[i] == aug[j]){
      PrcpData$month[i] <- "August"
    }
  }
}

sep<- 1
for (i in 0:49){
  if (i==0){
    sep[1] <- timelevels[9]}
  else{
    sep[i+1] <- timelevels[12*i+9]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(sep)){
    if (PrcpData$t[i] == sep[j]){
      PrcpData$month[i] <- "September"
    }
  }
}

oct<- 1
for (i in 0:49){
  if (i==0){
    oct[1] <- timelevels[10]}
  else{
    oct[i+1] <- timelevels[12*i+10]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(oct)){
    if (PrcpData$t[i] == oct[j]){
      PrcpData$month[i] <- "October"
    }
  }
}

nov<- 1
for (i in 0:49){
  if (i==0){
    nov[1] <- timelevels[11]}
  else{
    nov[i+1] <- timelevels[12*i+11]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(nov)){
    if (PrcpData$t[i] == nov[j]){
      PrcpData$month[i] <- "November"
    }
  }
}

dec<- 1
for (i in 0:49){
  if (i==0){
    dec[1] <- timelevels[12]}
  else{
    dec[i+1] <- timelevels[12*i+12]}
}
for (i in 1:nrow(PrcpData)){
  for (j in 1:length(dec)){
    if (PrcpData$t[i] == dec[j]){
      PrcpData$month[i] <- "December"
    }
  }
}

#---------------------------------------------------------------
#Years------------------------------------------------------------
#--------------------------------------------------------------

#goal: add column of year into our data set

years <- seq(1950,1999,1)
year <- rep(1,nrow(PrcpData))
PrcpData <- cbind(year, PrcpData)

trial2 <- seq(0,49,1)
floor(as.numeric(timelevels)/365.25)

for (i in 1:nrow(PrcpData)){
  for (j in 1:50){
    if (floor(as.numeric(PrcpData$t[i])/365.25) == trial2[j]){
      PrcpData$year[i] <- years[j]
    }
  }
}

#------------------------------------------------------------
#------Multiply Prcp by 30 Because we need it in rates------
#-----------------------------------------------------------



monthlyPrcp <- PrcpData
monthlyPrcp[1,c(3:37)] #rows we want to multiply by 30

monthlyPcrp1 <- 30*monthlyPrcp[,c(3:37)]
for (i in 1:nrow(monthlyPcrp1)){
  for (j in 3:37){
    monthlyPrcp[i,j] <- monthlyPcrp1[i,j-2]
  }
}

#remove the ocean and Mexico values
monthlyPrcp <- na.omit(monthlyPrcp)

write.csv(monthlyPrcp, file = "modifiedPrcp.csv", row.names = FALSE)

#Now we needed to run some models and get unified testing results in order to visualize them in 
#Tableau. For Precipitation, we used weighted knn, random forest with 25 and 100, and linear.
#We did not run SVM simply for time constraints. The last time I ran it, it took 2 days. On top of that,
#I lost the model because my computer was shut off by a power shortage (something with the microwave, 
#toaster, and coffee maker all being plugged in the same place as my computer and all being utilized).
#Thus I thought the SVM for temperature would suffice for Tableau purposes. Obviously for the paper, I
#will need to run it again. It gave 93% accuracy.

#Unified test data

modifiedPrcp <- read.csv("~/modifiedPrcp.csv")
set.seed(78)


training <- sample(nrow(modifiedPrcp), round(0.7*(nrow(modifiedPrcp)),0))
train <- modifiedPrcp[training,]
test <- modifiedPrcp[-training,]
remove(modifiedPrcp) #for memory sake

library(randomForest)
library(hydroGOF)
library(kknn)

#weighted knn
yhat_kknn <- (kknn(orig~., train = train, test = test, k = 5, kernel = "optimal", distance = 2))$fitted.values
plot(test$orig, yhat_kknn, main = "KNN k=5, optimal kernel, Prcp", xlab = "Observed", ylab = "Predicted")

#random forest 25 trees
randomforest25 <- randomForest(orig~., data = train, ntree = 25, mtry = 12)
forestpred25 <- predict(randomforest25, newdata = test)

#random forest 100 trees
randomforest100 <- randomForest(orig~., data = train, ntree = 100, mtry=24)
forestpred100 <- predict(randomforest100, newdata = test)

#linear regression
simpleLinearModel1 <- lm(orig~., data = train) #gis2rcc, lat, lon
lm_pred <- predict(simpleLinearModel1, newdata = test)

#put them in a data set
datatest<- cbind(test, yhat_kknn, forestpred25, forestpred100, lm_pred)

write.csv(datatest, file = "testmodelsprcp.csv", row.names = FALSE)

#So testmodelsprcp.csv was used in Tableau.

#For memory purposes:
remove(datatest)
remove(test)
remove(train)