#FOURTH SCRIPT

#This script was used for making our data more compatible with Tableau. However, we also used this
#ModifiedData in our data mining analysis because we liked having a column with the month names and the specific year
#rather than the time variable that we were given. Thus I labeled it as the Fourth Script.

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
#will need to run it again. It gave 93% accuracy. See more on the Data Mining script.

#Unified test data

modifiedPrcp <- read.csv("C:/Users/000680344/Desktop/Compilation_Precipitation/modifiedPrcp.csv")
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
