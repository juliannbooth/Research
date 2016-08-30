#THIRD SCRIPT

#We used this code to create our data in csv form in order to run regression
#analysis on it. 

#Creating a database: See Nina's codes for Temperature data frame. This is just Precipitation.


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
