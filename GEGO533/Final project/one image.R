## Define the function to calculate NDVI from land8 image
ndvCalc <- function(x) {
  ndvi <- (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
  return(ndvi)
}
ndvOver <- function(x, y) {
  ndvi <- (y - x) / (x + y)
  return(ndvi)
}
## Define the function to get the vegetation area and plot it
getVegetation <- function(x){
  x[x < 0.2] <- NA
  return(x)
}
## ## Define the function to calculate the dense vegetation area
getArea <- function(x){
  return(sum(x[x>=0.6])*res(x)[1]^2)
}

if(!require(raster)){install.packages("raster")}
library(raster)
if(!require(rgdal)){install.packages("rgdal")}
library(rgdal)
if(!require(UScensus2010)) {install.packages("UScensus2010")}
library(UScensus2010)
if(!require(UScensus2010county)) {install.county("osx")}
library(UScensus2010county)

#loading land_2017
setwd("G:/R/FinalProject/data/land8_2017")
getwd()
list <- list.files(pattern= "*.TIF",full.names=TRUE)
list[4:10]
band_2017 <- stack(list[4:9])
names(band_2017)
plot(band_2017)

#loading land_2016
setwd("G:/R/FinalProject/data/land8_2015")
getwd()
list <- list.files(pattern= "*.TIF",full.names=TRUE)
list[4:10]
band_2016 <- stack(list[4:9])
names(band_2016)
plot(band_2016)


###plotRGB(band_2017, 2,3,4)###############################

## get the broome county shapefile
data("new_york.county10")
shp_NY <- new_york.county10
df_NY <- shp_NY@data
broome <- df_NY$NAME10 == "Broome"
shp_Broome <- shp_NY[broome,]
plot(shp_Broome)
shp_Broome

## reproject shapefile to landsat8
shp_BroomeUTM <- spTransform(shp_Broome, CRS(proj4string(band_2017)))
shp_BroomeUTM 

## mask
###  the band_2017
band_2017_broome <- mask(band_2017, shp_BroomeUTM)
band_2017_broome
plot(band_2017_broome,ext = shp_BroomeUTM)
names(band_2017_broome)
#ndvi_2017 <- calc(x=band_2017, fun=ndvCalc)



## calculate NDVI 
### NDVI 2017 in Broome using overlay()
ndvi_2017 <- overlay(x=band_2017_broome[[3]], y=band_2017_broome[[4]], fun=ndvOver)
plot(ndvi_2017,ext = shp_BroomeUTM)


## click to get the value of a location 
### 2017NDVI
values <- click(ndvi_2017,n=1,xy=F,show=T)
plot(as.vector(values), type="b")


## extract the NDVI > 0.2 which is sparse vegetation and dense vegetation
### 2017NDVI 
ndvi_2017[ndvi_2017 < 0.2 ] <- NA
plot(ndvi_2017,ext = shp_BroomeUTM)


## calculate the area of dense vegetation
### 2017NDVI
vegetation_2017 <- ndvi_2017[ndvi_2017 >= 0.6]
Dveg_Area_2017 <- sum(vegetation_2017)*res(ndvi_2017)[1]^2
Dveg_Area_2017
hist(ndvi_2017,xlab="NDVI" ,main="NDVI values of vegetation")











