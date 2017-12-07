## The data are landsat8 June to Augest one day image from 2013 to 2017

## Install and loading library
if(!require(raster)){install.packages("raster")}
library(raster)
if(!require(rgdal)){install.packages("rgdal")}
library(rgdal)
if(!require(UScensus2010)) {install.packages("UScensus2010")}
library(UScensus2010)
if(!require(UScensus2010county)) {install.county("osx")}
library(UScensus2010county)
if(!require(knitr)){install.packages("knitr")}
library(knitr)



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


## get work directory and list file
listf <- list()
listf[1] <- "G:/R/FinalProject/data/land8_2017"
listf[2] <- "G:/R/FinalProject/data/lan8_2016"
listf[3] <- "G:/R/FinalProject/data/land8_2015"
listf[4] <- "G:/R/FinalProject/data/land8_2014"
listf[5] <- "G:/R/FinalProject/data/land8_2013"
listp <- list()
bandcom <- list()
for(i in 1:5){
  setwd(listf[[i]])
  getwd()
  listp[[i]] <- list.files(pattern= "*.TIF",full.names=TRUE)
  bandcom[[i]] <- stack(listp[[i]][4:9])
}
#### show landsat8 image in 2017 as an example
bandcom[[1]]
names(bandcom[[1]])
plot(bandcom[[1]])


## reproject the Rasster datayer into one Projection(including extent)
newbandcom <- list()
for(i in 1:5){
  newbandcom[[i]] <-  projectRaster(bandcom[[i]],bandcom[[1]])
}


## get the broome county shapefile
data("new_york.county10")
shp_NY <- new_york.county10
df_NY <- shp_NY@data
broome <- df_NY$NAME10 == "Broome"
shp_Broome <- shp_NY[broome,]
plot(shp_Broome)
shp_Broome


## reproject shapefile to landsat8
shp_BroomeUTM <- spTransform(shp_Broome, CRS(proj4string(bandcom[[1]])))
shp_BroomeUTM 


## mask
band_broome <- list()
for(i in 1:5){
  band_broome[[i]] <- mask(newbandcom[[i]],shp_BroomeUTM)
}
#### use 2016 images as an example
plot(band_broome[[2]],ext = shp_BroomeUTM)
names(band_broome[[2]])


## calculate NDVI in Broome using overlay()
ndvi <- list()
for(i in 1:5){
  ndvi[[i]] <- overlay(x=band_broome[[i]][[3]], y=band_broome[[i]][[4]], fun=ndvOver)
}


## extract the NDVI > 0.2 which is sparse vegetation and dense vegetation
vegetation <- list()
for(i in 1:5){
  vegetation[[i]] <- getVegetation(ndvi[[i]])
}



## calculate the area of dense vegetation
Dveg_Area <- list()
for(i in 1:5){
  Dveg_Area[i] <- getArea(ndvi[[i]])
}
area <- as.numeric(Dveg_Area)
year <- c("2017","2016","2015","2014","2013")
df <- data.frame(year,area)
plot(df)


## Display ndvi plot and hist
ndvi_show <- brick(ndvi[[1]],ndvi[[2]],ndvi[[3]],ndvi[[4]],ndvi[[5]])
plot(ndvi_show,ext = shp_BroomeUTM)
hist(ndvi_show)

## Display vegetation
vege_show1 <- brick(vegetation[[1]],vegetation[[2]],vegetation[[3]])
plot(vege_show1,ext = shp_BroomeUTM)
hist(vege_show1)
vege_show2 <- brick(vegetation[[4]],vegetation[[5]])
plot(vege_show2,ext = shp_BroomeUTM)
hist(vege_show2)

