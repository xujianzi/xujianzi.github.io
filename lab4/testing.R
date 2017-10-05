if(!require(raster)) install.packages("raster")
library(raster)
ras <- raster(nrows=10,ncols=10,xmn=0,xmx=10,ymn=0,ymx=10)
ras
ras[] <- runif(ncell(ras))
plot(ras)
