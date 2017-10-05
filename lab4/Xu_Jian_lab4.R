## Question 1
### Part a
pnorm(20, 32, 7) + dnorm(20, 32, 7)
#### Plot the density function
cord.x <- c(11, seq(11, 20, 0.01), 20)
cord.y <- c(0,dnorm(seq(11, 20, 0.01), 32,7),0)
curve(dnorm(x, 32, 7), xlim = c(11, 53), 
      main = "A team scoring 20 points or fewer")
polygon(cord.x, cord.y, col = "red")

### Part b
pnorm(35, 32, 7, lower.tail = F)
#### Plot the density function
cord.x <- c(35, seq(35,53,0.01), 53)
cord.y <- c(0, dnorm(seq(35, 53, 0.01), 32, 7), 0)
curve(dnorm(x, 32, 7), xlim = c(11, 53), 
      main = "A team scoring more than 35 points")
polygon(cord.x, cord.y, col = "blue")

### Part c
pnorm(40,32,7) - pnorm(20, 32, 7)
#### Plot the density function
cord.x <- c(20, seq(20,40,0.01), 40)
cord.y <- c(0, dnorm(seq(20, 40, 0.01), 32, 7), 0)
curve(dnorm(x, 32, 7), xlim = c(11, 53), 
      main = "A team scoring between 20 and 40 points")
polygon(cord.x, cord.y, col = "yellow")


## Question 2
### Part a
pexp(3,0.1)
#### Plot the density function
curve(dexp(x,0.1), xlim = c(0, 50), lwd = 2)
cord.x <- c(0, seq(0, 3, 0.01), 3)
cord.y <- c(0,dexp(seq(0,3,0.01), 0.1),0)
polygon(cord.x, cord.y, col = "blue")

### Part b
pexp(20,0.1,F)
#### Plot the density function
curve(dexp(x,0.1), xlim = c(0, 50), lwd = 2)
cord.x <- c(20, seq(20, 50, 0.01), 50)
cord.y <- c(0,dexp(seq(20,50,0.01), 0.1),0)
polygon(cord.x, cord.y, col = "pink")

### Part c
pexp(10, 0.1) - pexp(5, 0.1)
#### Plot the density function
curve(dexp(x,0.1), xlim = c(0, 50), lwd = 2)
cord.x <- c(5, seq(5, 10, 0.01), 10)
cord.y <- c(0,dexp(seq(5,10,0.01), 0.1),0)
polygon(cord.x, cord.y, col = "black")


## Question 3
library(raster)
### Part a
ras <- raster(nrows = 30, ncols = 30, xmn = 0,
              xmx = 30, ymn = 0, ymx = 30)
ras 
set.seed(100)
ras[] <- runif(ncell(ras))
plot(ras)
hist(ras)

### Part b
ras2 <- raster(nrows = 30, ncols = 30, xmn =0,
               xmx = 30, ymn = 0, ymx = 30)
ras2
set.seed(99)
ras2[] <- rnorm(ncell(ras2))
plot(ras2)
hist(ras2)

### Part c
ras3 <- ras + ras2
plot(ras3)
hist(ras3)

### Part d
if(!require(rgdal)) install.packages("rgdal")
library(rgdal)
mean_R3 <- cellStats(ras3, "mean")
print(mean_R3)
ras3[ras3 > mean_R3] <- 1
ras3[ras3 <= mean_R3] <- 0
plot(ras3)
writeRaster(ras3, filename = "ras3_reclassify.tiff", overwrite = TRUE)

