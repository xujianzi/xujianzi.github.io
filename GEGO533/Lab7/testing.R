## Q1-1
### a
income <- c(30,28,52,40,35)
edu <- c(12,13,18,16,17)
df <- data.frame(income,edu)
result <- cor.test(df$income,df$edu,method = "pearson")
result
result$estimate
### b
if(result$p.value > 0.05){
  print("we can't reject non hypothesis")
} else{
  print("we reject non hypothesis")
}
### c
result_c <- cor.test(income,edu,method = "spearman")
result_c
result_c$estimate
### d
if(result$p.value > 0.05){
  print("we can't reject non hypothesis")
} else{
  print("we reject non hypothesis")
}
## the rs from part(c) is not significantly diferent form zero

## Q2-3
n1 <- 36
n2 <- 80
critical_t1 <- qt(0.975, df = n1-2)
r1 <- critical_t1/sqrt(n1-2+critical_t1^2)
r1
critical_t2 <- qt(0.975, df = n2-2)
r2 <- critical_t2/sqrt(n2-2+critical_t2^2)
r2


## Q3-4
x <- c(2,8,9,7)
y <- c(6,6,10,4)
result3 <- cor.test(x,y,method = "pearson")
result3
if(result3$p.value > 0.05){
  print("we can't reject non hypothesis")
} else{
  print("we reject non hypothesis")
}
##the following data's correlation coefficient are not significant


## Q4-6
income <- c(35165,35778,37027,37256,37512,37997,37343,
            36054,35593,35241,35486)
races <- c(399,469,429,450,474,598,364,430,433,410,317)
result4 <- cor.test(income,races)
result4
result4 <- cor.test(x,y,method = "pearson")
if(result4$p.value > 0.05){
  print("we can't reject non hypothesis")
} else{
  print("we reject non hypothesis")
}
## correlation coefficient is equal to zero
## there is no correlation between incom and number of races


## Q5-7
x <- c(1,2,5,6,11,12)
y <- c(8,4,12,3,10,7)
result5 <- cor.test(x,y,method = "spearman",conf.level = 0.9)
result5
if(result5$p.value > 0.05){
  print("we can't reject non hypothesis")
} else{
  print("we reject non hypothesis")
}
## correlation coefficient is equal to zero,not signifcant
## there are no correlation with these two data


## Q6-8
x <- c(3.2,2.4,1.6,8.3,7.2,5.1)
y <- c(6.2,7.3,8.1,2.6,6.3,4.3)
result6 <- cor.test(y,x)
result6
if(result6$p.value > 0.05){
  print("we can't reject non hypothesis")
} else{
  print("we reject non hypothesis")
}
## correlation coefficient is equal to zero,not signifcant
## there are no correlation with these two data


## Q7-9
### a
df <- read.csv("https://xujianzi.github.io/data/Bedroom.csv",header = T)
result7 <- cor.test(df$Bdrms,df$Lotsize,alternative = "greater", method = "spearman")
result7
c_r <- 2/sqrt(nrow(df))
c_r
if(result7$estimate > c_r && result7$p.value <= 0.05){
  print("rs is significant, we accept alternative hypothesis")
}else{
  print("we can not reject non hypothesis")
}
## the bedroom number and lot size have positive correlation.
## the bigger bedroom number is , the larger size the lot has.


### b
df1 <- head(df,n = 7)
result7b <- cor.test(df1$Bdrms,df1$Lotsize, method = "spearman")
result7b
c_r1 <- 2/sqrt(7)
c_r1
if(abs(result7b$estimate)>c_r1){
  print("rs is significant, we accept alternative hypothesis")
}else
  print("rs is not significant, we can not reject non hypothesis")
## the bedroom number and lot size don't have correlation.



## Q8-10
df <- read.csv("https://xujianzi.github.io/data/UK.csv",header = T)
result8 <- cor.test(df$bedrooms,df$floorarea,
                    alternative = "greater",method = "spearman")
result8
c_r <- 2/sqrt(nrow(df))
if(result8$estimate > c_r && result8$p.value <= 0.05){
  print("rs is significant, we accept alternative hypothesis")
}else {
  print("we can not reject non hypothesis")
}
## the number of bedrooms and floorarea have positive correlation.
## the bigger bedroom number is, the larger floorarea is.


## Q9
library(datasets)
df <- cars
### a
plot(df$speed,df$dist,main = "correlation of speed and distance")

### b
nrow(df)
## there are 50 rows in the data frame

### c
df$mspeed <- mean(df$speed)
df$mdist <- mean(df$dist)
df$difs <- df$speed-df$mspeed
df$difd <- df$dist - df$mdist
df$difsd <- df$difs*df$difd
top <- sum(df$difsd)
bot <- sd(df$dist)*sd(df$speed)*(nrow(df)-1)
r <- top/bot
r
## the correlation coefficient is 0.8068949

### d
result9d <- cor.test(df$speed, df$dist)
result9d
if(result9d$estimate == r){
  print("this two outputs are equal")
}

### e
n <- nrow(df)
df$ranksp <- rank(df$speed) 
df$rankdi <- rank(df$dist)
df$dif <- df$ranksp - df$rankdi
df$sq_dif <- df$dif^2
top <-sum(df$sq_dif)
rs <- 1- 6*top/(n^3-n)
rs
### f
result9f <- cor.test(df$speed,df$dist,method = "spearman")
result9f 
if(result9f$estimate == rs){
  print("they are equal")
}else
  print("they are not exactly equal")
## the manual result and the cor.test result are roughly equal.