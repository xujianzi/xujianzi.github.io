#use library
library(MASS)
my_cars <- Cars93###直接从package种读取
View(my_cars)
str(my_cars)
table(my_cars$Type)                       #筛选出Type中的种类
t <- table(my_cars$Type)
t
cars_type = row.names(t)
cars_type[1]



#####1
###################solution 1
####filter
# final code
for(i in 1:6){
  print(cars_type[i])
  minp <- min(my_cars[my_cars$Type == cars_type[i],]$Price)
  print(my_cars[my_cars$Type == cars_type[i] & my_cars$Price == minp,]$Manufacturer)
  print(my_cars[my_cars$Type == cars_type[i] & my_cars$Price == minp,]$Model)
}
for(i in 1:6){
  print(cars_type[i])
  greMPG <- max(my_cars[my_cars$Type == cars_type[i],]$MPG.highway)
  print(my_cars[my_cars$Type == cars_type[i] & my_cars$MPG.highway == greMPG,]$Manufacturer)
  print(my_cars[my_cars$Type == cars_type[i] & my_cars$MPG.highway == greMPG,]$Model)
}






####2
#compute the mean,skewness,kurtosis of"Compact""Large""Midsize""Small""Sporty""Van"
library(moments)
################### solution 1
tapply(my_cars$Horsepower, my_cars$Type, skewness)
tapply(my_cars$Horsepower, my_cars$Type, kurtosis)

################## solution 2
aggregate(my_cars["Horsepower"], my_cars["Type"], skewness)
aggregate(my_cars["Horsepower"], my_cars["Type"], kurtosis)

################## solution 3
mean(my_cars[my_cars$Type == "Compact",]$Horsepower)
mean(my_cars[my_cars$Type == "Large",]$Horsepower)
mean(my_cars[my_cars$Type == "Midsize",]$Horsepower)
mean(my_cars[my_cars$Type == "Small",]$Horsepower)
mean(my_cars[my_cars$Type == "Sporty",]$Horsepower)
mean(my_cars[my_cars$Type == "Van",]$Horsepower)
skewness(my_cars[my_cars$Type == "Compact",]$Horsepower)
skewness(my_cars[my_cars$Type == "Large",]$Horsepower)
skewness(my_cars[my_cars$Type == "Midsize",]$Horsepower)
skewness(my_cars[my_cars$Type == "Small",]$Horsepower)
skewness(my_cars[my_cars$Type == "Sporty",]$Horsepower)
skewness(my_cars[my_cars$Type == "Van",]$Horsepower)
kurtosis(my_cars[my_cars$Type == "Compact",]$Horsepower)
kurtosis(my_cars[my_cars$Type == "Large",]$Horsepower)
kurtosis(my_cars[my_cars$Type == "Midsize",]$Horsepower)
kurtosis(my_cars[my_cars$Type == "Small",]$Horsepower)
kurtosis(my_cars[my_cars$Type == "Sporty",]$Horsepower)
kurtosis(my_cars[my_cars$Type == "Van",]$Horsepower)




#####3
#USA origin
str(my_cars)
USA_cars <- my_cars[my_cars$Origin == "USA",]
#no_USA origin
no_USA_cars <- my_cars[my_cars$Origin == "non-USA",]
no_USA_cars



#####4
#write and read USA car data
write.csv(USA_cars, file = "USA_cars.csv", row.names = FALSE)
check_USA_CARS <- read.csv("USA_cars.csv", header = TRUE)




####################solution 2 of question1
#################### cheapest price
#Compact
my_price <- my_cars[my_cars$Type == "Compact",]
#如何在其中加入两个筛选条件？solution:(R 中且是一个&，或是一个|)
#minP_cars <- min(my_cars[my_cars$Type == "Compact",]$Price)
#my_cars[my_cars$Type == "Compact" & my_cars$Price == 11.1,]$Manufacturer
#my_cars[my_cars$Type == "Compact" & my_cars$Price == 11.1,]$Model
sort(my_price$Price)
min_my_price <- my_price[my_price$Price == 11.1,]
min_my_price$Manufacturer
min_my_price$Model                   ##可以同时输出两个数据吗，Manu和Model？


#large
my_priceL <- my_cars[my_cars$Type == "Large",]
sort(my_priceL$Price)
min_my_priceL <- my_priceL[my_priceL$Price == 18.4,]
min_my_priceL$Manufacturer
min_my_priceL$Model  

#Mindsize
my_priceM <- my_cars[my_cars$Type == "Midsize",]
sort(my_priceM$Price)
min_my_priceM <- my_priceM[my_priceM$Price == 13.9,]
min_my_priceM$Manufacturer
min_my_priceM$Model 

#small
my_priceS <- my_cars[my_cars$Type == "Small",]
sort(my_priceS$Price)
min_my_priceS <- my_priceS[my_priceS$Price == 7.4,]
min_my_priceS$Manufacturer
min_my_priceS$Model

#Sporty
my_priceSp <- my_cars[my_cars$Type == "Sporty",]
sort(my_priceSp$Price)
min_my_priceSp <- my_priceSp[my_priceSp$Price == 10.0,]
min_my_priceSp$Manufacturer
min_my_priceSp$Model

#Van
my_priceV <- my_cars[my_cars$Type == "Van",]
sort(my_priceV$Price)
min_my_priceV <- my_priceSp[my_priceV$Price == 16.3,]
min_my_priceV$Manufacturer
min_my_priceV$Model






###########grestest fule use

#Compact
my_hwu <- my_cars[my_cars$Type == "Compact",]
rev(sort(my_hwu$MPG.highway))
max_my_hwu <- my_hwu[my_hwu$MPG.highway == 36,]
max_my_hwu$Manufacturer
max_my_hwu$Model                  

#large
my_hwuL <- my_cars[my_cars$Type == "Large",]
rev(sort(my_hwuL$MPG.highway))
max_my_hwuL <- my_hwuL[my_hwuL$MPG.highway == 28,]
max_my_hwuL$Manufacturer
max_my_hwuL$Model  

#Midsize
my_hwuM <- my_cars[my_cars$Type == "Midsize",]
rev(sort(my_hwuM$MPG.highway))
max_my_hwuM <- my_hwuM[my_hwuM$MPG.highway == 31,]
max_my_hwuM$Manufacturer
max_my_hwuM$Model 

#small
my_hwuS <- my_cars[my_cars$Type == "Small",]
rev(sort(my_hwuS$MPG.highway))
max_my_hwuS <- my_hwuS[my_hwuS$MPG.highway == 50,]
max_my_hwuS$Manufacturer
max_my_hwuS$Model 

#Sporty
my_hwuSp <- my_cars[my_cars$Type == "Sporty",]
rev(sort(my_hwuSp$MPG.highway))
max_my_hwuSp <- my_hwuSp[my_hwuS$MPG.highway == 36,]
max_my_hwuSp$Manufacturer
max_my_hwuSp$Model 

#Van
my_hwuV <- my_cars[my_cars$Type == "Van",]
rev(sort(my_hwuV$MPG.highway))
max_my_hwuV <- my_hwuV[my_hwuV$MPG.highway == 24,]
max_my_hwuV$Manufacturer
max_my_hwuV$Model 
########################### solution 3 of quesiton 1
tapply(my_cars$Manufacturer, my_cars$Type, FUN = NULL)
