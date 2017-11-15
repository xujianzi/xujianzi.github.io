if(!require(leaflet)) {install.packages("leaflet")}
library(leaflet)
df <- read.csv(file = "https://xujianzi.github.io/data/hometown1.csv",header = T)
content <- paste(sep = "<br/>",
                 "<b><a href='https://xujianzi.github.io/data/WechatIMG75.jpeg'>Jian Xu</a></b>",
                 "Yufeng International ",
                 "Tianxian road"
)
m <- leaflet(data = df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~longitude,~latitude,popup = ~fullname,label = ~image) %>%
  addPopups(113.8660792,30.9191275,content,options = popupOptions(closeButton = F))
m  # Print the map




url <- "http://spatial.binghamton.edu/geog533/data/AlbumSales1.csv"
#url <- "AlbumSales1.csv"
df <- read.csv(url,header = TRUE)
library(knitr)
kable(head(df,n = 10))

plot(df)
m <- lm(df$sales ~ df$adverts)
m
m <- lm(sales ~ adverts, data = df)
result <- summary(m)
result$df
sd(m$residuals)


