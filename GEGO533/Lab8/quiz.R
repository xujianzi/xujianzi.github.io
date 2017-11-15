x <- c(65,35,30,44,80,77,32,39,44,77)
y <- c(120,68,35,60,100,91,44,71,89,113)
df <- data.frame(x,y)
df$mx <- mean(df$x)
df$my <- mean(df$y)
df$dx <- df$x - df$mx
df$dy <- df$y - df$my
df$d <- df$dx*df$dy

top <- sum(df$d)
bot <- sum(df$dx^2)
b <- top/bot
m <- lm(df$y~df$x)
m
anova(m)
sst <- 5039.2+2189.7
summary(m)
qf(0.95,1,8)

ssr <- sum(m$residuals^2)
sum(df$d^2)
sd(m$residuals)
sum((m$fitted.values-df$my)^2)
