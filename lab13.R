install.packages("neuralnet")
install.packages("ggplot2")
library(neuralnet)
library(ggplot2)

iris <-read.csv("iris.csv", header=T, sep=",")
#Setosa
for(i in 1:50){ 
  iris[i,5]<-1
}
#Versicolor
for(i in 51:100){
  
  iris[i,5]<-2
}
#Virginica
for(i in 101:150){
  iris[i,5]<-3
}

a <- sapply(iris[ , -5], min)
b <- sapply(iris[ , -5], max) - a


iris.x <- scale(iris[, 1:4], center=a, scale=b)

y1 <- rep(0, nrow(iris))
y1[iris[ , 5]==1] <-1

y2 <- rep(0, nrow(iris))
y2[iris[ , 5]==2] <-1

y3 <- rep(0, nrow(iris))
y3[iris[ , 5]==3] <-1

z.1 <- as.data.frame(cbind (iris.x, y1, y2, y3))
z.1

set.seed(1234567)
index <- sample(1:nrow(z.1), round(nrow(z.1)*2/3), replace=F)   
z.train <- z.1[index,]
z.test <- z.1[-index,]


n <- names(z.1)
n

num.nets <- 10

seed.start <- 12345

error.best <- 1

error.vector <- rep(-9999, num.nets)
seed.current <- seed.start

for (i in 1: num.nets){ 
  
  seed.current <- seed.current + 1
  set.seed(seed.current)
  nn.temp <- neuralnet( y1+y2+y3 ~sepal.length + sepal.width + petal.length + petal.width,
                        data=z.train, hidden = c(3,2), linear.output=F)
  
  res.z <- compute(nn.temp, z.train[, 1:4] )  
  res.z2 <- apply(res.z$net.result, 1, which.max )
  error.temp <- sum(res.z2 != iris[index,1] )/length(index) 
  error.vector[i] <- error.temp
  if (error.temp <= error.best)
  {
    nn.best <- nn.temp
    error.best <- error.temp
    seed.best <- seed.current
  }
}

plot(nn.best)

error.vector

error.best

seed.best



res.3 <- compute(nn.best, z.train[, 1:4] )    
res.z3 <- apply(res.3$net.result, 1, which.max)
table(res.z3, iris[index,1])

res.4 <- compute(nn.best, z.test[, 1:4] )     
res.z4 <- apply(res.4$net.result, 1, which.max )
table(res.z4, iris[-index,1])

sum(diag(table(iris[-index,1], res.z4)))/length(iris[-index,1])*100

100 - (sum(diag(table(iris[-index,1], res.z4)))/length(iris[-index,1])*100)
