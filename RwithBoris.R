rm(list=ls())
library(ggplot2)
library(doBy)

data(iris)

dt<- iris
write.csv(dt, file="irisdata.csv")

read.csv(file= "irisdata.csv", sep = ",", nrows=5)

#the semi-colon always me to put in to functions and run them at the same time.

head(dt);tail(dt)
str(dt)
summary(dt)

unique(dt$Species)
table(dt$Species)
nrow(dt)

dt$LLOD <-ifelse(dt$Sepal.Length < 5, "y", "n")

str(dt)
table(dt$LLOD)


#Now we shall create some data




location <- c("A", "B", "C")

date <- c("2015-11-01", "2016-01-12")

rain <- c("lots", "some", "little", "no", "NA")





dt$loc <- rep(location, 150/3)
dt$date <- rep(sample(date), 150/2)
dt$rain <- rep(rain, nrow(dt)/ length(rain))
dt$rh <- runif(150, min = 37, max = 75)


str(dt)

aggregate(Sepal.Length ~ loc + date, data =dt,FUN = "mean")


tblsdt <- summaryBy(Sepal.Length ~ loc + date+ rain, data = dt, FUN = c(mean,sd))

head(tblsdt)

p <- ggplot(dt)
p <- p + geom_point(aes(x = Petal.Width, y= Sepal.Length, color=loc))
p

