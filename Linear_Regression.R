#data(airquality)
#names(airquality)
#plot(Ozone~Solar.R, data=airquality)
mydata<- read.csv("C:/Users/ybsipka/Desktop/R_dir/office_building_weekly_test.csv")
names(mydata)
plot(Y~time,data=bigdata)

#calculate mean, remove NAs
#mean.Ozone <- mean(airquality$Ozone, na.rm=TRUE)
mean.energy <- mean(mydata$e_cons)
mean.energy

#abline(h=mean.Ozone)
abline(h=mean.energy)

#use lm to fit a regression line
#lm(responseVar~ExplanatoryVar)
#model1 <-lm(Ozone~Solar.R,data=airquality)
model1 <-lm(e_cons~Week,data=mydata)
model1 <- lm(Y~time,data=bigdata)
model1

abline(lm11, col="red")

plot(model1)

termplot(model1)

summary(model1)
