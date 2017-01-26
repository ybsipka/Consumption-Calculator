#data(airquality)
mydata<- read.csv("C:/Users/ybsipka/Desktop/R_dir/office_building_weekly_test.csv")
names(mydata)
#produce plots
#plot(Ozone~Solar.R,airquality)
#plot(Ozone~Wind,airquality)
plot(e_cons~Week,mydata) #responseVar~Explanatory # dependent~independent
plot(e_cons~temperature,mydata)

coplot(e_cons~Week|temperature,panel = panel.smooth,mydata)

#model2 <- lm(Ozone~Solar.R*Wind,airquality)
model2 <- lm(e_cons~Week*temperature,mydata)
plot(model2)

summary(model2)

termplot(model2) #doesnt let us...

summary(airquality$Solar.R)
summary(mydata$temperature)

summary(airquality$Wind)

#Solar1 = mean(airquality$Solar.R,na.rm=TRUE)
#Solar2 = 100
#Solar3 = 300
temp1 = mean(mydata$temperature)
temp2 = 10
temp3 = 21

#predict(model2,data.frame(Solar.R=100,Wind=10))

#predictions for average solar Radiation
#p1 = predict(model2,data.frame(Solar.R=Solar1,Wind=1:20))
p1 = predict(model2, data.frame(temperature = temp1, Week = 1:40))

#p2 = predict(model2,data.frame(Solar.R=Solar2,Wind=1:20))
p2 = predict(model2, data.frame(temperature = temp2, Week = 1:40))

#p3 = predict(model2,data.frame(Solar.R=Solar3,Wind=1:20))
p3 = predict(model2, data.frame(temperature = temp3, Week = 1:40))

#plot(Ozone~Wind,airquality)
plot(e_cons~Week)
#lines(1:20,p1)
lines(1:40,p1)
#lines(1:20,p2)
lines(1:40,p2)
#lines(1:20,p3)
lines(1:40,p3)


