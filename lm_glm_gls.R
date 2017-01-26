data(airquality)

plot(Ozone~Wind,airquality)

model1 = lm(Ozone~Wind,airquality)
plot(model1)

coef(model1)
#predict for Wind speeds of 19 and 20 mph
Ozone1 = coef(model1)[1]+coef(model1)[2]*19
Ozone2 = coef(model1)[1]+coef(model1)[2]*20

Ozone1
Ozone2

model2 = glm(Ozone~Wind,airquality,family=poisson)
coef(model2)

Ozone1.glm = exp(coef(model2)[1]+coef(model2)[2]*19)
Ozone2.glm = exp(coef(model2)[1]+coef(model2)[2]*20)
Ozone2.glm/Ozone1.glm

exp(coef(model2)[2])

library(nlme)
model3 = gls(Ozone~Wind,airquality,na.action=na.exclude)

head(airquality)

airquality$Date = as.Date(paste(1973,airquality$Month,airquality$Day,sep="-"))

library(lattice)

xyplot(Ozone~Date,airquality)

model4 = gls(Ozone~Wind*Date,airquality,na.action=na.exclude)

air2 = subset(airquality,complete.cases(Ozone))

model5 = gls(Ozone~Wind*Date,air2)
plot(ACF(model5,form=~Date),alpha=0.05)

model6=update(model5,correlation=corAR1())

library(MuMIn)

AICc(model5,model6)

summary(model6)


              