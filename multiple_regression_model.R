#multiple regression in R

#model1 <- lm(V2 ~ V4 + V5 + V6 + V7, data = daily_consumption)
model1 <- lm(V3 ~ V2 , data = daily_consumption)
summary(model1)

#calculate Pearson's correlation between HDD and CDD
cor(daily_consumption$V5, daily_consumption$V6, method="pearson")

#confidence interval
confint(model1, conf.level = 0.95)

#all X variables
model2 <- lm(V2 ~ V5 + V6 + V7 + V8 + V9 + V10, data = daily_consumption)

# summary
summary(model2)

# check the regression diagnostic plots for this model
plot(model2)
