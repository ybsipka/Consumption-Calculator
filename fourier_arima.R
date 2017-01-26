aic_vals_temp <- NULL
aic_vals <- NULL
for(i in 1:5)
{
  xreg1 <- fourier(1:length(teb_data$Consumption),i,24)
  fitma1 <- auto.arima(teb_data$Consumption, D = 0, max.P = 0, max.Q = 0, xreg = xreg1)
  aic_vals_temp <- cbind(i, fitma1$aic)
  aic_vals <- rbind(aic_vals,aic_vals_temp)
}

colnames(aic_vals) <- c("FourierTerms24", "AicValue")
aic_vals <- data.frame(aic_vals)
minAICVal <- min(aic_vals$AICValue)
minvals <- aic_vals[which(aic_vals$AICValue == minAICVal),]
