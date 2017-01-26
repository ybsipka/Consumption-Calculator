#predict with SVM
model <- svm(teb168T ~ teb168S , teb168) #create the model
predictedY <- predict(model, teb168) #predict

#create the table to do a accurate prediction for te specified outside temperature
t_pred <- table(teb168_336S,predictedY) #table(OutsideTemp, Prediction)

#pull data from the table and calculate a sum of the consumptions
len <- 0
agg <- 0
#convert table labels to numeric
lab <- as.numeric(labels(t_pred)$predictedY)
#calculate the total consumption from the predictions due to the outside temperature
for(i in 1:ncol(t_pred)) {
    agg <- agg + sum(t_pred[,i])*lab[i] #multiply the sum of each column by the label of the column(multiply with the predicted consumption)
    print(agg)
    }
#if couldn't match all the temperature values to the consumptions, then add the mean of predictions for how many skipped
if(length(teb168_336S)>nrow(t_pred)){
  len <- length(teb168_336S)-nrow(t_pred)
  agg <- agg + len*mean(pred)
}
#Inform the user
print(cat("Consumption Prediction For the Next", length(teb168_336S)/24 , "Days =" , agg))
real_sum <- sum(teb168T)
pred_cons <- agg+real_sum
print(cat("Total Consumption After", length(teb168S)/24 , "Days =" , pred_cons))
real_cons <- real_sum + sum(pal168_336T)
error <- abs(real_cons-pred_cons)
print(cat("Real Consumption =", real_cons , "Predicted Consumptions" , pred_cons, " Error =", error, "\n"))

