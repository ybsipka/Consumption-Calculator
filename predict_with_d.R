u=read.table("/mydata/globtemp2.dat")  # read the data
gtemp=ts(u[,2],start=1880,freq=1)      # yearly temp in col 2
fit1=arima(gtemp, order=c(1,1,1))   
fore1=predict(fit1, 15)   
nobs=length(gtemp)                
fit2=arima(gtemp, order=c(1,1,1), xreg=1:nobs)  
fore2=predict(fit2, 15, newxreg=(nobs+1):(nobs+15))
par(mfrow=c(2,1))
ts.plot(gtemp,fore1$pred,col=1:2,main="WRONG")
ts.plot(gtemp,fore2$pred,col=1:2,main="RIGHT")