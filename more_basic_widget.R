library(gWidgets)
library(gWidgetstcltk)
library(xlsx)

win <- gwindow("Lineer Regression Example") #inwin -> win
grp_workhours <- ggroup(container = win) #win

#Work Hour Variables
lbl_whs <- glabel(
  "Work Hour Start: ",
  container = grp_workhours
)

txt_whs <- gedit(
  "-",
  container = grp_workhours,
  coerce.with = as.numeric,
  handler  = function(h, ...)
  {
    sampleStartHour <<- svalue(h$obj)
  }
)

lbl_whe <- glabel(
  "Work Hour End: ",
  container = grp_workhours
)

txt_whe <- gedit(
  "-",
  container = grp_workhours,
  coerce.with = as.numeric,
  handler  = function(h, ...)
  {
    sampleEndHour <<- svalue(h$obj)
  }
)

lbl_wd <- glabel(
  "Work Days: ",
  container = grp_workhours
)

txt_wd <- gedit(
  "Mon,Tue,Wed,Thu,Fri",
  container = grp_workhours,
  handler  = function(h, ...)
  {
    workDaysString <<- svalue(h$obj)
  }
)
############## UPLOAD BUTTON ##################
btn_upload <- gbutton(
  text      = "Upload tab delimited file",
  container = ggroup(container = win), #inwin
  handler   = function(h, ...)
  {
    gfile(
      text    = "Upload .csv file",
      type    = "open",
      action  = "read.delim",
      handler = function(h, ...)
      {
        allData <<- read.csv(file = h$file, head = TRUE, sep = ",")
        allData <<- na.omit(allData)
      },
      filter = list(
        ".csv files" = list(patterns = c("*.csv")),
        "All files" = list(patterns = c("*"))
      )
    )
  }
)


############## CALCULATE DAILY ##################
btn_dc <- gbutton(
  text      = "Calculate Daily",
  container = ggroup(container = win),
  handler  = function(h, ...)
  {
    allConsumption <- allData$Consumption
    allTemperature <- allData$Temperature
    allSampleDate <- allData$SampleDate
    workDays <- strsplit(workDaysString, ",")
    
    dayCount <- length(allData$SampleDate) / 24;
    consumption <- c()
    dayOfWeek <- c()
    sampleDate <- c()
    temperature <- c()
    HDD <- c()
    CDD <- c()
    
    for(i in 1:dayCount)
    {
      startIndex <- ((i-1) * 24) + 1
      endIndex <- startIndex + 23
      
      #Günlük toplam harcama bulunur
      consumption <- append(consumption, sum(allConsumption[startIndex:endIndex]))
      sampleDate <- append(sampleDate, substr(allSampleDate[startIndex], 1, 14))
      
      #Günlük ortalama sıcaklık bulunur, HDD ve CDD hesaplanır
      dailyAvgTmp <- mean(allTemperature[startIndex:endIndex])
      temperature <- append(temperature, dailyAvgTmp)
      if((15 - dailyAvgTmp) < 0)
      {
        HDD <- append(HDD, 0)
      }
      else
      {
        HDD <- append(HDD, 15 - dailyAvgTmp)
      }
      if((dailyAvgTmp - 22) < 0)
      {
        CDD <- append(CDD, 0)
      }
      else
      {
        CDD <- append(CDD, dailyAvgTmp - 22)
      }
      
      #Haftaiçi veya haftasonu olmasına göre dayOfWeek değişkenleri ayarlanır
      if(substr(allSampleDate[startIndex], 12, 14) %in% workDays[[1]])
      {
        dayOfWeek <- append(dayOfWeek, 1)
      }
      else
      {
        dayOfWeek <- append(dayOfWeek, 0)
      }
    }
    
    HDDSquare <- HDD^2
    CDDSquare <- CDD^2
    HDDCube <- HDD^3
    CDDCube <- CDD^3
    
    resultSet <- lm(consumption ~ dayOfWeek + HDD + CDD + HDDSquare + CDDSquare + HDDCube + CDDCube)
    summary(resultSet)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    variableCount <- 7
    consumptionCalculated <- (dayOfWeek * resultSet$coefficients[2]) + (HDD * resultSet$coefficients[3]) + (CDD * resultSet$coefficients[4]) + (HDDSquare * resultSet$coefficients[5]) + (CDDSquare * resultSet$coefficients[6]) + (HDDCube * resultSet$coefficients[7]) + (CDDCube * resultSet$coefficients[8]) + resultSet$coefficients[1]
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dayCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dayCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, dayOfWeek, HDD, CDD, HDDSquare, CDDSquare, HDDCube, CDDCube, consumptionCalculated)
    dataFrame2 <- data.frame(RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    
    write.xlsx(dataFrame1, file="DailyOutput.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="DailyOutput.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="DailyOutput.xlsx", sheetName="sheet3", append=TRUE)
    
    
  }
)



############## CALCULATE WEEKLY ##################
btn_wc <- gbutton(
  text      = "Calculate Weekly",
  container = ggroup(container = win),
  handler  = function(h, ...)
  {
    allConsumption <- allData$Consumption
    allTemperature <- allData$Temperature
    allSampleDate <- allData$SampleDate
    
    dayCount <- floor(length(allData$SampleDate) / 24);
    dailyConsumption <- c()
    dailySampleDate <- c()
    dailyTemperature <- c()
    dailyHDD <- c()
    dailyCDD <- c()
    
    for(i in 1:dayCount)
    {
      startIndex <- ((i-1) * 24) + 1
      endIndex <- startIndex + 23
      
      #Günlük toplam harcama bulunur
      dailyConsumption <- append(dailyConsumption, sum(allConsumption[startIndex:endIndex]))
      dailySampleDate <- append(dailySampleDate, substr(allSampleDate[startIndex], 1, 14))
      
      #Günlük ortalama sıcaklık bulunur, HDD ve CDD hesaplanır
      dailyAvgTmp <- mean(allTemperature[startIndex:endIndex])
      dailyTemperature <- append(dailyTemperature, dailyAvgTmp)
      if((15 - dailyAvgTmp) < 0)
      {
        dailyHDD <- append(dailyHDD, 0)
      }
      else
      {
        dailyHDD <- append(dailyHDD, 15 - dailyAvgTmp)
      }
      if((dailyAvgTmp - 22) < 0)
      {
        dailyCDD <- append(dailyCDD, 0)
      }
      else
      {
        dailyCDD <- append(dailyCDD, dailyAvgTmp - 22)
      }
    }
    
    weekCount <- floor(dayCount / 7);
    consumption <- c()
    temperature <- c()
    sampleDate <- c()
    HDD <- c()
    CDD <- c()
    
    for(i in 1:weekCount)
    {
      startIndex <- ((i-1) * 7) + 1
      endIndex <- startIndex + 6
      
      #Haftalık toplam harcama bulunur. (Günlük değerler toplanarak)
      consumption <- append(consumption, sum(dailyConsumption[startIndex:endIndex]))
      temperature <- append(temperature, sum(dailyTemperature[startIndex:endIndex]))
      sampleDate <- append(sampleDate, paste(dailySampleDate[startIndex], dailySampleDate[endIndex], sep=" - "))
      
      #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
      HDD <- append(HDD, sum(dailyHDD[startIndex:endIndex]))
      CDD <- append(CDD, sum(dailyCDD[startIndex:endIndex]))
    }
    
    HDDSquare <- HDD^2
    CDDSquare <- CDD^2
    HDDCube <- HDD^3
    CDDCube <- CDD^3
    
    resultSet <- lm(consumption ~ HDD + CDD + HDDSquare + CDDSquare + HDDCube + CDDCube)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    variableCount <- 6
    consumptionCalculated <- (HDD * resultSet$coefficients[2]) + (CDD * resultSet$coefficients[3]) + (HDDSquare * resultSet$coefficients[4]) + (CDDSquare * resultSet$coefficients[5]) + (HDDCube * resultSet$coefficients[6]) + (CDDCube * resultSet$coefficients[7]) + resultSet$coefficients[1]
    RMSE <- sum((consumption - consumptionCalculated)^2) / (weekCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((weekCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, HDD, CDD, HDDSquare, CDDSquare, HDDCube, CDDCube, consumptionCalculated)
    dataFrame2 <- data.frame(RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="WeeklyOutput.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="WeeklyOutput.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="WeeklyOutput.xlsx", sheetName="sheet3", append=TRUE)
    
    
  }
)



############## CALCULATE DAILY ##################
btn_dc <- gbutton(
  text      = "Calculate Daily",
  container = ggroup(container = win),
  handler  = function(h, ...)
  {
    allConsumption <- allData$Consumption
    allTemperature <- allData$Temperature
    allSampleDate <- allData$SampleDate
    workDays <- strsplit(workDaysString, ",")
    
    dayCount <- length(allData$SampleDate) / 24;
    consumption <- c()
    dayOfWeek <- c()
    sampleDate <- c()
    temperature <- c()
    HDD <- c()
    CDD <- c()
    
    for(i in 1:dayCount)
    {
      startIndex <- ((i-1) * 24) + 1
      endIndex <- startIndex + 23
      
      #Günlük toplam harcama bulunur
      consumption <- append(consumption, sum(allConsumption[startIndex:endIndex]))
      sampleDate <- append(sampleDate, substr(allSampleDate[startIndex], 1, 14))
      
      #Günlük ortalama sıcaklık bulunur, HDD ve CDD hesaplanır
      dailyAvgTmp <- mean(allTemperature[startIndex:endIndex])
      temperature <- append(temperature, dailyAvgTmp)
      if((15 - dailyAvgTmp) < 0)
      {
        HDD <- append(HDD, 0)
      }
      else
      {
        HDD <- append(HDD, 15 - dailyAvgTmp)
      }
      if((dailyAvgTmp - 22) < 0)
      {
        CDD <- append(CDD, 0)
      }
      else
      {
        CDD <- append(CDD, dailyAvgTmp - 22)
      }
      
      #Haftaiçi veya haftasonu olmasına göre dayOfWeek değişkenleri ayarlanır
      if(substr(allSampleDate[startIndex], 12, 14) %in% workDays[[1]])
      {
        dayOfWeek <- append(dayOfWeek, 1)
      }
      else
      {
        dayOfWeek <- append(dayOfWeek, 0)
      }
    }
    
    HDDSquare <- HDD^2
    CDDSquare <- CDD^2
    HDDCube <- HDD^3
    CDDCube <- CDD^3
    
    resultSet <- lm(consumption ~ dayOfWeek + HDD + CDD + HDDSquare + CDDSquare + HDDCube + CDDCube)
    summary(resultSet)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    variableCount <- 7
    consumptionCalculated <- (dayOfWeek * resultSet$coefficients[2]) + (HDD * resultSet$coefficients[3]) + (CDD * resultSet$coefficients[4]) + (HDDSquare * resultSet$coefficients[5]) + (CDDSquare * resultSet$coefficients[6]) + (HDDCube * resultSet$coefficients[7]) + (CDDCube * resultSet$coefficients[8]) + resultSet$coefficients[1]
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dayCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dayCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, dayOfWeek, HDD, CDD, HDDSquare, CDDSquare, HDDCube, CDDCube, consumptionCalculated)
    dataFrame2 <- data.frame(RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    
    write.xlsx(dataFrame1, file="DailyOutput.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="DailyOutput.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="DailyOutput.xlsx", sheetName="sheet3", append=TRUE)
    
    
  }
)



############## CALCULATE WEEKLY ##################
btn_wc <- gbutton(
  text      = "Calculate Weekly",
  container = ggroup(container = win),
  handler  = function(h, ...)
  {
    allConsumption <- allData$Consumption
    allTemperature <- allData$Temperature
    allSampleDate <- allData$SampleDate
    
    dayCount <- floor(length(allData$SampleDate) / 24);
    dailyConsumption <- c()
    dailySampleDate <- c()
    dailyTemperature <- c()
    dailyHDD <- c()
    dailyCDD <- c()
    
    for(i in 1:dayCount)
    {
      startIndex <- ((i-1) * 24) + 1
      endIndex <- startIndex + 23
      
      #Günlük toplam harcama bulunur
      dailyConsumption <- append(dailyConsumption, sum(allConsumption[startIndex:endIndex]))
      dailySampleDate <- append(dailySampleDate, substr(allSampleDate[startIndex], 1, 14))
      
      #Günlük ortalama sıcaklık bulunur, HDD ve CDD hesaplanır
      dailyAvgTmp <- mean(allTemperature[startIndex:endIndex])
      dailyTemperature <- append(dailyTemperature, dailyAvgTmp)
      if((15 - dailyAvgTmp) < 0)
      {
        dailyHDD <- append(dailyHDD, 0)
      }
      else
      {
        dailyHDD <- append(dailyHDD, 15 - dailyAvgTmp)
      }
      if((dailyAvgTmp - 22) < 0)
      {
        dailyCDD <- append(dailyCDD, 0)
      }
      else
      {
        dailyCDD <- append(dailyCDD, dailyAvgTmp - 22)
      }
    }
    
    weekCount <- floor(dayCount / 7);
    consumption <- c()
    temperature <- c()
    sampleDate <- c()
    HDD <- c()
    CDD <- c()
    
    for(i in 1:weekCount)
    {
      startIndex <- ((i-1) * 7) + 1
      endIndex <- startIndex + 6
      
      #Haftalık toplam harcama bulunur. (Günlük değerler toplanarak)
      consumption <- append(consumption, sum(dailyConsumption[startIndex:endIndex]))
      temperature <- append(temperature, sum(dailyTemperature[startIndex:endIndex]))
      sampleDate <- append(sampleDate, paste(dailySampleDate[startIndex], dailySampleDate[endIndex], sep=" - "))
      
      #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
      HDD <- append(HDD, sum(dailyHDD[startIndex:endIndex]))
      CDD <- append(CDD, sum(dailyCDD[startIndex:endIndex]))
    }
    
    HDDSquare <- HDD^2
    CDDSquare <- CDD^2
    HDDCube <- HDD^3
    CDDCube <- CDD^3
    
    resultSet <- lm(consumption ~ HDD + CDD + HDDSquare + CDDSquare + HDDCube + CDDCube)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    variableCount <- 6
    consumptionCalculated <- (HDD * resultSet$coefficients[2]) + (CDD * resultSet$coefficients[3]) + (HDDSquare * resultSet$coefficients[4]) + (CDDSquare * resultSet$coefficients[5]) + (HDDCube * resultSet$coefficients[6]) + (CDDCube * resultSet$coefficients[7]) + resultSet$coefficients[1]
    RMSE <- sum((consumption - consumptionCalculated)^2) / (weekCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((weekCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, HDD, CDD, HDDSquare, CDDSquare, HDDCube, CDDCube, consumptionCalculated)
    dataFrame2 <- data.frame(RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="WeeklyOutput.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="WeeklyOutput.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="WeeklyOutput.xlsx", sheetName="sheet3", append=TRUE)
    
    
  }
)


