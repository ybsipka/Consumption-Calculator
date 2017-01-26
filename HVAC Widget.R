library(gWidgets)
library(gWidgetstcltk)
library(xlsx)

win <- gwindow("HVAC Example")
grp_workhours <- ggroup(container = win)
grp_upload <- ggroup(container = win)

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

btn_upload <- gbutton(
  text      = "Upload tab delimited file",
  container = grp_upload,
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

btn_ch <- gbutton(
  text      = "Calculate Hourly",
  container = grp_upload,
  handler  = function(h, ...)
  {
    allConsumption <- allData$Consumption
    allTemperature <- allData$Temperature
    allSampleDate <- allData$SampleDate
    workDays <- strsplit(workDaysString, ",")
    
    consumption <- c()
    timeOfWeek <- c()
    temperature <- c()
    temperatureDifference <- c()
    sampleDate <- c()
    setPoint <- c()
    
    dataCount <- 0
    #Bir gün ilerlediðimizde timeOfWeek deðiþkeninin ne kadar artacaðý
    timeOfWeekFactor <- (sampleEndHour - sampleStartHour + 1)
    
    for(i in 1:length(allSampleDate))
    {
      day <- substr(allSampleDate[i], 12, 14)
      hour <- as.numeric(substr(allSampleDate[i], 16, 17))
      
      #Haftaiçi ve çalýþma saatlerindeki datalar üzerinde çalýþacaðýz.
      if(day %in% workDays[[1]] && hour >= sampleStartHour && hour <= sampleEndHour)
      { 
        dataCount <- dataCount + 1
        sampleDate <- append(sampleDate, toString(allSampleDate[i]))
        temperature <- append(temperature, allTemperature[i])
        consumption <- append(consumption, allConsumption[i])
        if(allTemperature[i] < 21)
        {
          temperatureDifference <- append(temperatureDifference, 21 - allTemperature[i])
          setPoint <- append(setPoint, 21)
        }
        else if(allTemperature[i] > 24)
        {
          temperatureDifference <- append(temperatureDifference, 24 - allTemperature[i])
          setPoint <- append(setPoint, 24)
        }
        else
        {
          temperatureDifference <- append(temperatureDifference, 22 - allTemperature[i])
          setPoint <- append(setPoint, 22)
        }
        if(day == "Mon")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 0))
        }
        else if(day == "Tue")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 1))
        }
        else if(day == "Wed")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 2))
        }
        else if(day == "Thu")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 3))
        }
        else if(day == "Fri")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 4))
        }
        else if(day == "Sat")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 5))
        }
        else if(day == "Sun")
        {
          timeOfWeek <- append(timeOfWeek, hour - sampleStartHour + 1 + (timeOfWeekFactor * 6))
        }
      }
    }
    
    resultSet <- lm(consumption ~ timeOfWeek + temperatureDifference)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    
    variableCount <- 2
    consumptionCalculated <- (timeOfWeek * resultSet$coefficients[2]) + (temperatureDifference * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointPlus1 <- (timeOfWeek * resultSet$coefficients[2]) + ((temperatureDifference + 1) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointPlus2 <- (timeOfWeek * resultSet$coefficients[2]) + ((temperatureDifference + 2) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointMinus1 <- (timeOfWeek * resultSet$coefficients[2]) + ((temperatureDifference - 1) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointMinus2 <- (timeOfWeek * resultSet$coefficients[2]) + ((temperatureDifference - 2) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    RSquare <- 1 - (sum((consumption - consumptionCalculated)^2) / sum((consumption - mean(consumption))^2))
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dataCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dataCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, setPoint, consumption, temperature, timeOfWeek, temperatureDifference, consumptionCalculated, consumptionSetPointPlus1, consumptionSetPointPlus2, consumptionSetPointMinus1, consumptionSetPointMinus2)
    dataFrame2 <- data.frame(RSquare, RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="HourlyOutputHVAC.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="HourlyOutputHVAC.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="HourlyOutputHVAC.xlsx", sheetName="sheet3", append=TRUE)
  }
)

btn_cd <- gbutton(
  text      = "Calculate Daily",
  container = grp_upload,
  handler  = function(h, ...)
  {
    allConsumption <- allData$Consumption
    allTemperature <- allData$Temperature
    allSampleDate <- allData$SampleDate
    workDays <- strsplit(workDaysString, ",")
    
    consumption <- c()
    dayOfWeek <- c()
    sampleDate <- c()
    temperature <- c()
    temperatureDifference <- c()
    setPoint <- c()
    
    dayCount <- 0
    endIndex <- 0
    endWHIndex <- 0
    startIndex <- 0
    while(endIndex < length(allSampleDate))
    {
      #Yeni gÃ¼nÃ¼n deÄŸerlerini okumak iÃ§in
      startIndex <- endIndex + 1
      endIndex <- endIndex + 1
      endWHIndex <- endIndex + 1
      currentDay <- substr(allSampleDate[startIndex], 12, 14)
      while(as.numeric(substr(allSampleDate[startIndex], 16, 17)) < sampleStartHour)
      {
        startIndex <- startIndex + 1
        endIndex <- endIndex + 1
        endWHIndex <- endWHIndex + 1
      }
      while(endIndex < length(allSampleDate))
      {
        if(currentDay == substr(allSampleDate[endIndex+1], 12, 14))
        {
          if(as.numeric(substr(allSampleDate[endIndex+1], 16, 17)) <= sampleEndHour)
          {
            endWHIndex <- endWHIndex + 1
          }
          endIndex <- endIndex + 1
        }
        else
        {
          break
        }
      }
      if(currentDay %in% workDays[[1]])
      {
        dayCount <- dayCount + 1
        #GÃ¼nlÃ¼k toplam harcama bulunur
        consumption <- append(consumption, sum(allConsumption[startIndex:endWHIndex]))
        sampleDate <- append(sampleDate, substr(allSampleDate[startIndex], 1, 14))
        
        #GÃ¼nlÃ¼k ortalama sÄ±caklÄ±k bulunur, cooling ve heating set pointler ile gÃ¼nlÃ¼k sÄ±caklÄ±k arasÄ±ndaki fark hesaplanÄ±r
        dailyAvgTmp <- mean(allTemperature[startIndex:endWHIndex])
        temperature <- append(temperature, dailyAvgTmp)
        if(dailyAvgTmp < 21)
        {
          temperatureDifference <- append(temperatureDifference, 21 - dailyAvgTmp)
          setPoint <- append(setPoint, 21)
        }
        else if(dailyAvgTmp > 24)
        {
          temperatureDifference <- append(temperatureDifference, 24 - dailyAvgTmp)
          setPoint <- append(setPoint, 24)
        }
        else
        {
          temperatureDifference <- append(temperatureDifference, 22 - dailyAvgTmp)
          setPoint <- append(setPoint, 22)
        }
        if(currentDay == "Mon")
        {
          dayOfWeek <- append(dayOfWeek, 1)
        }
        else if(currentDay == "Tue")
        {
          dayOfWeek <- append(dayOfWeek, 2)
        }
        else if(currentDay == "Wed")
        {
          dayOfWeek <- append(dayOfWeek, 3)
        }
        else if(currentDay == "Thu")
        {
          dayOfWeek <- append(dayOfWeek, 4)
        }
        else if(currentDay == "Fri")
        {
          dayOfWeek <- append(dayOfWeek, 5)
        }
        else if(currentDay == "Sat")
        {
          dayOfWeek <- append(dayOfWeek, 6)
        }
        else if(currentDay == "Sun")
        {
          dayOfWeek <- append(dayOfWeek, 7)
        }
      }
    }
    
    resultSet <- lm(consumption ~ dayOfWeek + temperatureDifference)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    variableCount <- 2
    consumptionCalculated <- (dayOfWeek * resultSet$coefficients[2]) + (temperatureDifference * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointPlus1 <- (dayOfWeek * resultSet$coefficients[2]) + ((temperatureDifference + 1) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointPlus2 <- (dayOfWeek * resultSet$coefficients[2]) + ((temperatureDifference + 2) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointMinus1 <- (dayOfWeek * resultSet$coefficients[2]) + ((temperatureDifference - 1) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    consumptionSetPointMinus2 <- (dayOfWeek * resultSet$coefficients[2]) + ((temperatureDifference - 2) * resultSet$coefficients[3]) + resultSet$coefficients[1]
    RSquare <- 1 - (sum((consumption - consumptionCalculated)^2) / sum((consumption - mean(consumption))^2))
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dayCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dayCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, setPoint, consumption, temperature, dayOfWeek, temperatureDifference, consumptionCalculated, consumptionSetPointPlus1, consumptionSetPointPlus2, consumptionSetPointMinus1, consumptionSetPointMinus2)
    dataFrame2 <- data.frame(RSquare, RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="DailyOutputHVAC.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="DailyOutputHVAC.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="DailyOutputHVAC.xlsx", sheetName="sheet3", append=TRUE)
  }
)