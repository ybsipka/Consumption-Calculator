library(gWidgets)
library(gWidgetstcltk)
library(xlsx)

win <- gwindow("Weather Normalization Example")
grp_changePoint <- ggroup(container = win)
grp_workhours <- ggroup(container = win)
grp_upload <- ggroup(container = win)

#Change Point Variables
lbl_hsp <- glabel(
  "Heating Change Point: ",
  container = grp_changePoint
)

txt_hsp <- gedit(
  "-",
  container = grp_changePoint,
  coerce.with = as.numeric,
  handler  = function(h, ...)
  {
    heatingChangePoint <<- svalue(h$obj)
  }
)

lbl_csp <- glabel(
  "Cooling Change Point: ",
  container = grp_changePoint
)

txt_csp <- gedit(
  "-",
  container = grp_changePoint,
  coerce.with = as.numeric,
  handler  = function(h, ...)
  {
    coolingChangePoint <<- svalue(h$obj)
  }
)



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
    sampleDate <- c()
    CCP <- c()
    HCP <- c()
    
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
        
        coolingDiff <- allTemperature[i] - coolingChangePoint
        if(coolingDiff < 0)
        {
          CCP <- append(CCP, 0)
        }
        else
        {
          CCP <- append(CCP, coolingDiff)
        }
        
        heatingDiff <- heatingChangePoint - allTemperature[i]
        if(heatingDiff < 0)
        {
          HCP <- append(HCP, 0)
        }
        else
        {
          HCP <- append(HCP, heatingDiff)
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
    
    HCPSquare <- HCP^2
    CCPSquare <- CCP^2
    HCPCube <- HCP^3
    CCPCube <- CCP^3
    
    resultSet <- lm(consumption ~ timeOfWeek + HCP + CCP + HCPSquare + CCPSquare + HCPCube + CCPCube)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    
    variableCount <- 7
    consumptionCalculated <- (timeOfWeek * resultSet$coefficients[2]) + (HCP * resultSet$coefficients[3]) + (CCP * resultSet$coefficients[4]) + (HCPSquare * resultSet$coefficients[5]) + (CCPSquare * resultSet$coefficients[6]) + (HCPCube * resultSet$coefficients[7]) + (CCPCube * resultSet$coefficients[8]) + resultSet$coefficients[1]
    consumptionChangePoint <- (timeOfWeek * resultSet$coefficients[2]) + resultSet$coefficients[1]
    consumptionNormalized <- consumption - (consumptionCalculated - consumptionChangePoint)
    RSquare <- 1 - (sum((consumption - consumptionCalculated)^2) / sum((consumption - mean(consumption))^2))
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dataCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dataCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, timeOfWeek, HCP, CCP, HCPSquare, CCPSquare, HCPCube, CCPCube, consumptionCalculated, consumptionChangePoint, consumptionNormalized)
    dataFrame2 <- data.frame(RSquare, RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="HourlyOutputWeatherNorm.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="HourlyOutputWeatherNorm.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="HourlyOutputWeatherNorm.xlsx", sheetName="sheet3", append=TRUE)
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
    CCP <- c()
    HCP <- c()
    
    dayCount <- 0
    endIndex <- 0
    endWHIndex <- 0
    startIndex <- 0
    while(endIndex < length(allSampleDate))
    {
      #Yeni günün deðerlerini okumak için
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
        #Günlük toplam harcama bulunur
        consumption <- append(consumption, sum(allConsumption[startIndex:endWHIndex]))
        sampleDate <- append(sampleDate, substr(allSampleDate[startIndex], 1, 14))
        
        #Günlük ortalama sýcaklýk bulunur
        dailyAvgTmp <- mean(allTemperature[startIndex:endWHIndex])
        temperature <- append(temperature, dailyAvgTmp)
        
        coolingDiff <- dailyAvgTmp - coolingChangePoint
        if(coolingDiff < 0)
        {
          CCP <- append(CCP, 0)
        }
        else
        {
          CCP <- append(CCP, coolingDiff)
        }
        
        heatingDiff <- heatingChangePoint - dailyAvgTmp
        if(heatingDiff < 0)
        {
          HCP <- append(HCP, 0)
        }
        else
        {
          HCP <- append(HCP, heatingDiff)
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
    
    HCPSquare <- HCP^2
    CCPSquare <- CCP^2
    HCPCube <- HCP^3
    CCPCube <- CCP^3
    
    resultSet <- lm(consumption ~ dayOfWeek + HCP + CCP + HCPSquare + CCPSquare + HCPCube + CCPCube)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    
    variableCount <- 7
    consumptionCalculated <- (dayOfWeek * resultSet$coefficients[2]) + (HCP * resultSet$coefficients[3]) + (CCP * resultSet$coefficients[4]) + (HCPSquare * resultSet$coefficients[5]) + (CCPSquare * resultSet$coefficients[6]) + (HCPCube * resultSet$coefficients[7]) + (CCPCube * resultSet$coefficients[8]) + resultSet$coefficients[1]
    consumptionChangePoint <- (dayOfWeek * resultSet$coefficients[2]) + resultSet$coefficients[1]
    consumptionNormalized <- consumption - (consumptionCalculated - consumptionChangePoint)
    RSquare <- 1 - (sum((consumption - consumptionCalculated)^2) / sum((consumption - mean(consumption))^2))
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dayCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dayCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, dayOfWeek, HCP, CCP, HCPSquare, CCPSquare, HCPCube, CCPCube, consumptionCalculated, consumptionChangePoint, consumptionNormalized)
    dataFrame2 <- data.frame(RSquare, RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="DailyOutputWeatherNorm.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="DailyOutputWeatherNorm.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="DailyOutputWeatherNorm.xlsx", sheetName="sheet3", append=TRUE)
  }
)