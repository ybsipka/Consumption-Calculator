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
############## CALCULATE HOURLY ##################
btn_hc <- gbutton(
  text      = "Calculate Hourly",
  container = ggroup(container = win),
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
    
    #sampleStartHour <- 10
    #sampleEndHour <- 17
    #Bir gün ilerlediğimizde timeOfWeek değişkeninin ne kadar artacağı
    timeOfWeekFactor <- (sampleEndHour - sampleStartHour + 1)
    
    for(i in 1:length(allSampleDate))
    {
      day <- substr(allSampleDate[i], 12, 14)
      hour <- as.numeric(substr(allSampleDate[i], 16, 17))
      
      #Haftaiçi ve çalışma saatlerindeki datalar üzerinde çalışacağız.
      if(day %in% workDays[[1]] && hour >= sampleStartHour && hour <= sampleEndHour)
      { 
        sampleDate <- append(sampleDate, toString(allSampleDate[i]))
        temperature <- append(temperature, allTemperature[i])
        consumption <- append(consumption, allConsumption[i])
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
    
    dataCount <- length(temperature)
    minTemp <- min(temperature)
    maxTemp <- max(temperature)
    equalDiff <- (maxTemp - minTemp) / 6
    
    BList <- c(minTemp, minTemp + equalDiff, minTemp + (equalDiff * 2), minTemp + (equalDiff * 3), minTemp + (equalDiff * 4), maxTemp)
    TList <- list()
    TList <- c(TList, list(c(), c(), c(), c(), c(), c()))
    
    for(i in 1:dataCount)
    {
      index <- 1
      currentTemp <- temperature[i]
      for(index in 1:5)
      {
        if(currentTemp > BList[index])
        {
          if(index == 1)
          {
            TList[[index]] <- append(TList[[index]], BList[index])
          }
          else
          {
            TList[[index]] <- append(TList[[index]], BList[index] - BList[index - 1])
            if(index == 5)
            {
              TList[[index+1]] <- append(TList[[index+1]], currentTemp - BList[index])
            }
          }
        }
        else
        {
          if(index == 1)
          {
            TList[[index]] <- append(TList[[index]], currentTemp)
          }
          else
          {
            TList[[index]] <- append(TList[[index]], currentTemp - BList[index -1])
          }
          j<-index+1
          while(j < 7)
          {
            TList[[j]] <- append(TList[[j]], 0)
            j <- j+1
          }
          break
        }		
      }
    }
    
    T1 <- TList[[1]]
    T2 <- TList[[2]]
    T3 <- TList[[3]]
    T4 <- TList[[4]]
    T5 <- TList[[5]]
    T6 <- TList[[6]]
    
    resultSet <- lm(consumption ~ timeOfWeek + T1 + T2 + T3 + T4 + T5 + T6)
    summary(resultSet)
    for(i in 1:length(resultSet$coefficients))
    {
      if(is.na(resultSet$coefficients[i]))
      {
        resultSet$coefficients[i] <- 0
      }
    }
    variableCount <- 7
    consumptionCalculated <- (timeOfWeek * resultSet$coefficients[2]) + (T1 * resultSet$coefficients[3]) + (T2 * resultSet$coefficients[4]) + (T3 * resultSet$coefficients[5]) + (T4 * resultSet$coefficients[6]) + (T5 * resultSet$coefficients[7]) + (T6 * resultSet$coefficients[8]) + resultSet$coefficients[1]
    RMSE <- sum((consumption - consumptionCalculated)^2) / (dataCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((dataCount - variableCount) * mean(consumption))
    
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, timeOfWeek, T1, T2, T3, T4, T5, T6, consumptionCalculated)
    dataFrame2 <- data.frame(RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="HourlyOutput.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="HourlyOutput.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="HourlyOutput.xlsx", sheetName="sheet3", append=TRUE)
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




############## CALCULATE MONTHLY ##################
btn_mc <- gbutton(
  text      = "Calculate Monthly",
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
    sampleMonths <- c()
    
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
      
      #Günlerin Hangi Aya Ait Olduğu Eklenir
      sampleMonths <- append(sampleMonths, substr(allSampleDate[startIndex], 4, 6)) 
    }
    
    consumption <- c()
    temperature <- c()
    sampleDate <- c()
    HDD <- c()
    CDD <- c()
    
    monthStartIndex <- 1
    monthEndIndex <- 1
    monthCount <- 0
    while(1)
    {
      if(length(dailyConsumption) < monthStartIndex)
      {
        break;
      }
      monthCount <- monthCount + 1
      sampleDate <- append(sampleDate, sampleMonths[monthStartIndex])
      if(sampleMonths[monthStartIndex] == "Jan")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }	
      else if(sampleMonths[monthStartIndex] == "Feb")
      {
        monthEndIndex <- monthStartIndex + 27
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Mar")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Apr")
      {
        monthEndIndex <- monthStartIndex + 29
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }	
      else if(sampleMonths[monthStartIndex] == "May")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Jun")
      {
        monthEndIndex <- monthStartIndex + 29
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Jul")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Aug")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Sep")
      {
        monthEndIndex <- monthStartIndex + 29
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Oct")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }	
      else if(sampleMonths[monthStartIndex] == "Nov")
      {
        monthEndIndex <- monthStartIndex + 29
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }
      else if(sampleMonths[monthStartIndex] == "Dec")
      {
        monthEndIndex <- monthStartIndex + 30
        #Aylık toplam harcama bulunur. (Günlük değerler toplanarak)
        consumption <- append(consumption, sum(dailyConsumption[monthStartIndex:monthEndIndex]))
        temperature <- append(temperature, sum(dailyTemperature[monthStartIndex:monthEndIndex]))
        
        #Haftalık toplam HDD ve CDD bulunur. (Günlük değerler toplanarak)
        HDD <- append(HDD, sum(dailyHDD[monthStartIndex:monthEndIndex]))
        CDD <- append(CDD, sum(dailyCDD[monthStartIndex:monthEndIndex]))
      }					
      monthStartIndex <- monthEndIndex + 1
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
    RMSE <- sum((consumption - consumptionCalculated)^2) / (monthCount - variableCount)
    CVRMSE <- RMSE / mean(consumption)
    NBE <- 100 * sum(consumption - consumptionCalculated) / sum(consumption)
    NMBE <- 100 * sum(consumption - consumptionCalculated) / ((monthCount - variableCount) * mean(consumption))
    
    dataFrame1 <- data.frame(sampleDate, consumption, temperature, HDD, CDD, HDDSquare, CDDSquare, HDDCube, CDDCube, consumptionCalculated)
    dataFrame2 <- data.frame(RMSE, CVRMSE, NBE, NMBE)
    dataFrame3 <- resultSet$coefficients
    
    write.xlsx(dataFrame1, file="MonthlyOutput.xlsx", sheetName="sheet1")
    write.xlsx(dataFrame2, file="MonthlyOutput.xlsx", sheetName="sheet2", append=TRUE)
    write.xlsx(dataFrame3, file="MonthlyOutput.xlsx", sheetName="sheet3", append=TRUE)
    
  }
)