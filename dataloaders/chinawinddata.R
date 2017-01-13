library(readxl)
library(dplyr)

ExcelToPOSIXDate <- function(edate,etime){
  #nulldate <- "1900-01-01 00:00:00"
  #dates <- strptime(nulldate,"%Y-%m-%d %T") + (86400 * (edate - 2)) + (60*15*etime*96)  - 3148
  dates <- as.POSIXct((edate + etime - 25569) * 86400)  
  return(dates)
}

ExcelToPOSIXDate15 <- function(edate,etime){
  dates <- as.POSIXct(round(((edate + etime - 25569) * 86400)/900)*900) 
  return(dates)
}

POSIXDate15 <- function(posixdate){
  dates <- as.POSIXct(round(as.numeric(posixdate)/900)*900,tz="UTC", origin = "1970-01-01")
  return(dates)
}

#as.POSIXct(ExcelToPOSIXDate(inputForecastData[1,"Date"],inputForecastData[1,"Time"]), origin = "1960-01-01")

chinawindGenerateStationData <- function(stationdatadir="../Data/ChinaWind/StationC"){
  for(i in 0:30){
    currentDate <- as.POSIXct("2015-08-01",tz="UTC") + (i-1)*86400
    currentDateStr <- format(currentDate,'%Y%m%d') 
    stationdatafile <- paste(stationdatadir,"/","SXTRPL",currentDateStr,".xlsx",sep="")
    cat(stationdatafile,"\n")
    inputForecastData <- read_excel(stationdatafile,
                                    skip = 1)
    if(ncol(inputForecastData)==10){
      inputForecastData <- inputForecastData[,-9]
      inputForecastData <- inputForecastData[,-9]
    }
    colnames(inputForecastData) <- c("Date","Time","WindSpeed","WindDirection","Temperature","Pressure","Humidity","Z-Component")
    #inputForecastData$Date <- inputForecastData$Date - 86400
    times <- as.numeric(inputForecastData$Time) + 2209161600
    inputForecastData$Date <- as.POSIXct(inputForecastData$Date + times)
    inputForecastData$Date <- format(inputForecastData$Date,"%Y-%m-%d %T")
    
    inputForecastData <- inputForecastData[,-2]
    inputForecastData <- inputForecastData[inputForecastData$Date <= currentDate + 2*86400,]
    if(i==0){
      stationData = inputForecastData
    } else {
      stationData <- rbind(stationData, inputForecastData) 
    }
  }
  write.table(file = paste(stationdatadir, "/Station1DayAhead.csv",sep=""),
              stationData,sep=";",row.names=F)
  
}

chinawinddata <- function(winddatafile="../Data/ChinaWind/alldata.csv"){
  tbl <- read.table(winddatafile, header=T, sep=";")
  tbl$Date <- as.character(tbl$Date)
  tbl$actualwindspeed[tbl$actualwindspeed < 0] = NA
  colnames(tbl)[9] <- "Target"
  return(tbl)
}