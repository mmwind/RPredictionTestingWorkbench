source("utils.R")
source("prediction.R")
source("featureSelection.R")

sourceDirectory("./dataloaders/","Datasets to test:")
sourceDirectory("./predictors/","Predictors to test:")
sourceDirectory("./featureSelectors/","Feature selectors to test:")
sourceDirectory("./preprocessors/","Preprocessors to test:")

reportDir <- makeReportDirectory()

rawdata <- chinawinddata()
rawdata <- rawdata[,-8] # delete actual wind power
rawdata <- na.omit(rawdata)



dVecLen = 32
dID = 32
featurenum <- 9
#output <- matrix(0,nrow=nrow(rawdata),ncol=featurenum)
output <- data.frame()
outputIdx = 0
invalidCounter = 0

while(dID < nrow(rawdata)){
  dID = dID + 1
  isValid <- T
  dVector <- as.data.frame(rawdata[(dID - dVecLen):(dID - 1),])
  dt <- 0
  for(i in 1:31){
    dt <- as.POSIXct(dVector[i+1,1])-as.POSIXct(dVector[i,1])
    if(dt != 15){
      isValid <- F
      invalidCounter = invalidCounter +1
    }
    print(dt)
  }
  if(isValid){
    dVector <- dVector[,-1]
    outputIdx = outputIdx + 1
    dTargetDate <- rawdata$Date[dID]
    dTargetValue <- rawdata$Target[dID]
    colnames(dVector)[7] <- "PastTarget"
    output <- rbind(output,generateFeatures(dVector,dTargetValue,dTargetDate))
    #output[outputIdx,] <- generateFeatures(dVector,dTargetValue,dTargetDate)
  }
    print(paste(dID,isValid))
}

output <- output[1:outputIdx,]
colnames(output) <- names(generateFeatures(dVector,dTargetValue,dTargetDate))
output <- as.data.frame(output)



generateFeatures <- function(dVector,dTargetValue,dTargetDate){
  outp <- data.frame(Date=as.POSIXct(dTargetDate), dVector[nrow(dVector),],Target = dTargetValue )
  #Historical <- dVector$PastTarget
  return(outp)
}