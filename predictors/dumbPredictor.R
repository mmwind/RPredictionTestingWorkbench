dumbPredictor <- function(olddata,newdata){
  return(data.frame(y = olddata[nrow(olddata),]$Target))
}