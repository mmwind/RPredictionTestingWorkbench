dumbPredictor1 <- function(olddata,newdata){
  return(data.frame(y = olddata[nrow(olddata),]$Target))
}