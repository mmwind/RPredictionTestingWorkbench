#Select the most informative features using Random Feature Elimination
library(caret)
library(mlbench)

# function returns selected feature subset
RFE <- function(training){
  targetIndex <- "Target"
  xtrain <- training[,-length(training[1,])]
  ytrain <- training[, targetIndex]
  # define the control using a random forest selection function
  control <- rfeControl(functions=rfFuncs, method="cv", number=2, verbose = T)
  # run the RFE algorithm
  results <- rfe(xtrain, ytrain, sizes=c(1:length(xtrain[1,])), rfeControl=control)
  return(predictors(results))
}
