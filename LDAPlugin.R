library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")


input <- function(inputfile) {
	pfix = prefix()
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
   # Need to get the three files
   csvfile <<- paste(pfix, parameters["csvfile", 2], sep="/")

   training <<- read.csv(csvfile)
   fitControl <<- readRDS(paste(pfix, parameters["fitControl", 2], sep="/"))
   myType <<- "Regular"
  if ("type" %in% rownames(parameters)) {
  myType  <<- as.integer(parameters["type", 2])
  }


}

run <- function() {}

output <- function(outputfile) {
set.seed(825)
if (myType == "step") {
result <- train(Class ~ ., data = training, 
                 method = "stepLDA", 
                 trControl = fitControl)
}
else if (myType == "loc") {
result <- train(Class ~ ., data = training, 
                 method = "loclda", 
                 trControl = fitControl)
}
else if (myType == "penalized") {
result <- train(Class ~ ., data = training, 
                 method = "PenalizedLDA", 
                 trControl = fitControl)
}
else if (myType == "sparse") {
result <- train(Class ~ ., data = training, 
                 method = "sparseLDA", 
                 trControl = fitControl)
}

else {
result <- train(Class ~ ., data = training, 
                 method = "lda", 
                 trControl = fitControl)
}
print(result)
saveRDS(result, outputfile)
}
