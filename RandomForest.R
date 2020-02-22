#this file performs Random Forest with help of different sets of Feature Selection

library(mlr)
library(iml)
set.seed(123)



#preparing the data for processing
mlr_data <- as.data.frame(scale(df[, 1:(length(df) - 1)]))
mlr_data$label <- factor(df$label)
NAN_col <- sapply(mlr_data, function(x)
  all(is.nan(x)))
mlr_data <- mlr_data[, !NAN_col]


#initializing the task,learner and resampling method
task <- makeClassifTask(data = mlr_data, target = "label")
rdesc = makeResampleDesc("LOO")
lrn <- makeLearner("classif.randomForest", predict.type = "prob")


#The function for tuning the parameter
tuning <- function(task) {
  rf_param <- makeParamSet(
    makeIntegerParam("ntree", lower = 50, upper = 500),
    makeIntegerParam("mtry", lower = 3, upper = 8)
  )
  
  rancontrol <- makeTuneControlGrid(tune.threshold = TRUE)
  rf_tune <-
    tuneParams(
      learner = lrn,
      resampling = rdesc,
      task = task,
      par.set = rf_param,
      control = rancontrol,
      measure = acc
    )
  return(rf_tune)
}

#To validate the dataset
resampling <- function(lrn, task) {
  res <-
    resample(
      lrn,
      task,
      rdesc,
      show.info = TRUE,
      models = TRUE,
      measures = list(acc, setAggregation(acc, test.sd))
    )
  print(res)
  #Confusion Matrix
  print(caret::confusionMatrix(res$pred$data$truth, res$pred$data$response))
  
  #To plot the feature importane
  #imp = feat_importance(mod,df)
  return(res)
}



#for plotting the importance value of each feature
feat_importance <- function(mod, length_filter) {
  library(iml)
  predictor = Predictor$new(mod, data = df[, length_filter], y = df$label)
  imp = FeatureImp$new(predictor, loss = "ce")
  print(plot(imp))
  return(imp)
}


correlation <- function(dataset) {
  corr_data = cor(dataset[, 1:length(dataset) - 1], method = "spearman")
  hc <- caret::findCorrelation(corr_data, cutoff = 0.9)
  hc <- sort(hc)
  reduced_Data = dataset[, -c(hc)]
  #print (reduced_Data)
  reduced_Data$label <- mlr_data$label
  return(reduced_Data)
}

importance_grouping <- function(imp)
{
  option <-
    readline(
      prompt = "Do u want to group the features according to importance value?
      \n1. Yes,with importance > 1\n2.Yes, group feature with same importance value\n3.No \n"
    )
  #group with importance > 1
  if (option == 1) {
    x <- which(imp$results$importance > 1)
    dataset <- df[, c(imp$results$feature[c(x)])]
    dataset <- as.data.frame(scale(dataset))
    dataset$label <- factor(df$label)
    NAN_col <- sapply(dataset, function(x)
      all(is.nan(x)))
    dataset <- dataset[, !NAN_col]
    
  } else if (option == 2) {
    #group as cluster center
    data <- data.frame(imp$results$feature , imp$results$importance)
    colnames(data) <- c("feat", "importance")
    feat_grouped_IG <-
      sqldf::sqldf("Select * From data Group By importance")
    dataset <- df[, c(feat_grouped_IG$feat)]
    dataset <- as.data.frame(scale(dataset))
    dataset$label <- factor(df$label)
    NAN_col <- sapply(dataset, function(x)
      all(is.nan(x)))
    dataset <- dataset[, !NAN_col]
  }
  else
    dataset = NULL
  
  return(dataset)
}
