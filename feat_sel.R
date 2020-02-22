
Information_gain <- function() {
  option <- readline(prompt = "Which dataset do u want to perform information Gain \n 1. Whole Dataset
      \n 2.Grouped according to importance \n 3. Correlation in dataset \n
      4. correaltion in importance grouped dataset")
  
  fil_val <- generateFilterValuesData(task, method = c("FSelectorRcpp_information.gain"))
  print(fil_val)
  
  IG_data <- mlr_data[, which(fil_val$data$value != 0)]
  IG_data$label <- mlr_data$label
  
  if (option == 1) {
    task <- makeClassifTask(data = IG_data, target = "label")
    
    rf_tune <- tuning(task)
    lrn <-
      makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    reduced_list <- length(df) - 1
    imp <- feat_importance(mod, reduced_list)
    
  }  ## GROUPING ACCORDING TO IMPORTANCE VALUE ##
  else if (option == 2) {
    grouped_data <- importance_grouping(imp)
    
    if (!is.null(grouped_data))
    {
      grouped_data$label <- mlr_data$label
      task = makeClassifTask(data = grouped_data, target = "label")
      rf_tune <- tuning(task)
      lrn <- makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
      res <- resampling(lrn, task)
      mod <- train(lrn, task)
      #res <- resampling(lrn,task)
      reduced_list <- c(colnames(grouped_data))
      imp <- feat_importance(mod, reduced_list)
    }
  }
  
  else if (option == 3) {
    corr_data <- correlation(IG_data)
    task <- makeClassifTask(data = corr_data, target = 'label')
    rf_tune <- tuning(task)
    lrn <-
      makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    reduced_list <- c(colnames(corr_data))
    imp <- feat_importance(mod, reduced_list)
  }
  else if (option == 4) {
    grouped_data <- importance_grouping(imp)
    corr_grouped_data <- correlation(grouped_data)
    task = makeClassifTask(data = corr_grouped_data, target = "label")
    rf_tune <- tuning(task)
    lrn <-
      makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    reduced_list <- c(colnames(corr_grouped_data))
    imp <- feat_importance(mod, reduced_list)
  }
}



Chi_square <- function() {
  option <- readline(prompt = "Which dataset do u want to perform ChiSquare \n 2.Grouped according to importance\n3.Correlation in dataset\n4. correaltion in importance grouped dataset")
  
  fil_val <- generateFilterValuesData(task, method = c("FSelector_chi.squared"))
  fil_val
  
  Chi_data <- mlr_data[, which(fil_val$data$value != 0)]
  Chi_data$label <- mlr_data$label
  #if (option == 1) {
    task <- makeClassifTask(data = Chi_data, target = "label")
    
    rf_tune <- tuning(task)
    lrn <-
      makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    reduced_list <- 1 : (length(df) - 1)
    imp <- feat_importance(mod, reduced_list)
    
  ## GROUPING ACCORDING TO IMPORTANCE VALUE ##
  if (option == 2) {
    
    grouped_data <- importance_grouping(imp)
    
    if (!is.null(grouped_data))
    {
      grouped_data$label <- mlr_data$label
      task = makeClassifTask(data = grouped_data, target = "label")
      rf_tune <- tuning(task)
      lrn <- makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
      res <- resampling(lrn, task)
      mod <- train(lrn, task)
      #res <- resampling(lrn,task)
      reduced_list <- c(colnames(grouped_data))
      imp <- feat_importance(mod, reduced_list)
    }
  }
  
  else if (option == 3) {
    corr_data <- correlation(Chi_data)
    task <- makeClassifTask(data = corr_data, target = 'label')
    rf_tune <- tuning(task)
    lrn <-
      makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    reduced_list <- c(colnames(corr_data))
    imp <- feat_importance(mod, reduced_list)
  }
  else if (option == 4) {
    grouped_data <- importance_grouping(imp)
    corr_grouped_data <- correlation(grouped_data)
    task = makeClassifTask(data = corr_grouped_data, target = "label")
    rf_tune <- tuning(task)
    lrn <-
      makeLearner(
        "classif.randomForest",
        predict.type = "prob",
        predict.threshold = rf_tune$threshold,
        par.vals = rf_tune$x
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    reduced_list <- c(colnames(corr_grouped_data))
    imp <- feat_importance(mod, reduced_list)
  }
}


Random_search <- function() {
  option <- readline(prompt = "Which dataset do u want to perform in Random \n 1.Grouped according to importance\n2.Correlation in dataset\n3. correaltion in importance grouped dataset")
  
  rf_tune <- tuning(task)
  lrn <-
    makeLearner(
      "classif.randomForest",
      predict.type = "prob",
      predict.threshold = rf_tune$threshold,
      par.vals = rf_tune$x
    )
  lrn <-
    makeFeatSelWrapper(
      learner = lrn,
      resampling = rdesc,
      control = makeFeatSelControlRandom(maxit = 20L),
      show.info = FALSE
    )
  
  mod <- mlr::train(lrn,task = task)
  sfeat <- getFeatSelResult(mod)
  res = resampling(lrn,task)
  imp <- feat_importance(mod,sfeat$x)
  
  ## GROUPING ACCORDING TO IMPORTANCE VALUE ##
  if (option == 2) {
    
    grouped_data <- importance_grouping(imp)
    
    if (!is.null(grouped_data))
    {
      grouped_data$label <- mlr_data$label
      task = makeClassifTask(data = grouped_data, target = "label")
      rf_tune <- tuning(task)
      lrn <- makeLearner("classif.randomForest",predict.type = "prob",predict.threshold = rf_tune$threshold,par.vals = rf_tune$x)
      lrn <-
        makeFeatSelWrapper(
          learner = lrn,
          resampling = rdesc,
          control = makeFeatSelControlRandom(maxit = 20L),
          show.info = FALSE
        )
      res <- resampling(lrn, task)
      mod <- train(lrn, task)
      sfeat <- getFeatSelResult(mod)
      #res <- resampling(lrn,task)
      #reduced_list <- c(colnames(grouped_data))
      imp <- feat_importance(mod, sfeat$x)
    }
  }
  
  else if (option == 3) {
    corr_data <- correlation(Chi_data)
    task <- makeClassifTask(data = corr_data, target = 'label')
    rf_tune <- tuning(task)
    lrn <- makeLearner("classif.randomForest",predict.type = "prob",predict.threshold = rf_tune$threshold,par.vals = rf_tune$x)
    lrn <-
      makeFeatSelWrapper(
        learner = lrn,
        resampling = rdesc,
        control = makeFeatSelControlRandom(maxit = 20L),
        show.info = FALSE
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    sfeat <- getFeatSelResult(mod)
    #reduced_list <- c(colnames(corr_data))
    imp <- feat_importance(mod, sfeat$x)
  }
  else if (option == 4) {
    grouped_data <- importance_grouping(imp)
    corr_grouped_data <- correlation(grouped_data)
    task = makeClassifTask(data = corr_grouped_data, target = "label")
    rf_tune <- tuning(task)
    lrn <- makeLearner("classif.randomForest",predict.type = "prob",predict.threshold = rf_tune$threshold,par.vals = rf_tune$x)
    lrn <-
      makeFeatSelWrapper(
        learner = lrn,
        resampling = rdesc,
        control = makeFeatSelControlRandom(maxit = 20L),
        show.info = FALSE
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    sfeat <- getFeatSelResult(mod)
    #reduced_list <- c(colnames(corr_grouped_data))
    imp <- feat_importance(mod, sfeat$x)
  }
}

GA_search <- function() {
  option <- readline(prompt = "Which dataset do u want to perform in GA search \n 1.Grouped according to importance\n2.Correlation in dataset\n3. correaltion in importance grouped dataset")
  
  rf_tune <- tuning(task)
  lrn <-
    makeLearner(
      "classif.randomForest",
      predict.type = "prob",
      predict.threshold = rf_tune$threshold,
      par.vals = rf_tune$x
    )
  lrn <-
    makeFeatSelWrapper(
      learner = lrn,
      resampling = rdesc,
      control = makeFeatSelControlGA(maxit = 20L),
      show.info = FALSE
    )
  
  mod <- mlr::train(lrn,task = task)
  sfeat <- getFeatSelResult(mod)
  res = resampling(lrn,task)
  imp <- feat_importance(mod,sfeat$x)
  
  ## GROUPING ACCORDING TO IMPORTANCE VALUE ##
  if (option == 2) {
    
    grouped_data <- importance_grouping(imp)
    
    if (!is.null(grouped_data))
    {
      grouped_data$label <- mlr_data$label
      task = makeClassifTask(data = grouped_data, target = "label")
      rf_tune <- tuning(task)
      lrn <- makeLearner("classif.randomForest",predict.type = "prob",predict.threshold = rf_tune$threshold,par.vals = rf_tune$x)
      lrn <-
        makeFeatSelWrapper(
          learner = lrn,
          resampling = rdesc,
          control = makeFeatSelControlGA(maxit = 20L),
          show.info = FALSE
        )
      res <- resampling(lrn, task)
      mod <- train(lrn, task)
      sfeat <- getFeatSelResult(mod)
      #res <- resampling(lrn,task)
      #reduced_list <- c(colnames(grouped_data))
      imp <- feat_importance(mod, sfeat$x)
    }
  }
  
  else if (option == 3) {
    corr_data <- correlation(Chi_data)
    task <- makeClassifTask(data = corr_data, target = 'label')
    rf_tune <- tuning(task)
    lrn <- makeLearner("classif.randomForest",predict.type = "prob",predict.threshold = rf_tune$threshold,par.vals = rf_tune$x)
    lrn <-
      makeFeatSelWrapper(
        learner = lrn,
        resampling = rdesc,
        control = makeFeatSelControlGA(maxit = 20L),
        show.info = FALSE
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    sfeat <- getFeatSelResult(mod)
    #reduced_list <- c(colnames(corr_data))
    imp <- feat_importance(mod, sfeat$x)
  }
  else if (option == 4) {
    grouped_data <- importance_grouping(imp)
    corr_grouped_data <- correlation(grouped_data)
    task = makeClassifTask(data = corr_grouped_data, target = "label")
    rf_tune <- tuning(task)
    lrn <- makeLearner("classif.randomForest",predict.type = "prob",predict.threshold = rf_tune$threshold,par.vals = rf_tune$x)
    lrn <-
      makeFeatSelWrapper(
        learner = lrn,
        resampling = rdesc,
        control = makeFeatSelControlGA(maxit = 20L),
        show.info = FALSE
      )
    res <- resampling(lrn, task)
    mod <- train(lrn, task)
    res <- resampling(lrn, task)
    sfeat <- getFeatSelResult(mod)
    #reduced_list <- c(colnames(corr_grouped_data))
    imp <- feat_importance(mod, sfeat$x)
  }
}