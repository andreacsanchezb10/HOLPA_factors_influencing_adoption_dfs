EXP <- function(dataset_name, dataset, target, test = NULL, task = "C", metric = NULL, modeler = NULL, lasso_metric = NULL, lasso_modeler = NULL) {
  
  ## task = "C" for classification, "R" for regresion, "S" for Survival
  ## Choose model and metric functions due to task for SES and LASSO
  if (task == "C") {
    
    ## Classification task (logistic regression)
    if (is.null(metric)) {
      metricFunction <- auc.mxm
    } else {
      metricFunction <- metric
    }
    
    if (is.null(modeler)) {
      modelerFunction <- glm.mxm
    } else {
      modelerFunction <- modeler
    }
    
    if (is.null(test)) {
      test <- "testIndLogistic"
    } else {
      test <- test
    }
    
  } else if (task == "R") {
    
    ## Regression task (linear regression)
    if (is.null(metric)) {
      metricFunction <- mse.mxm
    } else {
      metricFunction <- metric
    }
    
    if (is.null(modeler)) {
      modelerFunction <- lm.mxm
    } else {
      modelerFunction <- modeler
    }
    
    if (is.null(test)) {
      test <- "testIndFisher"
    } else {
      test <- test
    }
    
  } else if (task == "S") {
    
    #cox survival analysis (cox regression)
    if (is.null(metric)) {
      metricFunction <- ci.mxm
    } else {
      metricFunction <- metric
    }
    
    if (is.null(modeler)) {
      modelerFunction <- coxph.mxm
    } else {
      modelerFunction <- modeler
    }
    
    if (is.null(test)) {
      test <- "censIndLR"
    } else {
      test <- test
    }
    
  } else {
    stop("Please provide a valid task argument 'C'-classification, 'R'-regression, 'S'-survival.")
  }
  
  ## SES configurations
  alphas <- c(0.1, 0.05, 0.01) 
  max_ks <- c(2:5) 
  
  ##############################################################
  # 1. partition the dataset to 50% training and 50% hold-out. #
  ##############################################################
  
  k <- 10
  
  ## stratified cross validation
  if (survival::is.Surv(target)) {
    folds <- TunePareto::generateCVRuns(target[,2], ntimes = 1, nfold = k, leaveOneOut = FALSE, stratified = TRUE)
  } else {
    folds <- TunePareto::generateCVRuns(target, ntimes = 1, nfold = k, leaveOneOut = FALSE, stratified = TRUE)
  }
  #create train instances vector
  train_samples <- c()
  for (i in which(c(1:k) != k)) {
    train_samples <- c(train_samples, folds[[1]][[i]])
  }
  train_set <- dataset[train_samples,]
  train_target <- target[train_samples]
  hold_out <- dataset[folds[[1]][[k]],]
  hold_out_target <- target[folds[[1]][[k]]]
  
  #create the 10 folds on the training set for the cross validation run
  if (survival::is.Surv(target))
  {
    cv_folds <- TunePareto::generateCVRuns(train_target[,2], ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = TRUE)
  }else
  {
    cv_folds <- TunePareto::generateCVRuns(train_target, ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = TRUE)
  }
  
  ########################################################################
  # 2. Cross validation on the training to get the best hyper-parameters #
  # for SES and LASSO with the cv.ses function                           #
  ########################################################################
  
  cv_time <- system.time({
    best_model <- cv.ses(target = train_target, dataset = train_set, kfolds = 10, folds = cv_folds[[1]], alphas = alphas, max_ks = max_ks, task = task,
                         metric = metric, modeler = modeler, ses_test = test)
  })
  best_a <- best_model$best_configuration$a
  best_max_k <- best_model$best_configuration$max_k
  
  ## save(best_model, file = "safety.RData")
  
  ## get the set of multiple signatures on the training set
  SES_res <- SES(train_target, train_set, max_k = best_max_k, threshold=best_a, test = test)
  summary(SES_res)
  ref_signature <- SES_res@selectedVars
  signatures <- as.matrix(SES_res@signatures)
  
  ## save(SES_res, file = "safety.RData")
  
  ############################################################################
  # 3. Train a model for each signature on the training data using the best  #
  # hyper-parameters from step 2.                                            #
  ############################################################################
  
  SES_performance <- vector("numeric", dim(signatures)[1])
  SES_preds <- vector("list", dim(signatures)[1])
  
  for (i in 1:dim(signatures)[1])
  {
    #if (any(is.na(signatures[i,])) == TRUE)
    cur_sign = signatures[i,]
    sign_data = matrix(nrow = dim(train_set)[1] , ncol = dim(signatures)[2])
    sign_test = matrix(nrow = dim(hold_out)[1] , ncol = dim(signatures)[2])
    for (j in 1:length(cur_sign))
    {
      sign_data[,j] = train_set[,cur_sign[j]]
      sign_test[,j] = hold_out[,cur_sign[j]]
    }
    
    #regression task (linear model)
    preds<-modelerFunction(train_target, sign_data, sign_test, wei = NULL)
    print(class(preds)) 
    str(preds)
    
    performance = metricFunction(preds$preds, hold_out_target)
    
    SES_performance[i] = performance
    SES_preds[[i]] = preds
  }
  
  #the result list to be returned
  RES <- NULL
  #RES$dataset_name = dataset_name
  RES$task = task
  RES$test = test
  RES$folds = folds
  RES$cv_folds = cv_folds
  RES$best_model = best_model
  RES$cv_time = cv_time
  
  RES$SES_res = SES_res
  RES$signatures = signatures
  RES$SES_performance = SES_performance #all the signature perfs on the holdout
  #RES$SES_preds = SES_preds #all the predictions for each signature
  
  return(RES)
}
