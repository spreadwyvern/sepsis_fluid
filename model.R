TrainXGB <- function(x.train, x.test, y.train, y.test){
  
  print(paste0("train data: nrow=",nrow(x.train),", ncol=",ncol(x.train)))
  print(paste0("test data: nrow=", nrow(x.test) ,", ncol=",ncol(x.test )))
  
  df.train <- xgb.DMatrix(data=as.matrix(x.train), label=as.matrix(y.train))
  df.test <- xgb.DMatrix(data=as.matrix(x.test), label=as.matrix(y.test))
  watchlist <- list(train = df.train, test = df.test)
  
  # parameters
  if(task=='uo'){
    config <- list(
      name = paste0("xgb_uo"), # result will be stored in train/name
      depth = 17,
      eta = 0.01,
      nround = 2000,
      rowsample = 1,
      colsample = 0.6,
      ntree = 1,
      weight_balance = TRUE,
      early_stop = 20
    )
  } else if(task=='bo'){
    config <- list(
      name = paste0("xgb_bo"), # result will be stored in train/name
      depth = 18,
      min_child_weight = 75,
      gamma = 1,
      eta = 0.01,
      nround = 2000,
      rowsample = 0.8,
      colsample = 0.8,
      ntree = 1,
      weight_balance = TRUE,
      early_stop = 20
    )
  } else {
    print('unassigned task')
    stop()
  }
  depth  <- ifelse(is.null(config$depth),3,config$depth)
  eta    <- ifelse(is.null(config$eta),0.1,config$eta)
  nround <- ifelse(is.null(config$nround),200,config$nround)
  rowsample <- ifelse(is.null(config$rowsample),0.75,config$rowsample)
  colsample <- ifelse(is.null(config$colsample),0.75,config$colsample)
  ntree <- ifelse(is.null(config$ntree),1,config$ntree)
  pos_weight <- ifelse(config$weight_balance == TRUE, (sum(abs(y.train)==0)/sum(abs(y.train)>0)),1)
  early_stop <- ifelse(is.null(config$early_stop),round(nround*0.1,0),config$early_stop)
  model_name <- paste0(config$name, ".model")
  
  xgb.mit <- xgb.train(data = df.train,
                       nthread = 6, nround = nround, 
                       max_depth = depth,
                       eta=eta, 
                       objective= 'binary:logistic',#'multi:softmax', #'reg:linear',
                       # objective='reg:linear',
                       # num_class = 3,
                       
                       subsample= rowsample,  colsample_bylevel= colsample, 
                       num_parallel_tree = ntree, 
                       eval_metric="logloss",
                       # eval_metric = "mlogloss",
                       maximize = FALSE,
                       alpha=0, lambda=1, 
                       scale_pos_weight = pos_weight, 
                       early_stopping_rounds = early_stop,
                       base_score=0.5, watchlist = watchlist)
  
  return(xgb.mit)
}