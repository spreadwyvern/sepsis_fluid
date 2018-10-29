# TrainXGB <- function(x.train, x.test, y.train, y.test){
#   
#   print(paste0("train data: nrow=",nrow(x.train),", ncol=",ncol(x.train)))
#   print(paste0("test data: nrow=", nrow(x.test) ,", ncol=",ncol(x.test )))
#   
#   df.train <- xgb.DMatrix(data=as.matrix(x.train), label=as.matrix(y.train))
#   df.test <- xgb.DMatrix(data=as.matrix(x.test), label=as.matrix(y.test))
#   watchlist <- list(train = df.train, test = df.test)
#   
#   # parameters
#   if(task=='uo'){
#     config <- list(
#       name = paste0("xgb_uo"), # result will be stored in train/name
#       depth = 17,
#       eta = 0.01,
#       nround = 2000,
#       rowsample = 1,
#       colsample = 0.6,
#       ntree = 1,
#       weight_balance = TRUE,
#       early_stop = 20
#     )
#   } else if(task=='bo'){
#     config <- list(
#       name = paste0("xgb_bo"), # result will be stored in train/name
#       depth = 18,
#       min_child_weight = 75,
#       gamma = 1,
#       eta = 0.01,
#       nround = 2000,
#       rowsample = 0.8,
#       colsample = 0.8,
#       ntree = 1,
#       weight_balance = TRUE,
#       early_stop = 20
#     )
#   } else {
#     print('unassigned task')
#     stop()
#   }
#   depth  <- ifelse(is.null(config$depth),3,config$depth)
#   eta    <- ifelse(is.null(config$eta),0.1,config$eta)
#   nround <- ifelse(is.null(config$nround),200,config$nround)
#   rowsample <- ifelse(is.null(config$rowsample),0.75,config$rowsample)
#   colsample <- ifelse(is.null(config$colsample),0.75,config$colsample)
#   ntree <- ifelse(is.null(config$ntree),1,config$ntree)
#   pos_weight <- ifelse(config$weight_balance == TRUE, (sum(abs(y.train)==0)/sum(abs(y.train)>0)),1)
#   early_stop <- ifelse(is.null(config$early_stop),round(nround*0.1,0),config$early_stop)
#   model_name <- paste0(config$name, ".model")
#   
#   xgb.mit <- xgb.train(data = df.train,
#                        nthread = 6, nround = nround, 
#                        max_depth = depth,
#                        eta=eta, 
#                        objective= 'binary:logistic',#'multi:softmax', #'reg:linear',
#                        # objective='reg:linear',
#                        # num_class = 3,
#                        
#                        subsample= rowsample,  colsample_bylevel= colsample, 
#                        num_parallel_tree = ntree, 
#                        eval_metric="logloss",
#                        # eval_metric = "mlogloss",
#                        maximize = FALSE,
#                        alpha=0, lambda=1, 
#                        scale_pos_weight = pos_weight, 
#                        early_stopping_rounds = early_stop,
#                        base_score=0.5, watchlist = watchlist)
#   
#   return(xgb.mit)
# }


# calculate evaluations at certain threshold
perf_stat <- function(pred,ans,class=TRUE){
  if(class){
    ppv = MLmetrics::Precision(y_true = ans, y_pred = pred,positive="1")
    recall    = MLmetrics::Recall(y_true = ans, y_pred = pred,positive=1)
    F1        = MLmetrics::F1_Score(y_true = ans, y_pred = pred,positive=1)
    acc       = MLmetrics::Accuracy(y_pred = pred, y_true = ans)
    gini      = MLmetrics::Gini(y_pred = pred, y_true = ans)
    sen       = MLmetrics::Sensitivity(y_pred = pred, y_true = ans, positive = 1)
    spe       = MLmetrics::Specificity(y_pred = pred, y_true = ans, positive = 1)
    npv       = MLmetrics::Precision(y_true = ans, y_pred = pred,positive="0")
    return(list(ppv = ppv,recall=recall,F1=F1,acc=acc,
                gini=gini,sensitivity=sen,specificity=spe, npv=npv))
  }
}



# output the whole evaluation metric at each threshold
EvalTh <- function(output, eval.stat){
  library(MLmetrics)
  library(data.table)
 
  # set threshold test range
  min.th <- 0.1
  max.th <- 0.9
  intvl.th <- 0.1
  # th <- seq(min.th, max.th,by=intvl.th)
  
  # choose which evaluation to optimize for
  eval.stat <- eval.stat
  tdf <- data.table()
  tmp <- c()
  for(th in seq(min.th, max.th,by=intvl.th)){
    y.pred.class <- (output$pred>th)+0
    y.test <- output$actual
    if(length(unique(y.pred.class))>1){
      tmp <- perf_stat(y.pred.class, y.test)
      tmp[["th"]] <- th
      # df.tmp <- cbind(threshold = th, tmp)
      tdf <- rbind(tdf, tmp)}
    cat(paste(th, '_', tmp[[eval.stat]]+0, '\n'))
  }
  th = min.th + intvl.th*(which.max(tdf[[eval.stat]])-1)
  print(paste0("optimal threshold to ", eval.stat, ": ", th))
  return(tdf)
}

# Find the optimal threshold for selected evaluation metric
FindOptTh <- function(tdf, eval.stat){
  min.th <- 0.1
  max.th <- 0.9
  intvl.th <- 0.1
  th = min.th + intvl.th*(which.max(tdf[[eval.stat]])-1)
  print(paste0("optimal threshold to ", eval.stat, ": ", th))
  return(th)
}

# 
# PlotThreshold <- function(output, eval1, eval2){
#   library(ggplot2)
#   library(reshape2)
# 
#   # set threshold test range
#   min.th <- 0.1
#   max.th <- 0.9
#   intvl.th <- 0.1
#   tdf <- data.table()
#   for(th in seq(min.th, max.th,by=intvl.th)){
#     y.pred.class <- (output$pred>th)+0
#     y.pred.class <- as.factor(y.pred.class)
#     y.test <- output$actual
#     if(length(unique(y.pred.class))>1){
#       tmp <- perf_stat(y.pred.class, y.test)
#       df.tmp <- cbind(threshold = th, 
#                       assign(eval1, tmp[[eval1]]),
#                       assign(eval2, tmp[[eval2]]),
#                       accuracy = tmp[['acc']] 
#                       )
#       tdf <- rbind(tdf, df.tmp)
#       
#     }
#     cat(paste(th, '_', tmp[['acc']], '\n'))
#   }
# 
#   trade <- melt(tdf, id='threshold')
#   plot <- ggplot(data=trade,
#                   aes(x=threshold, y=value, xlim=1, ylim=1, colour=variable)) +
#     geom_line() +
#     expand_limits(x=c(0,1), y = c(0,1))
#   
#   return(plot)
# }

# Select the best result of the repeated n timies of each fold
SelectFoldResult <- function(all.result, n.fold = 10, rep.time = 5){
  fold.result <- data.table(fold = rep(1:n.fold, each = rep.time), all.result)
  fold.result <- fold.result %>% group_by(fold) %>% filter(auc==max(auc))
  return(fold.result)
}

# Fetch output of all folds
OutputFold <- function(foldResult, feats, task, expand, dir.model){
  feat = ifelse(expand == TRUE, "expand", "current")
  bstFold <- as.integer(foldResult[["V1"]])
  
  bstList <- list()
  for(v1 in bstFold){
    fold <- ifelse(v1 %/% 5 == 0, 1, v1 %/% 5)
    n.rep <- ifelse(v1 %% 5 == 0, 5, v1 %% 5)
    bst <- paste0("_", fold, "_", n.rep)
    bstList <- append(bstList, bst)
  }
  
  model.out <- data.table()
  for(f in bstList){
    tmp <- fread(paste0(dir.model, 'out_', feat, '_', task, '_', feats, f, '.csv'))
    model.out <- rbind(model.out, tmp)
  }
  return(model.out)
}

# Calculate the result at optimal threshold
CalOptiResult <- function(output, th){
  y.pred <- output$pred
  y.test <- output$actual
  accuracy <- MLmetrics::Accuracy(y_pred = (y.pred>th)+0,y_true = y.test)
  sensitivity <- MLmetrics::Sensitivity(y_pred = (y.pred>th)+0,y_true = y.test, positive = 1)
  specificity <- MLmetrics::Specificity(y_pred = (y.pred>th)+0,y_true = y.test, positive = 1)
  ppv <- MLmetrics::Precision(y_pred = (y.pred>th)+0,y_true = y.test, positive = 1)
  npv <- MLmetrics::Precision(y_pred = (y.pred>th)+0,y_true = y.test, positive = 0)
  # auc <- MLmetrics::AUC(y_pred = (y.pred>th)+0,y_true = y.test)
  result <- as.data.table(cbind(accuracy = accuracy, sensitivity = sensitivity, 
                                specificity = specificity, ppv = ppv, npv = npv, th = th
  ))
  return(result)
}

# Plot trade-off performance to threshold
PlotThreshold <- function(tdf, eval1, eval2, opt.th){
  library(ggplot2)
  library(reshape2)
  library(gridExtra)
  cols <- c("th", eval1, eval2)
  tdf <- tdf[, ..cols]
  trade <- melt(tdf, id='th')
  plot <- ggplot(data=trade,
                  aes(x=th, y=value, xlim=1, ylim=1, colour=variable)) +
    geom_line() +
    expand_limits(x=c(0,1), y = c(0,1)) +
    xlab("Threshold") +
    ylab("Value") +
    # coord_fixed() +
    geom_vline(xintercept = opt.th, color = "navy", linetype = 'dotted') +
    geom_text(x = opt.th, y = 0, label = paste0("threshold = ", opt.th), 
              color = "royalblue", size = 4, angle = 90, hjust = -0.02, vjust=-0.4) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_blank() 
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank())
          )
  return(plot)
}
