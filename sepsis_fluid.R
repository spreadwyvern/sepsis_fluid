#!/usr/bin/env Rscript
library(data.table)
library(xgboost)
library(caret)
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# source model and utils
source("utils.R")
source("model.R")

# Controls
# select "current" or "expand" features set
feat.set <-  "expand" 

# select task: 
# predict increase of urine output: "uo"
# predict exceeding basic uo: "bo"
task <- "bo"

# selecting sub-groups with UO already exceeding basic urine output or not
# "None", True, False
exceed.BO <- TRUE

# select top-N features if presented importance from XGBoost
# all: "all" 
# can loop over list to select top-N features in decend order
feature.list <- c("all")#, "20", "15", "10", "5") # filter features by importance
# or load previous importance list
# importance <- fread("./result/importance/importance_exoand_bo_all.csv")

# set threshold for classification
th <- 0.5 

# K-fold cross-validation
n.folds <- 5

# repeated times of training in each fold
rep.times <- 1


# data import and filter
df <- fread('/home/mmnas2/cmchang/med/mit/Sepsis_MIT.csv')

# shift input_4hourly and output_4hourly up one row as these are the targets that
# we want to predict in the next 4 hour
df <- setDT(df)[,input_4hr_pred:=shift(input_4hourly_tev,1,type="lead"), by = 'icustayid']
df <- setDT(df)[,output_4hr_pred:=shift(output_4hourly,1,type="lead"), by = 'icustayid']

# add if urine output increase
df <- setDT(df)[,uo.increase:=shift(output_4hourly,1,type="lead")-output_4hourly, by = 'icustayid']

# add basic urine output
df <- setDT(df)[,basic_output:= 0.5*Weight_kg*4, by = 'icustayid']

# create the "expanded" features by adding previous 4-hour record
# list of features to add from previous record
expand.list <- c("GCS", "HR", "SysBP", "DiaBP", "RR", "SpO2", "Temp_C", "FiO2_1", 
                "BUN", "Creatinine", "CO2_mEqL", "Hb", "WBC_count", "INR", 
                "Arterial_pH", "paO2", "paCO2", "Arterial_lactate",
                "HCO3", "Total_bili", "Platelets_count")

new.cols <- paste("p", expand.list, sep="_")
df[, (new.cols ) := shift(.SD, 1, NA, type = "lag"),  .SDcols=expand.list, by = "icustayid"]

# add fluid ballance related features
df <- setDT(df)[,input_4hourly_shift:=input_4hourly_tev-shift(input_4hourly_tev,1,type="lag"), by = 'icustayid']
df <- setDT(df)[,ouput_4hourly_shift:=output_4hourly-shift(output_4hourly,1,type="lag"), by = 'icustayid']
df <- setDT(df)[,cumulated_shift:=cumulated_balance_tev-shift(cumulated_balance_tev,1,type="lag"), by = 'icustayid']

# create two task targets
df[, target.bo := ifelse(output_4hr_pred <= basic_output, 0, 1)]
df[, target.uo := ifelse(uo.increase <= 0, 0, 1)]
# remove na in target.bo
df <- na.omit(df,"target.bo")
df <- df[!(is.infinite(df$target.bo))]
print(sum(is.infinite(df$target.bo)))
print(head(df$target.bo))
# write.csv(df, file = "sepsis_final.csv")

# remove patients with a urine output alrady exceeding/below basic urine output
if(exceed.BO == TRUE){
  df <- df[df$output_4hourly > df$basic_output]
} else if (exceed.BO == FALSE){
  df <- df[df$output_4hourly < df$basic_output]
}

# use original or expanded features
if(feat.set == "current"){
  df <- df[, c('gender', 'age', 'Weight_kg', 'GCS', 'HR', 'SysBP', 'DiaBP', 'RR', 
                      'SpO2', 'Temp_C', 'FiO2_1', 'BUN', 'Creatinine', 'CO2_mEqL', 
                      'Hb', 'WBC_count', 'INR', 'Arterial_pH', 'paO2', 'paCO2', 
                      'Arterial_lactate', 'HCO3', 'input_4hr_pred', 'target.bo', 
                      'target.uo', 'cumulated_balance_tev', 'output_4hourly', 
                      'Total_bili', 'Platelets_count')]
} else if(feat.set == 'expand'){
  df <- df[, c('gender', 'age', 'Weight_kg', 'GCS', 'HR', 'SysBP', 'DiaBP', 'RR', 
                      'SpO2', 'Temp_C', 'FiO2_1', 'BUN', 'Creatinine', 'CO2_mEqL', 
                      'Hb', 'WBC_count', 'INR', 'Arterial_pH', 'paO2', 'paCO2', 
                      'Arterial_lactate', 'HCO3', 'p_GCS', 'p_HR', 'p_SysBP', 
                      'p_DiaBP', 'p_RR', 'p_SpO2', 'p_Temp_C', 'p_FiO2_1', 
                      'p_BUN', 'p_Creatinine', 'p_CO2_mEqL', 'p_Hb', 'p_WBC_count', 
                      'p_INR', 'p_Arterial_pH', 'p_paO2', 'p_paCO2', 'p_Arterial_lactate', 
                      'p_HCO3', 'input_4hr_pred', 'target.bo', 'target.uo',
                      'cumulated_balance_tev', 'output_4hourly', "input_4hourly_shift", 
                      "cumulated_shift", "ouput_4hourly_shift", 'p_Total_bili', 
                      'p_Platelets_count', 'Total_bili', 'Platelets_count')]
  
} else {
  print('undefined features set!')
  stop()
}



for(features in feature.list){

  if(features != "all"){
    df <- df[,c(importance$Feature[1:features], 'target.bo', 'target.uo'), with=FALSE]
  }
  
  # split x and Y
  x <- as.data.frame(df)
  x$target.uo <- NULL
  x$target.bo <- NULL
  x$target <- NULL
  x <- apply(x, 2, as.numeric)
  
  # select task
  if(task == "uo"){
    y <- df[,target.uo]
  } else if(task == "bo"){
    y <- df[,target.bo]
  }
  
  
  # K-fold cross-validation
  #Create k equally size folds
  df <- df[sample(nrow(df)),]
  folds <- cut(seq(1,nrow(df)), breaks=n.folds, labels=FALSE)
  
  for(f in 1:n.folds){
    i.test <- which(folds==f, arr.ind=TRUE)
    i.train <-  setdiff(seq(1,nrow(df)),i.test)
  
    # repeated runs of each fold
    for(i in 1:rep.times){
      x.train <- x[i.train, ]
      x.test <- x[i.test, ]
      y.train <- y[i.train]
      y.test <- y[i.test]
      
      # train XGBoost model
      xgb.mit <- TrainXGB(x.train, x.test, y.train, y.test)
          
      y.pred <- predict(xgb.mit, as.matrix(x.test))
      output <- data.frame(actual=y.test,pred=y.pred)
      
      # Create the confusion matrix
      # pred.resp <- ifelse(y.pred >= th, 1, 0)
      # confusionMatrix(pred.resp, y.test, positive="1")
      
      auc <-  MLmetrics::AUC(y_pred = y.pred,y_true = y.test)
      recall <- MLmetrics::Recall(y_pred = (y.pred>th)+0,y_true = y.test, positive = '1')
      precision <- MLmetrics::Precision(y_pred = (y.pred>th)+0,y_true = y.test, positive = '1')
      f1 <- MLmetrics::F1_Score(y_pred = (y.pred>th)+0,y_true = y.test, positive = '1')
      accuracy <- MLmetrics::Accuracy(y_pred = (y.pred>th)+0,y_true = y.test)
      sensitivity <- MLmetrics::Sensitivity(y_pred = (y.pred>th)+0,y_true = y.test, positive = '1')
      specificity <- MLmetrics::Specificity(y_pred = (y.pred>th)+0,y_true = y.test, positive = '1')
      
      # importance
      if(i == 1){
        importance <- xgb.importance(feature_names = colnames(x.train), model = xgb.mit)
      } 
      # print(importance)
      
      # save files
      dir.result.mit <- paste0("./result/", Sys.Date(), "/")
      if (!file.exists(dir.result.mit)){
        dir.create(dir.result.mit, recursive = TRUE)
        }
      xgb.save(xgb.mit, paste0(dir.result.mit, "model_", feat.set, "_", task, "_", features, "_", i)) 
      write.csv(output, paste0(dir.result.mit, "out_", feat.set, "_", task, "_", features, "_", f, "_", i, ".csv"))
      write.csv(importance, paste0(dir.result.mit, 'importance_', feat.set, "_", task, "_", features, '.csv'))
      # saveRDS(importance, paste0(dir_result_mit, 'importance_', feat.set, "_", task, "_", features, '.rds'))
      result.tmp.mit <- as.data.table(cbind(features = features, sys.time = as.character(Sys.time()), recall, precision, f1, accuracy, sensitivity, specificity, auc))
      
      
      if(!file.exists(paste0(dir.result.mit, "result_", feat.set, "_", task, "_", features, ".csv"))){
        
        write.csv(result.tmp.mit, paste0(dir.result.mit, "result_", feat.set, "_", task, "_", features, ".csv"))
        
        } else {
      result.mit<- fread(paste0(dir.result.mit, "result_", feat.set, "_", task, "_", features, ".csv"), drop = 1)
      result.mit <- rbind(result.mit, result.tmp.mit)
      write.csv(result.mit, paste0(dir.result.mit, "result_", feat.set, "_", task, "_", features, ".csv"))
        }
      
      # for loop end for runs
      }
    write.csv(df[i.test,], paste0(dir.result.mit, "test_fold_", f, ".csv"))
    write.csv(df[i.train,], paste0(dir.result.mit, "test_fold_", f, ".csv"))
  }
}
