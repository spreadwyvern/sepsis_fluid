library(data.table)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
rm(list = ls())

source("utils.R")


dir.res.mit <- "/home/lwk/sepsis_fluid/result/"
dir.res.fold <- "/home/lwk/sepsis_fluid/result/10-fold/"

dir.bo.all <- paste0(dir.res.mit, "2018-10-01_all/")

# task bo
res.bo.all <- fread(paste0(dir.bo.all, "result_expand_bo_all.csv"))

# select the best fold result
res.cv.bo.all <- SelectFoldResult(res.bo.all)

# average CV result
res.cv.bo.avf <- colMeans(apply(res.cv.bo.all[, -c(1:4)], 2, as.numeric))
View(res.cv.bo.avf)
# grab output of the folds
out.bo.all <- OutputFold(res.cv.bo.all, "all", "bo", TRUE, dir.bo.all)

# optimize threshold to evaluation metrics
metric.bo.all <- EvalTh(out.bo.all, "acc")
th.bo.all <- FindOptTh(metric.bo.all, "acc")

# calculate result at optimal thershold
res.bo.all.opt <- CalOptiResult(out.bo.all, th.bo.all)
View(res.bo.all.opt)

# plot threshold-performace trade-off
plt.trade1 <- PlotThreshold(metric.bo.all, "sensitivity", "specificity", th.bo.all)
plt.trade2 <- PlotThreshold(metric.bo.all, "npv", "ppv", th.bo.all)
figure <- egg::ggarrange(plt.trade1, plt.trade2, widths = c(5, 5), heights = c(1), nrow = 1)
# ggsave(paste0(dir.res.fold, "trade_off_all.png"), figure, width = 24, height = 9, units = "cm", dpi = 600)
