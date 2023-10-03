if(!require(xtable)) install.packages('xtable')
library(dplyr)

# helper functions
source('read_data.R')
source('models/predictor_descriptions.R')
source('models/evaluation.R')

# performance
# dimensions: metric, model (all models + baseline)


get_perf = function(results, model, sd) {
  metrics = as.data.frame(results$metrics[,1+sd])
  metrics$Model = model
  metrics
}

# year 1
get_perf_span = function(span, year, sd=F) {
  data_to_impute = get_data_to_impute(span)
  baserate = mean(data_to_impute$dropout, na.rm=T)
  
  load(paste0("models/results/year_", year, "_results.Rdata"))
  
  baseline = sapply(rownames(lr_results$metrics), function(metric) get_baseline(metric, baserate))
  if (sd) baseline = sapply(rownames(lr_results$metrics), function(x) 0)
  
  baseline$Model = "No predictors"
  
   df = rbind(baseline,
        get_perf(lr_results, "Logistic Regression", sd),
        get_perf(kNN_results, "k-Nearest Neighbors", sd),
        get_perf(nB_results, "Naive Bayes", sd),
        get_perf(nn_results, "Neural Network", sd),
        get_perf(rf_results, "Random Forest", sd),
        get_perf(svm_results, "Support Vector Machine", sd))
   colnames(df) = c("F2 score", "AUPRC", "Accuracy", "AUROC", "Model")
   df
}

year1_perf = get_perf_span(3, 1, F)
year1_sd = get_perf_span(3, 1, T)

year1_perf[,1:4] <- paste0(
  format(round(as.matrix(year1_perf[,1:4]), 3), nsmall = 3, scientific = FALSE), 
  " (", 
  format(round(as.matrix(year1_sd[,1:4]), 4), nsmall = 4, scientific = FALSE), 
  ")"
)


year2_perf = get_perf_span(6, 2, F)
year2_sd = get_perf_span(6, 2, T)
year2_perf[,1:4] <- paste0(
  format(round(as.matrix(year2_perf[,1:4]), 3), nsmall = 3, scientific = FALSE), 
  " (", 
  format(round(as.matrix(year2_sd[,1:4]), 4), nsmall = 4, scientific = FALSE), 
  ")"
)


xtab = xtable(cbind(year1_perf[,c(5, 2,4,3)], year2_perf[,c(2,4,3)]),
              caption = "Metrics for 1-year-observation span",
              align = "rrllllll",
              digits = 3)
hline <- c(-1,0,nrow(xtab))
htype <- c("\\toprule ", "\\midrule ", "\\bottomrule ")

print(xtab,add.to.row = list(pos = as.list(hline),
                             command = htype),
      hline.after = NULL,
      include.rownames = F)

# PFI
year = 1
load(paste0("models/results/year_", year, "_results.Rdata"))

get_diffs = function(results) {
  baseline = results$metrics['AUPRC','mean'][[1]]
  results$pfi$diff = (baseline - results$pfi$score[,1]) / baseline
  results$pfi[, c('predictor', 'diff')]
}

recursive_merge <- function(df1, df2) {
  merged_df <- merge(df1, df2, by = "predictor")  # Replace "common_column" with the actual column name
  return(merged_df)
}

print_pfi = function(diff_list) {
  # Apply recursive merge using Reduce function
  merged_df <- Reduce(recursive_merge, diff_list)
  
  merged_df$mean_diff = rowMeans(merged_df[,2:5])
  names(merged_df) = c("Predictor", "nn", "RF", "svm", "lr", "Top 4")
  merged_df$Predictor <- translate_predictor_names(merged_df$Predictor)
  
  RQ2_year_1 = merged_df[order(-merged_df[,3]),]
  rownames(RQ2_year_1) = 1:nrow(RQ2_year_1)
  
  RQ2_year_1 <- RQ2_year_1 %>%
    mutate_at(vars(2:6), ~ paste0(round(. * 100, 1), "%"))
  # Generate LaTeX table

  xtab = xtable(RQ2_year_1[1:15,c(1,3,6)],
                caption = "Ranking of features in terms of their PFI",
                align = "rlll",
                digits = 4)
  hline <- c(-1,0,nrow(xtab))
  htype <- c("\\toprule ", "\\midrule ", "\\bottomrule ")
  print(xtab,
        add.to.row = list(pos = as.list(hline), command = htype),
        hline.after = NULL,
        include.rownames = T)
}

diff_list = lapply(list(nn_results, rf_results, svm_results, lr_results), get_diffs)
print_pfi(diff_list)


