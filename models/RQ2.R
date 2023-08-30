if(!require(xtable)) install.packages('xtable')
library(dplyr)

# helper functions
source('models/predictor_descriptions.R')

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
  names(merged_df) = c("Predictor", "nn", "Random Forest", "svm", "lr", "Mean in Top 4")
  merged_df$Predictor <- translate_predictor_names(merged_df$Predictor)
  
  RQ2_year_1 = merged_df[order(-merged_df[,3]),]
  rownames(RQ2_year_1) = 1:nrow(RQ2_year_1)
  
  RQ2_year_1 <- RQ2_year_1 %>%
    mutate_at(vars(2:6), ~ paste0(round(. * 100, 1), "%"))
  # Generate LaTeX table
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  print(xtable(RQ2_year_1[1:15,c(1,3,6)],
               caption = "Ranking of features in terms of their PFI",
               align = "|r|l|l|l|",
               digits = 4),
        sanitize.colnames.function=bold,
        include.rownames = T)
}

# year 1
load("models/results/year_1_results.Rdata")
diff_list = lapply(list(nn_results, rf_results, svm_results, lr_results), get_diffs)
print_pfi(diff_list)

# year 2 a bit hacky currently (needs better result saving in the next modeling iteration)
year_2_lr_pfi <- read.csv("~/EWS/models/results/year_2_lr_pfi.txt", sep="")
year_2_nn_pfi <- read.csv("~/EWS/models/results/year_2_nn_pfi.txt", sep="")
year_2_rf_pfi <- read.csv("~/EWS/models/results/year_2_rf_pfi.txt", sep="")
year_2_svm_pfi <- read.csv("~/EWS/models/results/year_2_svm_pfi.txt", sep="")

# LR
baseline = 0.7736455 
year_2_lr_pfi$diff = (baseline - year_2_lr_pfi$score.mean) / baseline

# NN
baseline = 0.7823003 
year_2_nn_pfi$diff = (baseline - year_2_nn_pfi$score.mean) / baseline

# RF
baseline = 0.7912229 
year_2_rf_pfi$diff = (baseline - year_2_rf_pfi$score.mean) / baseline

# SVM
baseline = 0.778 
year_2_svm_pfi$diff = (baseline - year_2_svm_pfi$score.mean) / baseline

diff_list = list(year_2_lr_pfi[, c('predictor', 'diff')],
                 year_2_nn_pfi[, c('predictor', 'diff')],
                 year_2_rf_pfi[, c('predictor', 'diff')],
                 year_2_svm_pfi[, c('predictor', 'diff')])

print_pfi(diff_list)



