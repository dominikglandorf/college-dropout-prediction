# Load required libraries
library(ggplot2)
setwd("~/EWS")
source("read_data.R")
source('models/predictor_descriptions.R')

# Initialize a list to store the data
results_data <- list()
perf = data.frame(span = numeric(),
                  F2score = numeric(),
                  AUPRC = numeric(),
                  accuracy = numeric(),AUROC = numeric(),
                  dropout = numeric())

spans = 0:9


# Loop through span from 1 to 9
for (span in spans) {
  # Create the file name
  file_name <- paste0("models/results/max_", span, "_results.Rdata")
  
  # Load the data from the file
  loaded_data <- load(file_name)
  
  perf[span+1,] = c(span,
                    unlist(rf_results$metrics[,"mean"]),
                    mean(get_data_to_impute(span)$dropout))
  
  pfi = rf_results$pfi
  pfi$span = span
  
  # Store the data in the results_data list
  results_data[[span+1]] <- pfi
}

perf_long = perf %>%
  gather(key = "Variable", value = "Value", -c(span, dropout))

baselines = perf_long
dropout = perf_long[perf_long$Variable=="F2score",]$dropout
baselines$Value = NA
baselines[baselines$Variable=="AUROC",]$Value = 0.5
# F2 score best will be reached for assigning everyone as dropout (recall=1, precision=dropout)
precision=dropout
recall=1
baselines[baselines$Variable=="F2score",]$Value = 5*precision*recall/(4*precision+recall)
baselines[baselines$Variable=="accuracy",]$Value = 1 - precision
baselines[baselines$Variable=="AUPRC",]$Value = precision

perf_long_2 = rbind(baselines %>%
    mutate(model="Baseline"),
  perf_long %>% mutate(model="Best")) %>% select(-dropout)
  

perf_plot = ggplot(perf_long_2, aes(x=span, y=Value,  fill=model)) + 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Observed span [terms]") + 
  ylab("") +
  theme_minimal() +
  labs(title="Hypertuned random forest performance metrics", fill="") +
  scale_x_continuous(breaks = 0:9, labels = 0:9) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ Variable, labeller = labeller(Variable = c("accuracy"="Accuracy",
                                                          "F2score" = "F2 score",
                                                          "AUPRC" = "Precision-recall curve",
                                                          "AUROC" = "Receiver-operator curve")))
perf_plot
ggsave("documentation/metrics_by_span.pdf", perf_plot, scale = 0.5)


combined_df <- as.data.frame(do.call(rbind, results_data))
combined_df$mean = combined_df$score[,'mean']

combined_df <- combined_df %>%
  group_by(span) %>%
  mutate(delta = (max(mean) - mean) / max(mean),
         rank = rank(mean))

ggplot(combined_df, aes(x = span, y = delta)) +
  geom_line() +
  geom_point() +
  labs(x = "Observed span", y = "Importance", color = "Feature", title = "Feature importance vs observed span") +
  facet_wrap(~ predictor, ncol = 7, labeller=labeller(predictor = translate_predictor_names)) +
  scale_x_continuous(breaks = 0:9)

