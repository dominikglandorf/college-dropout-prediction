# Load required libraries
library(ggplot2)

# Initialize a list to store the data
results_data <- list()
perf = c()

# Loop through span from 1 to 9
for (span in 1:9) {
  # Create the file name
  file_name <- paste0("models/results/max_", span, "_results.Rdata")
  
  # Load the data from the file
  loaded_data <- load(file_name)
  
  perf = c(perf, rf_results$metrics['F2score','mean'][[1]])
  
  pfi = rf_results$pfi
  pfi$span = span
  
  # Store the data in the results_data list
  results_data[[span]] <- pfi
}

ggplot(data.frame(span=1:9, AUPRC=perf)) + 
  geom_bar(aes(x=span, y=AUPRC), stat="identity") + 
  scale_x_continuous(breaks = 1:9, labels = 1:9) +
  scale_y_continuous(limits = c(0, 1))

combined_df <- as.data.frame(do.call(rbind, results_data))
combined_df$mean = combined_df$score[,'mean']

combined_df <- combined_df %>%
  group_by(span) %>%
  mutate(delta = (max(mean) - mean) / max(mean),
         rank = rank(mean))

filtered_data <- combined_df %>%
  group_by(predictor) %>%
  filter(any(rank <= 7))

ggplot(combined_df, aes(x = span, y = delta)) +
  geom_line() +
  geom_point() +
#  scale_y_reverse() +
  labs(x = "Observed span", y = "Importance", color = "Feature", title = "Feature importance vs observed span") +
  facet_wrap(~ predictor, ncol = 7) +
  scale_x_continuous(breaks = 1:9)

