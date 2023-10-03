# Load required libraries
library(ggplot2)
library(scales)
setwd("~/EWS")
source("read_data.R")
source('models/predictor_descriptions.R')
source('models/evaluation.R')

# Initialize a list to store the data
results_data <- list()
perf = data.frame(span = numeric(),
                  F2score = numeric(),
                  AUPRC = numeric(),
                  accuracy = numeric(),
                  AUROC = numeric(),
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

perf_long = perf %>% select(-F2score) %>%
  gather(key = "Variable", value = "Value", -c(span, dropout))

baselines = perf_long %>% rowwise() %>% mutate(Value=get_baseline(Variable, dropout))

perf_long$Value = perf_long$Value - baselines$Value

perf_long_2 = rbind(
  baselines %>% mutate(model="Baseline"), 
  perf_long %>% mutate(model="Best")
) %>% select(-dropout) %>% mutate(
  Variable=factor(Variable, levels=c("AUPRC", "AUROC", "accuracy")),
  model = factor(model, levels=c("Best", "Baseline")))

my_theme = theme(
                 strip.text = element_text(size = 10),
                 title = element_text(size = 10))
  
perf_plot = ggplot(perf_long_2, aes(x=span,y=Value, fill=model)) + 
  geom_bar(stat="identity") + 
  xlab("Observed span of terms") + 
  ylab("") +
  theme_minimal() +
  labs(title="Model performance over time since initial enrollment", fill="") +
  scale_x_continuous(breaks = 0:9, labels = 0:9) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ Variable, labeller = 
               labeller(Variable = c("accuracy"="Accuracy",
                                     "F2score" = "F2 score",
                                     "AUPRC" = "Area under PRC",
                                     "AUROC" = "Area under ROC")),
             ncol=4)+
  my_theme +
  theme(legend.position = "bottom")+
  scale_fill_manual(values=c("darkgreen", "darkgrey"))
perf_plot


combined_df <- as.data.frame(do.call(rbind, results_data))
combined_df$mean = combined_df$score[,'mean']

combined_df <- combined_df %>%
  group_by(span) %>%
  mutate(delta = (max(mean) - mean) / max(mean),
         rank = rank(mean))

filtered_df <- combined_df %>%
  group_by(predictor) %>%
  filter(max(delta) > 0.025) %>%
  ungroup()

breaks_sqrt_scale <- seq(0, 0.6, length.out = 13)

filtered_df <- filtered_df %>%
  mutate(label = translate_predictor_names(predictor)) %>%
  arrange(label) %>%
  mutate(predictor = factor(predictor, levels = unique(predictor)))

pfi_plot <- ggplot(filtered_df, aes(x = span, y = delta, colour = predictor, shape=predictor)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  my_theme +
  labs(x = "Observed span of terms", y = "Importance", color = "Predictor", title = "Predictor importance over time since initial enrollment", shape="Predictor") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(labels = percent_format(scale = 100), trans="sqrt", breaks=breaks_sqrt_scale, limits=c(0.0, max(filtered_df$delta))) +
  #coord_cartesian(ylim=c(0.00, max(filtered_df$delta))) +
  scale_color_manual(values = 1:length(unique(filtered_df$predictor)), labels = translate_predictor_names) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 21, 22, 23, 24, 25, 21, 21, 22, 23, 24, 25, 21, 22), labels = translate_predictor_names)
pfi_plot

# 
# pfi_plot = ggplot(combined_df, aes(x = span, y = delta)) +
#   geom_line() +
#   geom_point() +
#   theme_minimal() +
#   labs(x = "Observed span", y = "Importance", color = "Feature", title = "Feature importance vs observed span") +
#   facet_wrap(~ predictor, ncol = 6, labeller=labeller(predictor = translate_predictor_names)) +
#   scale_x_continuous(breaks = 0:9)
pfi_plot

ggsave("documentation/pfi_by_span.pdf", pfi_plot, scale = 1)
