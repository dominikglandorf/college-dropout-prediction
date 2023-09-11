setwd("~/EWS")
source("read_data.R")
source('models/predictor_descriptions.R')

get_baseline = function(metric, dropout)  {
  # F2 score best will be reached for assigning everyone as dropout (recall=1, precision=dropout)
  precision=dropout
  recall=1
  if (metric == "F2score") return(5*precision*recall/(4*precision+recall))
  if (metric == "accuracy") return(1 - dropout)
  if (metric == "AUROC") return(0.5)
  if (metric == "AUPRC") return(precision)
}

annotate_results = function(results) {
  unrestricted = results$metrics["AUPRC",]$mean
  results$pfi$delta = (unrestricted - results$pfi$score[,'mean']) / unrestricted
  results$metrics = as.data.frame(results$metrics)
  results$metrics$group = results$pfi[1,"group"]
  results$metrics$baseline = sapply(rownames(results$metrics), function(metric) get_baseline(metric, results$desc$dropout))
  results$metrics$metric = rownames(results$metrics)
  results
}

plot_deltas = function(first, second) {
  first = annotate_results(first)
  second = annotate_results(second)
  
  perf_long = rbind(first$metrics, second$metrics) %>% select(-sd) %>% 
    gather("model", "value", -metric, -group)
  perf_long$value = unlist( perf_long$value)
  
  print(ggplot(perf_long, aes(x=metric, y=value,  fill=group)) + 
    geom_bar(stat="identity", position="dodge") + 
    scale_y_continuous(limits = c(0, 1)) +
    facet_wrap(~ model) +
    labs(title=paste0("Metrics for ", first$pfi[1,"group"], " (n=", first$desc$pop_size, ") vs ", second$pfi[1,"group"], " (n=", second$desc$pop_size, ")")))
  
 deltas <- rbind(first$pfi, second$pfi) %>%
   group_by(group) %>%
   select("predictor", "group", "delta") %>% 
   pivot_wider(names_from = group, values_from = delta) %>% 
   gather(group, delta, -predictor)
 
 ggplot(deltas, aes(x = predictor, y = delta, fill = group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Predictor", y = "Feature Weight", fill = "Group",
        title=paste0("PFI for ", first$pfi[1,"group"], " vs ", second$pfi[1,"group"])) +
   theme_minimal() +
   scale_x_discrete(labels = translate_predictor_names) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
  
data_to_impute = get_data_to_impute(3)
load("models/results/female_results.Rdata")
desc_female = data_to_impute %>% filter(!is.na(female)) %>%
  group_by(female) %>% summarize(pop_size = n(),
                                 dropout = mean(dropout))
female_rf_results$pfi$group = "female"
female_rf_results$desc = desc_female[desc_female$female==TRUE,2:3]
non_female_rf_results$pfi$group = "_non_female"
non_female_rf_results$desc = desc_female[desc_female$female==FALSE,2:3]
plot_deltas(female_rf_results, non_female_rf_results)

load("models/results/low_income_results.Rdata")
desc_low_income = data_to_impute %>% filter(!is.na(low_income)) %>%
  group_by(low_income) %>% summarize(pop_size = n(),
                                 dropout = mean(dropout))
low_income_rf_results$pfi$group = "low_income"
low_income_rf_results$desc = desc_low_income[desc_low_income$low_income==TRUE,2:3]
non_low_income_rf_results$pfi$group = "_non_low_income"
non_low_income_rf_results$desc = desc_low_income[desc_low_income$low_income==FALSE,2:3]
plot_deltas(low_income_rf_results, non_low_income_rf_results)

load("models/results/stem_results.Rdata")
stem_desc = data_to_impute %>% filter(!is.na(any_major_stem)) %>%
  group_by(any_major_stem) %>% summarize(pop_size = n(),
                               dropout = mean(dropout))
stem_rf_results$pfi$group = "stem"
stem_rf_results$desc = stem_desc[stem_desc$any_major_stem == TRUE, 2:3]
non_stem_rf_results$pfi$group = "_non_stem"
non_stem_rf_results$desc = stem_desc[stem_desc$any_major_stem == FALSE, 2:3]
plot_deltas(stem_rf_results, non_stem_rf_results)

data_to_impute = data_to_impute %>% mutate(urm = ethnicity_smpl == "Asian / Asian American" | ethnicity_smpl == "White non-Hispanic")
load("models/results/urm_results.Rdata")
urm_desc <- data_to_impute %>% filter(!is.na(urm)) %>%
  group_by(urm) %>%
  summarize(pop_size = n(), dropout = mean(dropout))
urm_rf_results$pfi$group <- "_non_urm"
urm_rf_results$desc = urm_desc[urm_desc$urm == TRUE, 2:3]
non_urm_rf_results$pfi$group <- "urm"
non_urm_rf_results$desc = urm_desc[urm_desc$urm == FALSE, 2:3]
plot_deltas(non_urm_rf_results, urm_rf_results)

load("models/results/first_generation_results.Rdata")
first_generation_desc <- data_to_impute %>% filter(!is.na(first_generation)) %>%
  group_by(first_generation) %>%
  summarize(pop_size = n(), dropout = mean(dropout))
first_generation_rf_results$pfi$group <- "first_generation"
first_generation_rf_results$desc = first_generation_desc[first_generation_desc$first_generation == TRUE, 2:3]
non_first_generation_rf_results$pfi$group <- "_non_first_generation"
non_first_generation_rf_results$desc = first_generation_desc[first_generation_desc$first_generation == FALSE, 2:3]
plot_deltas(first_generation_rf_results, non_first_generation_rf_results)

