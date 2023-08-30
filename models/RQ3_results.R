setwd("~/EWS")

plot_deltas = function(first, second) {
  unrestricted = first$metrics["AUPRC",]$mean
  first$pfi$delta = (unrestricted - first$pfi$score[,'mean']) / unrestricted
  
  unrestricted = second$metrics["AUPRC",]$mean
  second$pfi$delta = (unrestricted - second$pfi$score[,'mean']) / unrestricted
  
 deltas <- rbind(first$pfi, second$pfi) %>%
   group_by(group) %>%
   select("predictor", "group", "delta") %>% 
   pivot_wider(names_from = group, values_from = delta) %>% 
   gather(group, delta, -predictor)
 
 ggplot(deltas, aes(x = predictor, y = delta, fill = group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Predictor", y = "Feature Weight", fill = "Group",
        title=paste0(first$pfi[1,"group"], " AUPRC ", round(first$metrics["AUPRC",]$mean, 2), " ", second$pfi[1,"group"], " AUPRC ", round(second$metrics["AUPRC",]$mean, 2))) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
  

load("models/results/female_results.Rdata")
female_rf_results$pfi$group = "female"
non_female_rf_results$pfi$group = "non_female"
plot_deltas(female_rf_results, non_female_rf_results)

load("models/results/low_income_results.Rdata")
low_income_rf_results$pfi$group = "low_income"
non_low_income_rf_results$pfi$group = "non_low_income"
plot_deltas(low_income_rf_results, non_low_income_rf_results)

load("models/results/stem_results.Rdata")
stem_rf_results$pfi$group = "stem"
non_stem_rf_results$pfi$group = "non_stem"
plot_deltas(stem_rf_results, non_stem_rf_results)

load("models/results/urm_results.Rdata")
urm_rf_results$pfi$group = "urm"
non_urm_rf_results$pfi$group = "non_urm"
plot_deltas(urm_rf_results, non_urm_rf_results)

load("models/results/first_generation_results.Rdata")
first_generation_rf_results$pfi$group = "first_generation"
non_first_generation_rf_results$pfi$group = "non_first_generation"
plot_deltas(first_generation_rf_results, non_first_generation_rf_results)
