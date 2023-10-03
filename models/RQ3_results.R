library(gridExtra)
library(scales)
setwd("~/EWS")
source("read_data.R")
source('models/predictor_descriptions.R')
source('models/evaluation.R')


my_theme = theme(
  strip.text = element_text(size = 10),
  title = element_text(size = 10))

annotate_results = function(results) {
  unrestricted = results$metrics["AUPRC",]$mean
  results$pfi$delta = (unrestricted - results$pfi$score[,'mean']) / unrestricted
  results$metrics = as.data.frame(results$metrics)
  results$metrics$group = results$pfi[1,"group"]
  results$metrics$baseline = sapply(rownames(results$metrics), function(metric) get_baseline(metric, results$desc$dropout))
  results$metrics$metric = rownames(results$metrics)
  results
}

plot_deltas = function(first, second, SPAN) {
  first = annotate_results(first)
  second = annotate_results(second)
  
  perf_long = rbind(first$metrics, second$metrics) %>% select(-sd) %>% 
    gather("model", "value", -metric, -group)
  perf_long$value = unlist( perf_long$value)
  
  print(ggplot(perf_long, aes(x=metric, y=value,  fill=group)) + 
    geom_bar(stat="identity", position="dodge") + 
    scale_y_continuous(limits = c(0, 1)) +
    facet_wrap(~ model) +
    labs(title=paste0("Metrics for ", first$pfi[1,"group"], " (n=", first$desc$pop_size, ") vs ", second$pfi[1,"group"], " (n=", second$desc$pop_size, ") after ", SPAN,  " terms")))
  
 deltas <- rbind(first$pfi, second$pfi) %>%
   group_by(group) %>%
   select("predictor", "group", "delta") %>% 
   pivot_wider(names_from = group, values_from = delta) %>% 
   gather(group, delta, -predictor)
 
 ggplot(deltas, aes(x = predictor, y = delta, fill = group)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Predictor", y = "Feature Weight", fill = "Group",
        title=paste0("PFI for ", first$pfi[1,"group"], " vs ", second$pfi[1,"group"], " after ", SPAN,  " terms")) +
   theme_minimal() +
   scale_x_discrete(labels = translate_predictor_names) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

SPAN = 2
SUFFIX = "_SPAN_2"
SPAN = 3
SUFFIX = "_SPAN_3"
SPAN = 4
SUFFIX = "_SPAN_4"

data_to_impute = get_data_to_impute(SPAN)

#data_to_impute %>% group_by(e) %>% summarise(nr = n(), dropout=mean(dropout))%>%
#  mutate(relative = nr / sum(nr))


load(paste0("models/results/stem_results", SUFFIX, ".Rdata"))
stem_desc = data_to_impute %>% filter(!is.na(any_major_stem)) %>%
  group_by(any_major_stem) %>% summarize(pop_size = n(),
                                         dropout = mean(dropout))
stem_rf_results$pfi$group = "stem"
stem_rf_results$desc = stem_desc[stem_desc$any_major_stem == TRUE, 2:3]
non_stem_rf_results$pfi$group = "_non_stem"
non_stem_rf_results$desc = stem_desc[stem_desc$any_major_stem == FALSE, 2:3]
#plot_deltas(stem_rf_results, non_stem_rf_results, SPAN)


load(paste0("models/results/female_results", SUFFIX, ".Rdata"))
desc_female = data_to_impute %>% filter(!is.na(female)) %>%
  group_by(female) %>% summarize(pop_size = n(),
                                 dropout = mean(dropout))
female_rf_results$pfi$group = "female"
female_rf_results$desc = desc_female[desc_female$female==TRUE,2:3]
non_female_rf_results$pfi$group = "_non_female"
non_female_rf_results$desc = desc_female[desc_female$female==FALSE,2:3]
#plot_deltas(female_rf_results, non_female_rf_results, SPAN)

load(paste0("models/results/low_income_results", SUFFIX, ".Rdata"))
desc_low_income = data_to_impute %>% filter(!is.na(low_income)) %>%
  group_by(low_income) %>% summarize(pop_size = n(),
                                 dropout = mean(dropout))
low_income_rf_results$pfi$group = "low_income"
low_income_rf_results$desc = desc_low_income[desc_low_income$low_income==TRUE,2:3]
non_low_income_rf_results$pfi$group = "_non_low_income"
non_low_income_rf_results$desc = desc_low_income[desc_low_income$low_income==FALSE,2:3]
#plot_deltas(low_income_rf_results, non_low_income_rf_results, SPAN)

data_to_impute = data_to_impute %>% mutate(urm = ethnicity_smpl == "Asian / Asian American" | ethnicity_smpl == "White non-Hispanic")
load(paste0("models/results/urm_results", SUFFIX, ".Rdata"))
urm_desc <- data_to_impute %>% filter(!is.na(urm)) %>%
  group_by(urm) %>%
  summarize(pop_size = n(), dropout = mean(dropout))
urm_rf_results$pfi$group <- "_non_urm"
urm_rf_results$desc = urm_desc[urm_desc$urm == TRUE, 2:3]
non_urm_rf_results$pfi$group <- "urm"
non_urm_rf_results$desc = urm_desc[urm_desc$urm == FALSE, 2:3]
#plot_deltas(non_urm_rf_results, urm_rf_results, SPAN)

load(paste0("models/results/first_generation_results", SUFFIX, ".Rdata"))
first_generation_desc <- data_to_impute %>% filter(!is.na(first_generation)) %>%
  group_by(first_generation) %>%
  summarize(pop_size = n(), dropout = mean(dropout))
first_generation_rf_results$pfi$group <- "first_generation"
first_generation_rf_results$desc = first_generation_desc[first_generation_desc$first_generation == TRUE, 2:3]
non_first_generation_rf_results$pfi$group <- "_non_first_generation"
non_first_generation_rf_results$desc = first_generation_desc[first_generation_desc$first_generation == FALSE, 2:3]
#plot_deltas(first_generation_rf_results, non_first_generation_rf_results, SPAN)

# compare performance between the groups
get_auroc = function(results) results$metrics[4,1][[1]]
get_size = function(results) results$desc$pop_size[1]
get_dropout = function(results) results$desc$dropout[1]
all_results = list(stem_rf_results, non_stem_rf_results,
                   female_rf_results, non_female_rf_results,
                   low_income_rf_results, non_low_income_rf_results,
                   non_urm_rf_results, urm_rf_results,
                   first_generation_rf_results, non_first_generation_rf_results)

group_perf = data.frame(group=c(
  rep(c("STEM major","Female", "Low-income family", "Under- represented minority",  
  "First generation"), each=2)
),
has_attribute=rep(c(TRUE, FALSE), 5),
AUROC=unlist(lapply(all_results, get_auroc)),
pop_size=unlist(lapply(all_results, get_size)),
dropout=unlist(lapply(all_results, get_dropout))) %>% mutate(has_attribute = factor(has_attribute, levels=c(FALSE,TRUE)))

# p1=ggplot(group_perf, aes(x=group, fill = has_attribute, y = AUROC)) + 
#   geom_bar(stat = "identity") + 
#   facet_wrap(~ group, scales = "free_x", ncol=5, labeller=label_wrap_gen(width=10)) +
#   theme_minimal() +
#   labs(
#     x = NULL,
#     y = "AUROC",
#     title = "Model performance by groups"
#   ) +
#   theme(
#     axis.ticks.x = element_blank()
#   ) +
#   my_theme


label_vector <- c(
  stem = "STEM major",
  female = "Female",
  low_income = "Low-income family",
  urm = "Under-represented minority",
  first_generation = "First generation"
)

group_perf

p0=ggplot(group_perf, aes(x=group, fill = has_attribute, y = dropout)) + 
  geom_bar(stat = "identity", position="dodge") + 
  theme_minimal() +
  labs(
    x = "",
    y = "Dropout rate",
    title = "Dropout by groups",
    fill=""
  ) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank()
  ) +
  scale_x_discrete(labels  = function(x) {
    str_wrap(x, width = 8)
  }) +
  scale_fill_manual(values=c("#4682B4", "orange"), labels=c("Yes", "No")) +
  my_theme +
  scale_y_continuous(labels = percent_format(scale = 100))

p1=ggplot(group_perf, aes(x=group, fill = has_attribute, y = AUROC)) + 
  geom_bar(stat = "identity", position="dodge") + 
  theme_minimal() +
  labs(
    x = "",
    y = "AUROC",
    title = "Model performance by groups",
    fill=""
  ) +
  theme(legend.position = "none",
    axis.ticks.x = element_blank()
  ) +
  scale_x_discrete(labels  = function(x) {
    str_wrap(x, width = 8)
  }) +
  scale_fill_manual(values=c("#4682B4", "orange"), labels=c("Yes", "No")) +
  my_theme

# p2=ggplot(group_perf, aes(x = has_attribute, y = pop_size)) + 
#   geom_bar(stat = "identity") + 
#   facet_wrap(~ group, scales = "free_x", ncol=5, labeller=label_wrap_gen(width=10)) +
#   theme_minimal() +
#   labs(
#     x = NULL,
#     y = "Population size",
#     title = "Population sizes by groups"
#   ) +
#   theme(
#     axis.ticks.x = element_blank()
#   ) +
#   my_theme

p2=ggplot(group_perf, aes(x=group, fill = has_attribute, y = pop_size)) + 
  geom_bar(stat = "identity", position="dodge") + 
  theme_minimal() +
  labs(
    x = "Grouping factor",
    y = "Population size",
    title = "Population sizes by groups",
    fill="Has attribute"
  ) +
  theme(
    legend.position='bottom',
    axis.ticks.x = element_blank()
  ) +
  scale_x_discrete(labels  = function(x) {
    str_wrap(x, width = 8)
  }) +
  scale_fill_manual(values=c("#4682B4", "orange"), labels=c("Yes", "No")) +
  my_theme
common_legend <- get_legend( p2 )

grid.arrange(arrangeGrob(p2 + theme(legend.position='none'), p0,p1, ncol = 3), common_legend, nrow=2, heights=c(10,1))

group_perf_long <- group_perf %>%
  pivot_longer(
    cols = c(dropout, AUROC, pop_size),
    names_to = "Metric",
    values_to = "Value"
  ) %>% mutate(
    Metric = factor(Metric, levels=c("pop_size", "dropout", "AUROC")),
    group = factor(group, levels=rev(sort(unique(group_perf$group))))) %>%
  mutate(
    Metric = recode(
      Metric,
      dropout = "Dropout rate",
      AUROC = "Model performance (AUROC)",
      pop_size = "Population size"
    )
  )



ggplot(group_perf_long, aes(y=group, fill = has_attribute, x = Value)) + 
  facet_wrap(~ Metric, scales="free_x", ncol=3) +
  geom_bar(stat = "identity", position="dodge") + 
  theme_minimal() +
  labs(
    x = "",
    y = "Grouping factor",
    title = "Population and performance by groups",
    fill="Has attribute"
  ) +
  theme(
    legend.position='bottom'
  ) +
  scale_y_discrete(labels  = function(x) {
    str_wrap(x, width = 8)
  }) +
  scale_fill_manual(values=c("orange","#4682B4"), labels=c("No", "Yes")) +
  my_theme

# PFI for six most important predictors

get_pfis = function(results) {
  unrestricted = results$metrics["AUROC",]$mean
  results$pfi$delta = (unrestricted - results$pfi$score[,'mean']) / unrestricted
  results$pfi %>% select(group, predictor, delta)
}

pfis = lapply(all_results, get_pfis)
all_pfis <- do.call(rbind, pfis)
all_pfis = all_pfis %>% mutate(belonging = !str_detect(group, stringr::fixed("non"))) %>% 
                    mutate(group = str_replace_all(group, stringr::fixed("_non_"), ""),
                           belonging=factor(belonging, levels=c(TRUE, FALSE)))

relevant_predictors <- all_pfis %>%
  group_by(predictor) %>%
  filter(any(delta > 0.01)) %>%
  ungroup()

pred_ranking = relevant_predictors %>% group_by(predictor) %>% 
  summarise(mean = mean(delta)) %>% arrange(-mean)

relevant_predictors = relevant_predictors %>% mutate(predictor = factor(predictor, levels=pred_ranking$predictor))


avg_delta <- relevant_predictors %>%
  group_by(predictor) %>%
  summarise(avg_delta = mean(delta, na.rm = TRUE))

custom_label = function(name) {
  
  avgpfi=round(100*avg_delta %>% filter(predictor==name) %>% select(avg_delta), 1)
  paste0(translate_predictor_names(name), " (Avg. ", avgpfi, "%)")
}

ggplot(relevant_predictors, aes(x=group, y=delta, fill=belonging)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ predictor, labeller=labeller(predictor=function(x) sapply(x,custom_label)), ncol=3, scales="free") +
  labs(title="Predictor importance by groups", fill="Has attribute", x="", y="Importance (AUROC decrease)") +
  scale_x_discrete(labels= function(x) {
    str_wrap(label_vector[x], width = 8)
  }) +
  scale_fill_manual(values=c("#4682B4", "orange"), labels=c("Yes", "No")) +
  scale_y_continuous(labels = percent_format(scale = 100), n.breaks=3) +
  theme_minimal()+
  my_theme+
  theme(legend.position=c(0.5, -0.05),
        legend.justification = "center",
        legend.direction="horizontal",
        strip.background = element_rect(fill = "#EEEEEE", color="#EEEEEE")) 
