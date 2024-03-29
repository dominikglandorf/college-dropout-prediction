if(!require(MLmetrics)) install.packages('MLmetrics')
if(!require(pROC)) install.packages('pROC') # for RO curve
if(!require(PRROC)) install.packages('PRROC') # for PR curve


thresholds = seq(0, 1, 0.005)

accuracy_by_threshold = function(predicted_scores, true_labels) {
  accuracies = sapply(thresholds, function(t) Accuracy(predicted_scores>t, true_labels))
  acc_data = data.frame(threshold=thresholds, metric=accuracies)
  best_thres = thresholds[which.max(accuracies)]
  return(list(accuracies = acc_data,
              best_threshold = best_thres,
              best_metric = max(accuracies)))
}

Fbetascore_by_threshold = function(predicted_scores, true_labels, beta=2) {
  Fscores = sapply(thresholds, function(t) FBeta_Score(true_labels, predicted_scores>=t, positive=TRUE, beta=beta))
  F2_data = data.frame(threshold=thresholds, metric=Fscores)
  best_thres = thresholds[which.max(Fscores)]
  return(list(Fscores = F2_data,
              best_threshold = best_thres,
              best_metric = max(Fscores, na.rm=T)))
}

plot_metric_by_threshold = function(data, metric_label) {
  data = drop_na(data)
  best_thres = data$threshold[which.max(data$metric)]
  print(ggplot(data, aes(x=threshold, y=metric)) +
          labs(title=paste0("Best ", metric_label, "=", round(max(data$metric, na.rm=T),3), " at threshold=", round(best_thres, 2)),
               x="Threshold",
               y=metric_label) +
          geom_point() +
          geom_vline(xintercept = best_thres, linetype = "dashed", color = "red"))
}

plot_ROC = function(predicted_scores, true_labels) {
  rocobj <- roc(true_labels, predicted_scores)
  auc <- round(auc(true_labels, predicted_scores),4)
  print(ggroc(rocobj) +
    ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) + 
    geom_abline(intercept = 1, slope = 1, color = "red", linetype = "dashed"))
}


plot_PR = function(predicted_scores, true_labels) {
  pr_curve <- pr.curve(scores.class0 = predicted_scores, weights.class0 = as.numeric(true_labels), curve=T)
  print(ggplot(as.data.frame(pr_curve$curve), aes(x=V1, y=V2)) +
    geom_line() +
    ylim(0, 1) +
    geom_hline(yintercept=mean(true_labels)) +
    labs(title=paste0("PR-Curve (AUC: ", round(pr_curve$auc.davis.goadrich, 2), ")"),
         x="Recall",
         y="Precision"))
}


plot_multiple_ROC = function(scores_by_method, true_labels) {
  my_palette <- palette("Set1")
  rocs = lapply(scores_by_method, function(predicted_scores) roc(true_labels, predicted_scores))
  aucs = lapply(scores_by_method, function(predicted_scores) AUC(predicted_scores, true_labels))
  labels = paste0(names(scores_by_method), " (AUC: ", round(unlist(aucs), 2), ")")
  ggroc(rocs, aes = c("color")) +
    scale_color_manual(labels=labels, values =my_palette[1:length(scores_by_method)]) +
    labs(title="ROC by method", color="Method", x="Specificity", y="Sensitivity") +
    geom_abline(intercept = 1, slope=1, color = "black", linetype = "dashed") +
    theme_minimal()
}

plot_multiple_PRC = function(scores_by_method, true_labels) {
  my_palette <- palette("Set1")
  prcs = lapply(scores_by_method, function(predicted_scores) as.data.frame(pr.curve(scores.class0 = predicted_scores, weights.class0 = as.numeric(true_labels), curve=T)$curve))

  pr_curves <- do.call(rbind, prcs)
  pr_curves$method <- rep(names(prcs), times = sapply(prcs, nrow))

  aucs = lapply(scores_by_method, function(predicted_scores) PRAUC(predicted_scores, true_labels))
  
  labels = paste0(names(scores_by_method), " (AUC: ", round(unlist(aucs), 2), ")")
  ggplot(pr_curves, aes(x=V1, y=V2, color=method)) +
    geom_line() +
    ylim(0, 1) +
    scale_color_manual(labels=labels, values =my_palette[1:length(scores_by_method)]) +
    geom_hline(yintercept=mean(true_labels)) +
    labs(title=paste0("PR-Curve by method"),
         x="Recall",
         y="Precision",
         color="Method")+
    theme_minimal()
}

get_auprc = function(predicted_scores, true_labels) {
  pr_curve <- pr.curve(scores.class0 = predicted_scores, weights.class0 = as.numeric(true_labels), curve=T)
  auprc = as.numeric(pr_curve$auc.davis.goadrich)
}

get_all_metrics = function(predicted_scores, true_labels) {
  acc = accuracy_by_threshold(predicted_scores, true_labels)
  Fsco = Fbetascore_by_threshold(predicted_scores, true_labels)
  auroc = AUC(predicted_scores, true_labels)
  auprc = get_auprc(predicted_scores, true_labels)
  
  return(list(F2score=as.numeric(Fsco$best_metric),
              AUPRC=as.numeric(auprc),
              accuracy=as.numeric(acc$best_metric),
              AUROC=as.numeric(auroc)))
}

get_baseline = function(metric, dropout)  {
  # F2 score best will be reached for assigning everyone as dropout (recall=1, precision=dropout)
  precision=dropout
  recall=1
  if (metric == "F2score") return(5*precision*recall/(4*precision+recall))
  if (metric == "accuracy") return(1 - dropout)
  if (metric == "AUROC") return(0.5)
  if (metric == "AUPRC") return(precision)
}


feature_importance = function(model_predict, test) {
  auroc_scores = c()
  auprc_scores = c()
  true_labels = as.logical(test$dropout)
  for (feature in names(test %>% select(-dropout))) {
    permuted = test
    permuted[[feature]] = sample(permuted[[feature]])
    
    predicted_scores = model_predict(permuted)
    
    #F2 = Fbetascore_by_threshold(predicted_scores, true_labels)
    #prauc = PRAUC(predicted_scores, true_labels)
    auroc_scores = c(auroc_scores, AUC(predicted_scores, true_labels))
    #auprc_scores = c(auprc_scores, get_auprc(predicted_scores, true_labels))#F2$best_metric)
  }
  pfi = data.frame(predictor=names(test %>% select(-dropout)), score=auroc_scores)
  return(pfi[order(pfi$score),])
}


