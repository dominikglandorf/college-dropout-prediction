library(gridExtra)

source('read_data.R')

original = get_data_to_impute()
colSums(is.na(original))

datasets = get_imputed_features()

cont_features = c("hs_gpa", "uc_total_score", "uc_math_score", "cum_avg_credits", "cum_avg_gpa", "cum_avg_rel_owngen_crs", "cum_avg_rel_owngen_crs", "cum_avg_rel_ownfirstgen_crs")

plot_density = function(name) {
  amount_NA = mean(is.na(original[[name]]))
  # Extract the column of interest from each dataset
  values <- unlist(lapply(datasets, function(dataset) dataset[[name]]))
  # Combine the column data into a single data frame
  values_dataset = data.frame(dataset=rep(1:length(datasets), each=nrow(datasets[[1]])), values=values)
  # Plot kernel density plot for the column in one plot
  plot = ggplot() +
    geom_density(data=original, aes_string(x = name), color = "black") +
    geom_density(data=values_dataset, aes(x = values, color = as.factor(dataset))) +
    labs(title=paste0(name, " (", round(amount_NA*100, 1),"% missing)"), x = "Value", y = "Density", color = "Imputation") +
    theme_minimal()
  return(plot)
}

plots = lapply(cont_features, plot_density)

grid.arrange(grobs = plots, nrow = 2)

disc_features = c("int_student", "father_edu_level_code", "mother_edu_level_code","cal_res_at_app","female", "ethnicity_smpl", "first_generation","household_size_app")

plot_discrete = function(name) {
  amount_NA = mean(is.na(original[[name]]))
  # Extract the column of interest from each dataset
  values <- unlist(lapply(datasets, function(dataset) dataset[[name]]))
  # Combine the column data into a single data frame
  values_dataset = data.frame(dataset=rep(0:length(datasets), each=nrow(datasets[[1]])), value=c(original[[name]], values))
  
  # Plot kernel density plot for the column in one plot

  plot = ggplot(values_dataset, aes(x=value, fill=as.factor(dataset))) +
    geom_bar(position = "dodge", stat = "count",show.legend=FALSE) +
    labs(title=paste0(name, " (", round(amount_NA*100, 1),"% missing)"))
    theme_minimal()
  return(plot)
}

plots = lapply(disc_features, plot_discrete)

grid.arrange(grobs = plots)

colMeans(is.na(datasets[[1]]))
