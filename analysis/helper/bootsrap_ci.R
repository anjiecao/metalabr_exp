bootstrap_function_stratified <- function(data, indices) {
  
  data_resampled <- data.frame()
  
  # For each unique group, sample with replacement and bind to the resampled data
  for (grp in unique(data$short_cite)) {
    group_data <- data[data$short_cite == grp, ]
    group_sample <- group_data[sample(nrow(group_data), nrow(group_data), replace = TRUE), ]
    data_resampled <- rbind(data_resampled, group_sample)
  }
  
  # Now fit the mixed model on the stratified resampled data
  model_resampled <- lmer(d_calc ~ mean_age_months + (mean_age_months | short_cite), 
                          control = lme4::lmerControl(optimizer="bobyqa"), data = data_resampled)
  
  fixed_slope <- fixef(model_resampled)["mean_age_months"]
  random_slopes <- ranef(model_resampled)$short_cite[, "mean_age_months"]
  
  group_slopes <- fixed_slope + random_slopes
  
  return(group_slopes)
}



get_ci_for_random_slope <- function(ds_name, data, run = 5000){
  
  #Fit the mixed-effects model
  model <- lmerTest::lmer(d_calc ~ mean_age_months + (mean_age_months | short_cite), data = data, 
                          control = lme4::lmerControl(optimizer="bobyqa"))
  
  results <- boot(data = data, statistic = bootstrap_function_stratified, R = run)
  ci_lower <- apply(results$t, 2, function(x) quantile(x, 0.025))
  ci_upper <- apply(results$t, 2, function(x) quantile(x, 0.975))
  
  df = tibble(
    ds_name = ds_name,
    group_id = seq(1, length(ci_lower)), 
    ci_lower = ci_lower, 
    ci_upper = ci_upper
  )
  
  return (df)
  
}