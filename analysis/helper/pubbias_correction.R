
# extract the data from the likelihood ratio test
get_lrt <- function(model){
  df <- length(model[[2]]$par) - length(model[[1]]$par)
  lrchisq <- 2*(abs(model[[1]]$value - model[[2]]$value))
  pvalue <- 1-pchisq(lrchisq,df)
  
  return (
    tibble(
      "lrchisq" = lrchisq, 
      "df" = df, 
      "pvalue" = pvalue
    )
  )
}

# extrat the esitmates, both adjusted and unadjusted 
get_estimates <- function(model){
  
  adjusted_estimates = tibble(
    "moderator" = c("intercept", "mean_age_months"),
    "estimate" = c(model$adj_est[[2]], model$adj_est[[3]]), 
    "ci.lb" = c(model$ci.lb_adj[[2]], model$ci.lb_adj[[3]]), 
    "ci.ub" = c(model$ci.ub_adj[[2]], model$ci.ub_adj[[3]]),
    "se" = c(model$adj_se[[2]], model$adj_se[[3]]), 
    "z" = c(model$z_adj[[2]], model$z_adj[[3]]), 
    "p_val" = c(model$p_adj[[2]], model$p_adj[[3]])
  ) %>% mutate(type = "adjusted")
  
  
  unadjusted_estimates = tibble(
    "moderator" = c("intercept", "mean_age_months"),
    "estimate" = c(model$unadj_est[[2]], model$unadj_est[[3]]), 
    "ci.lb" = c(model$ci.lb_unadj[[2]], model$ci.lb_unadj[[3]]), 
    "ci.ub" = c(model$ci.ub_unadj[[2]], model$ci.ub_unadj[[3]]),
    "se" = c(model$unadj_se[[2]], model$unadj_se[[3]]), 
    "z" = c(model$z_unadj[[2]], model$z_unadj[[3]]), 
    "p_val" = c(model$p_unadj[[2]], model$p_unadj[[3]])
  ) %>% mutate(type = "unadjusted")
  
  return (bind_rows(
    adjusted_estimates, unadjusted_estimates
  ))

}


get_pub_bias_lrt <- function(full_d, ds_name, split_option = "median"){
  
  d <- full_d %>% filter(ds_clean == ds_name)
  
  if (split_option == "median"){
    median_age <- median(d$mean_age_months)
    younger_ds <- d %>% filter(mean_age_months < median_age)
    older_ds <- d %>% filter(mean_age_months >= median_age)
  }else{
    younger_ds <- d %>% filter(mean_age_months < 12)
    older_ds <- d %>% filter(mean_age_months >= 12)
  }
  
  
  # check how many studies are in each half 
  n_younger <- nrow(younger_ds)
  n_older <- nrow(older_ds)
  
  if (n_younger == 0 | n_older == 0){
    return (tibble("ds_clean" = name, 
                   "warning" = "no enough data"))
  }else{
    
    # run three models 
    # first half 
    younger_res <- weightr::weightfunct(effect = younger_ds$d_calc, 
                                        v = younger_ds$d_var_calc, 
                                        steps = c(0.05, 1), 
                                        mods = ~ younger_ds$mean_age_months 
    )
    
    
    # second half 
    older_res <- weightr::weightfunct(effect = older_ds$d_calc, 
                                      v = older_ds$d_var_calc, 
                                      steps = c(0.05, 1), 
                                      mods = ~ older_ds$mean_age_months 
    )
    
    # together 
    full_res <- weightr::weightfunct(effect = d$d_calc, 
                                     v = d$d_var_calc, 
                                     steps = c(0.05, 0.5, 1), 
                                     mods = ~ d$mean_age_months 
    )
    
    younger_p_val <- get_lrt(younger_res)
    older_p_val <- get_lrt(older_res)
    full_p_val <- get_lrt(full_res)
    
    
    lrt_summary <- bind_rows(
      younger_p_val %>% mutate(test_data = "younger"), 
      older_p_val %>% mutate(test_data = "older"), 
      full_p_val %>% mutate(test_data = "full")
    ) %>% 
      mutate(ds_clean = ds_name)
    
    return(lrt_summary)
    
  }
  
  
}



get_pub_bias_estimates <- function(full_d, ds_name,  split_option = "median"){
  
  d <- full_d %>% filter(ds_clean == ds_name)
  print(ds_name)
  
  if (split_option == "median"){
    median_age <- median(d$mean_age_months)
    younger_ds <- d %>% filter(mean_age_months < median_age)
    older_ds <- d %>% filter(mean_age_months >= median_age)
  }else{
    younger_ds <- d %>% filter(mean_age_months < 12)
    older_ds <- d %>% filter(mean_age_months >= 12)
  }
  
  n_younger <- nrow(younger_ds)
  n_older <- nrow(older_ds)
  if (n_younger == 0 | n_older == 0){
    return (tibble("ds_clean" = name, 
                   "warning" = "no enough data"))
  }else{
  # run three models 
  # first half 
  younger_res <- weightr::weightfunct(effect = younger_ds$d_calc, 
                                      v = younger_ds$d_var_calc, 
                                      steps = c(0.05, 1), 
                                      mods = ~ younger_ds$mean_age_months 
  )
  
  # second half 
  older_res <- weightr::weightfunct(effect = older_ds$d_calc, 
                                    v = older_ds$d_var_calc, 
                                    steps = c(0.05, 1), 
                                    mods = ~ older_ds$mean_age_months 
  )
  
  
  # together 
  full_res <- weightr::weightfunct(effect = d$d_calc, 
                                   v = d$d_var_calc, 
                                   steps = c(0.05, 0.5, 1), 
                                   mods = ~ d$mean_age_months 
  )

 
  younger_estimates <- get_estimates(younger_res)
  older_estimates <- get_estimates(older_res)
  full_estimates <- get_estimates(full_res)
  
  
  estimates_summary <- bind_rows(
    younger_estimates %>% mutate(test_data = "younger"), 
    older_estimates %>% mutate(test_data = "older"), 
    full_estimates %>% mutate(test_data = "full")
  )%>% 
    mutate(ds_clean = ds_name)
  
  return(estimates_summary)
  
  }
}
