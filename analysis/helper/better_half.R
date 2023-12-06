


get_model_estimate_df <- function(data, ds_name, moderators){
  
  if (is.null(moderators)){
    
    base_model <-"d_calc ~ 1"
    
    models = c(base_model)
    
  }else if (moderators == "age"){
    
    linear_model_formula <-"d_calc ~ mean_age_months"
    log_model_formula <- "d_calc ~ log(mean_age_months)"
    qua_model_formula <- "d_calc ~ I(mean_age_months^2)"
    const_model_formula <- "d_calc ~ 1"
    
    models = c(linear_model_formula, 
               log_model_formula, 
               qua_model_formula, 
               const_model_formula)
  
  }else{
    
    
    linear_model_formula <- paste0("d_calc ~ mean_age_months * ", paste(moderators, collapse = " + "))
    log_model_formula <- paste0("d_calc ~ log(mean_age_months) * ", paste(moderators, collapse = " + "))
    qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) * ", paste(moderators, collapse = " + "))
    const_model_formula <-  paste0("d_calc ~ 1 + ", paste(moderators, collapse = " + "))
    
    models = c(linear_model_formula, 
               log_model_formula, 
               qua_model_formula, 
               const_model_formula)
  }
  
  
  
  
  res = lapply(models, 
               function(m){
                 #print(m)
                 # check if has same_infant
                 
                 if (ds_name %in% (d %>% filter(is.na(same_infant)) %>% distinct(ds_clean) %>% pull())){
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/unique_row, 
                                                data = data)) %>% 
                     mutate(model_spec = m) 
                   
                 }else if(ds_name %in% (d %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/same_infant, 
                                                data = data)) %>% 
                     mutate(model_spec = m) 
                   
                 }else{
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/same_infant/unique_row, 
                                                data = data)) %>% 
                     mutate(model_spec = m) 
                   
                 }
                 
                 
               }) %>% 
    bind_rows() %>% 
    mutate(dataset = ds_name) %>% 
    mutate(model_spec_clean = case_when(
      grepl("log", model_spec) ~ "Log", 
      grepl("2", model_spec) ~ "Quadratic", 
      grepl("1", model_spec) ~ "Const",
      TRUE ~ "Linear"
    )) %>% 
    mutate(lb = estimate - std.error, ub = estimate + std.error)
  
  return (res)
}



get_compare_IC_df <- function(data , ds_name, moderators){
  
  if (is.null(moderators)){
    linear_model_formula <-"d_calc ~ mean_age_months"
    log_model_formula <- "d_calc ~ log(mean_age_months)"
    qua_model_formula <- "d_calc ~ I(mean_age_months^2)"
    const_model_formula <- "d_calc ~ 1"
    
  }else{
    
    linear_model_formula <- paste0("d_calc ~ mean_age_months + ", paste(moderators, collapse = " + "))
    log_model_formula <- paste0("d_calc ~ log(mean_age_months) + ", paste(moderators, collapse = " + "))
    qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) + ", paste(moderators, collapse = " + "))
    const_model_formula <-  paste0("d_calc ~ 1 + ", paste(moderators, collapse = " + "))
  }
  
  
  
  
  res = lapply(c(linear_model_formula, 
                 log_model_formula, 
                 qua_model_formula, 
                 const_model_formula), 
               function(m){
                 #print(m)
                 # check if has same_infant
                 
                 if (ds_name %in% (d %>% filter(is.na(same_infant)) %>% distinct(ds_clean) %>% pull())){
                   
                   print("~ 1 | short_cite/unique_row")
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/unique_row, 
                                             data = data))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                   
                   
                   
                 }else if(ds_name %in% (d %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
                   
                   print("~ 1 | short_cite/same_infant")
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant, 
                                             data = data))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                   
                   
                 }else{
                   
                   #print("full!")
                   #print(m)
                   
                   
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant/unique_row, 
                                             data = data), 
                                      control =list(msMaxIter = 1000, msMaxEval = 1000))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                   
                   
                 }
                 
                 
               }) %>% 
    bind_rows() %>% 
    mutate(dataset = ds_name) %>% 
    mutate(model_spec_clean = case_when(
      grepl("log", model_spec) ~ "Log", 
      grepl("2", model_spec) ~ "Quadratic", 
      grepl("1", model_spec) ~ "Const",
      TRUE ~ "Linear"
    ))
  
  return (res)
}
