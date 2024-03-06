
get_age_model_prediction <- function(ds_name, min_age_df, all_ds){
  
  current_df = all_ds %>% filter(ds_clean == ds_name)
  formula = (min_age_df %>% filter(dataset == ds_name))$model_spec
  model_type = (min_age_df %>% filter(dataset == ds_name))$model_spec_clean
  
  
  if (ds_name %in% (all_ds %>% filter(is.na(same_infant)) %>% distinct(ds_clean) %>% pull())){
    
    model <- rma.mv(as.formula(formula), 
                    V = d_var_calc, 
                    random = ~ 1 | short_cite/unique_row, 
                    data = current_df) 
    
  }else if(ds_name %in% (all_ds %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
    
    model <- rma.mv(as.formula(formula), 
                    V = d_var_calc, 
                    random = ~ 1 | short_cite/same_infant, 
                    data = current_df) 
    
  }else{
    
    model <- rma.mv(as.formula(formula), 
                    V = d_var_calc, 
                    random = ~ 1 | short_cite/same_infant/unique_row, 
                    data = current_df)
    
  }
  
  if (model_type == "Log"){
    age = log(seq(1, 36, .5))
  }else if(model_type == "Quadratic"){
    age = (seq(1, 36, .5)) ** 2
  }else{
    age = seq(1, 36, .5)
  }
  
  if (model_type != "Const"){
    prediction =  predict(model, newmods = age, addx =TRUE) %>% 
      as.data.frame() %>% 
      mutate(ds_name = ds_name)
    
  }else{
    
    prediction = predict(model) %>% 
      as.data.frame() 
    
    prediction = map_dfr(seq(length(age)), ~prediction)
    
    prediction$X.const.mean_age_months = age
    
    prediction$ds_name = ds_name
    
  }
  
  
  
  
  
  return (prediction)
}


get_moderators <- function(ds_name){
  
  key_moderators_df <- d %>% 
    filter(ds_clean == ds_name) %>% 
    summarise_all(n_distinct) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "column_name", 
                 values_to = "n_distinct_value") %>% 
    filter(column_name %in%  c("response_mode", "behavioral_measure", "exposure_phase"))
  
  
  moderator_df <- d %>% 
    filter(ds_clean == ds_name) %>% 
    summarise_all(n_distinct) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "column_name", 
                 values_to = "n_distinct_value") %>% 
   # filter(n_distinct_value != 1 & !column_name %in% c("response_mode", "behavioral_measure", "exposure_phase")) %>% 
    filter(n_distinct_value != 1) %>% 
    filter(!column_name %in% c("unique_row", 
                               "study_ID", 
                               "long_cite", 
                               "short_cite", 
                               "peer_reviewed", 
                               "expt_num", 
                               "expt_condition", 
                               "same_infant", 
                               "n_1", 
                               "mean_age_1", 
                               "x_1", 
                               "x_2", 
                               "SD_1", 
                               "SD_2", 
                               "t", 
                               "F", 
                               "d", 
                               "corr", 
                               "age_range_1", 
                               "n_excluded_1", 
                               "n_excluded_2",
                               "gender_2", 
                               "corr_imputed", 
                               "d_calc", 
                               "d_var_calc", 
                               "g_calc", 
                               "g_var_calc", 
                               "r_calc", 
                               "r_var_calc", 
                               "z_calc", 
                               "z_var_calc", 
                               "log_odds_calc", 
                               "log_odds_var_calc", 
                               "es_method", 
                               "mean_age", 
                               "n", 
                               "same_infant_calc", 
                               "mean_age_months", 
                               "year", 
                               "group_name_2",
                               "group_name_1",
                               "infant_type",
                               "r", 
                               "gender_1",
                               "gender_2",
                               "coder", 
                               "publication_type",
                               "dataset", 
                               "short_name", 
                               "d_var", 
                               "age_range_2", 
                               "original_ma", 
                               "lab",
                               "data_from_figure",
                               "effect_significance_reported",
                               "non_criterion", 
                               "criterion",
                               "Coder 1 Comments", 
                               "coder",
                               "n_2", 
                               "mean_age_2", 
                               "age_range_2", 
                               "n_excluded_2", 
                               "main_question_IDS_preference",
                               "fussers",
                               "participant_design", 
                               "expt_condition2",
                               "word_round", 
                               "word_spiky",
                               "participant_design",
                               "x_dif", 
                               "F1_1", 
                               "F1_2", 
                               "F2_1",
                               "F2_2",
                               "Linguistic", 
                               "preceding", 
                               "native_lang", 
                               "test_register", 
                               "percentV", 
                               "deltaC", 
                               # SB parsing issue 
                               "n_train_test_pair", 
                               "n_test_trial_per_pair", 
                               "n_repetitions_sentence", 
                               "n_repetitions_video",
                               
                               # too detailed for VD
                               "contrast_sampa", 
                               "contrast_pseudoIPA", 
                               "sampa_comments", 
                               "counterbalanced", 
                               "backness", 
                               "height", 
                               "nasality", 
                               "roundness", 
                               "tenseness", 
                               "length", 
                               "duration_word_1", 
                               "duration_word_2", 
                               "duration_vowel_1", 
                               "duration_vowel_2", 
                               "peripherality"
                               
    )) %>% 
    bind_rows(key_moderators_df) %>% 
    filter(n_distinct_value != 1) %>% 
    mutate(ds_clean =  ds_name) %>% 
    distinct(ds_clean, column_name) %>% 
    nest(moderator_name = column_name)
  
}



get_model_fit_df <- function(all_ds, ds_name, moderators, age_type = "mean_age_months"){
  
  if (is.null(moderators)){
    if (age_type == "mean_age_months"){
      linear_model_formula <-"d_calc ~ mean_age_months"
      log_model_formula <- "d_calc ~ log(mean_age_months)"
      qua_model_formula <- "d_calc ~ I(mean_age_months^2)"
      const_model_formula <- "d_calc ~ 1"
    }else{
      linear_model_formula <-"d_calc ~ delta_age"
      log_model_formula <- "d_calc ~ log(delta_age)"
      qua_model_formula <- "d_calc ~ I(delta_age^2)"
      const_model_formula <- "d_calc ~ 1"
    }
    
    
  }else{
    
    
    linear_model_formula <- paste0("d_calc ~ mean_age_months + ", paste(moderators, collapse = " + "))
    log_model_formula <- paste0("d_calc ~ log(mean_age_months) + ", paste(moderators, collapse = " + "))
    qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) + ", paste(moderators, collapse = " + "))
    const_model_formula <-  paste0("d_calc ~ 1 + ", paste(moderators, collapse = " + "))
  }
  
  formulas = c(linear_model_formula)
  
  
  
  # if (age_type == "mean_age_months"){
  #   formulas = c(linear_model_formula, log_model_formula, qua_model_formula, const_model_formula)
  # }else{
  #   formulas = c(linear_model_formula)
  # }
  # 
  
  res = lapply(formulas, 
               function(m){
                 
                 print(m)
                 # check if has same_infant
                 
                 if (ds_name %in% (d %>% filter(is.na(same_infant)) %>% distinct(ds_clean) %>% pull())){
                   print("~ 1 | short_cite/unique_row")
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/unique_row, 
                                                data = all_ds %>% filter(ds_clean == ds_name)), 
                                         effects = "fixed", conf.int=TRUE, conf.level = 0.95) %>% 
                     mutate(model_spec = m) 
                   
                   
                 }else if(ds_name %in% (d %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
                   
                   print(" random = ~ 1 | short_cite/same_infant, ")
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/same_infant, 
                                                data = all_ds %>% filter(ds_clean == ds_name)), 
                                         effects = "fixed", conf.int=TRUE, conf.level = 0.95) %>% 
                     mutate(model_spec = m) 
                   
                   
                   
                 }else{
                   
                   print("~ 1 | short_cite/same_infant/unique_row")
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/same_infant/unique_row, 
                                                data = all_ds %>% filter(ds_clean == ds_name)), 
                                         effects = "fixed", conf.int=TRUE, conf.level = 0.95) %>% 
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
    ))
  
  return (res)
}




get_model_fit_interaction_df <- function(all_ds, ds_name, moderators){
  
  if (is.null(moderators)){
    linear_model_formula <-"d_calc ~ mean_age_months"
    log_model_formula <- "d_calc ~ log(mean_age_months)"
    qua_model_formula <- "d_calc ~ I(mean_age_months^2)"
    const_model_formula <- "d_calc ~ 1"
    
  }else{
    
    
    linear_model_formula <- paste0("d_calc ~ mean_age_months * ", paste(moderators, collapse = " + "))
    log_model_formula <- paste0("d_calc ~ log(mean_age_months) * ", paste(moderators, collapse = " + "))
    qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) * ", paste(moderators, collapse = " + "))
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
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/unique_row, 
                                             data = all_ds %>% filter(ds_clean == ds_name))) %>% 
                     mutate(model_spec = m) 
                   
                 }else if(ds_name %in% (d %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant, 
                                             data = all_ds %>% filter(ds_clean == ds_name))) %>% 
                     mutate(model_spec = m) 
                   
                 }else{
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant/unique_row, 
                                             data = all_ds %>% filter(ds_clean == ds_name))) %>% 
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
    ))
  
  return (res)
}



get_compare_IC_df <- function(all_ds , ds_name, moderators){
  
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
                                             data = all_ds %>% filter(ds_clean == ds_name)))$fit.stats
                              ) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                    
                   
                   
                 }else if(ds_name %in% (d %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
                   
                   print("~ 1 | short_cite/same_infant")
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant, 
                                             data = all_ds %>% filter(ds_clean == ds_name)))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                   
                   
                 }else{
                   
                   #print("full!")
                   #print(m)
                   

                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant/unique_row, 
                                             data = all_ds %>% filter(ds_clean == ds_name)), 
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





get_compare_IC_interaction_df <- function(all_ds , ds_name, moderators){
  
  if (is.null(moderators)){
    linear_model_formula <-"d_calc ~ mean_age_months"
    log_model_formula <- "d_calc ~ log(mean_age_months)"
    qua_model_formula <- "d_calc ~ I(mean_age_months^2)"
    const_model_formula <- "d_calc ~ 1"
    
  }else{
    
    
    linear_model_formula <- paste0("d_calc ~ mean_age_months * ", paste(moderators, collapse = " + "))
    log_model_formula <- paste0("d_calc ~ log(mean_age_months) * ", paste(moderators, collapse = " + "))
    qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) * ", paste(moderators, collapse = " + "))
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
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/unique_row, 
                                             data = all_ds %>% filter(ds_clean == ds_name)))$fit.stats
                   ) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                 }else if(ds_name %in% (d %>% filter(is.na(unique_row)) %>% distinct(ds_clean) %>% pull())){
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant, 
                                             data = all_ds %>% filter(ds_clean == ds_name)))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                 }else{
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant/unique_row, 
                                             data = all_ds %>% filter(ds_clean == ds_name)))$fit.stats) %>% 
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



