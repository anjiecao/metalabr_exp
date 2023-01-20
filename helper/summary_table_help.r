


get_descriptive_stas <- function(d){
  
  n_studies <- length(unique(d$study_ID))
  n_effects <- nrow(d)
  n_kids <- (d %>% 
    mutate(n_2 = ifelse(is.na(n_2), 0, n_2)) %>% 
    distinct(same_infant, n_1, n_2) %>% 
    mutate(total_infants = n_1 + n_2) %>% 
    summarise(n = sum(total_infants)))$n
  
  min_age <- min(d$mean_age_months, na.rm = TRUE)
  max_age <- max(d$mean_age_months, na.rm = TRUE)
  mean_age <- mean(d$mean_age_months, na.rm = TRUE)
  
  basic_descriptive <- tibble(
    "n_studies" = n_studies, 
    "n_effects" = n_effects, 
    "n_kids" = n_kids, 
    "min_age" = min_age, 
    "max_age" = max_age, 
    "mean_age" = mean_age
  )
  
  return (basic_descriptive)
  
}


get_ma_effect_size <- function(d){
  
  # building different random effect structure 
  formula = "d_calc ~ 1"
  
  if (!nrow(d %>% filter(!is.na(same_infant))) == nrow(d)){
    
    model <- rma.mv(as.formula(formula), 
                    V = d_var_calc, 
                    random = ~ 1 | short_cite/unique_row, 
                    data = d) 
    
  }else if(!nrow(d %>% filter(!is.na(unique_row))) == nrow(d)){
    
    model <- rma.mv(as.formula(formula), 
                    V = d_var_calc, 
                    random = ~ 1 | short_cite/same_infant, 
                    data = d) 
    
  }else{
    
    model <- rma.mv(as.formula(formula), 
                    V = d_var_calc, 
                    random = ~ 1 | short_cite/same_infant/unique_row, 
                    data = d)
    
  }
  
  es <- tibble(
    "es" = model$b[[1]],
    "es_lb" = model$ci.lb,
    "es_ub" = model$ci.ub
  ) 
  
  
  return (es)
  
  
}