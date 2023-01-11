

get_author_model_fits <- function(d, ds_name, major_author_name){
  
  current_d <- (d %>% 
                  filter(ds_clean == ds_name, major_author == major_author_name))$data[[1]]
  
  linear_model_formula <- paste0("d_calc ~ mean_age_months + by_major_author")
  log_model_formula <- paste0("d_calc ~ log(mean_age_months) + by_major_author")
  qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) + by_major_author")
  const_model_formula <-  paste0("d_calc ~ 1 + by_major_author")
  
  
  
  res = lapply(c(linear_model_formula, 
                 log_model_formula, 
                 qua_model_formula, 
                 const_model_formula), 
               function(m){
                 #print(m)
                 # check if the random variable has everything
                 
                 
                 
                 if (!nrow(current_d %>% filter(!is.na(same_infant))) == nrow(current_d)){
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/unique_row, 
                                                data = current_d)) %>% 
                     mutate(model_spec = m) 
                   
                 }else if(!nrow(current_d %>% filter(!is.na(unique_row))) == nrow(current_d)){
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/same_infant, 
                                                data = current_d)) %>% 
                     mutate(model_spec = m) 
                   
                 }else{
                   
                   raw_df <- broom::tidy(rma.mv(as.formula(m), 
                                                V = d_var_calc, 
                                                random = ~ 1 | short_cite/same_infant/unique_row, 
                                                data = current_d)) %>% 
                     mutate(model_spec = m) 
                   
                   
                 }
                 
                 
               }) %>% 
    bind_rows() %>% 
    mutate(dataset = ds_name, 
           major_author = major_author_name) %>% 
    mutate(model_spec_clean = case_when(
      grepl("log", model_spec) ~ "Log", 
      grepl("2", model_spec) ~ "Quadratic", 
      grepl("1", model_spec) ~ "Const",
      TRUE ~ "Linear"
    ))
  
  return (res)
  
    
}




get_author_model_comparison <- function(d, ds_name, major_author_name){
  
  current_d <- (d %>% 
    filter(ds_clean == ds_name, major_author == major_author_name))$data[[1]]
  
  linear_model_formula <- paste0("d_calc ~ mean_age_months + by_major_author")
  log_model_formula <- paste0("d_calc ~ log(mean_age_months) + by_major_author")
  qua_model_formula <- paste0("d_calc ~ I(mean_age_months^2) + by_major_author")
  const_model_formula <-  paste0("d_calc ~ 1 + by_major_author")
  
  
  
  res = lapply(c(linear_model_formula, 
                 log_model_formula, 
                 qua_model_formula, 
                 const_model_formula), 
               function(m){
                 #print(m)
                 # check if the random variable has everything
                 
            
                 
                 if (!nrow(current_d %>% filter(!is.na(same_infant))) == nrow(current_d)){
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/unique_row, 
                                             data = current_d))$fit.stats
                   ) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                 }else if(!nrow(current_d %>% filter(!is.na(unique_row))) == nrow(current_d)){
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant, 
                                             data = current_d))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                 }else{
                   
                   raw_df <- (summary(rma.mv(as.formula(m), 
                                             V = d_var_calc, 
                                             random = ~ 1 | short_cite/same_infant/unique_row, 
                                             data = current_d))$fit.stats) %>% 
                     mutate(model_spec = m) %>% 
                     rownames_to_column("ic") 
                   
                 }
                 
                 
               }) %>% 
    bind_rows() %>% 
    mutate(dataset = ds_name, 
           major_author = major_author_name) %>% 
    mutate(model_spec_clean = case_when(
      grepl("log", model_spec) ~ "Log", 
      grepl("2", model_spec) ~ "Quadratic", 
      grepl("1", model_spec) ~ "Const",
      TRUE ~ "Linear"
    ))
 
  return (res)
   
}





get_major_author_effect <- function(main_d, ds_name, ds_author_percent, age_limit = FALSE){
  d <- ds_author_percent %>% 
    filter(percent_by_author > .15) %>% 
    filter(ds_clean == ds_name) 
  
  
  if (ds_name != "Prosocial Agents"){
    major_authors_ds <- lapply(d$author_clean, 
                               function(name){
                                 main_d %>% 
                                   filter(ds_clean == ds_name) %>% 
                                   mutate(
                                     by_major_author = case_when(
                                       grepl(name, long_cite) ~ "yes", 
                                       !grepl(name, long_cite) ~ "no"
                                     )
                                   ) %>% 
                                   #filter(ds_clean %in% c("Mispronunciation sensitivity", "Prosocial agents")) %>% 
                                   #filter(mean_age_months < 36) %>% 
                                   mutate(major_author = name)
                               }) %>% 
      bind_rows()
    
  }else{
    major_author_ds <- main_d %>% filter(ds_clean == ds_name)
  }
  
  if (age_limit){
    major_authors_ds <- major_authors_ds %>% filter(mean_age_months < 36)
  }
  
  return (major_authors_ds)
  

  
}


visualize_major_author_effect <- function(main_d, ds_name, ds_author_percent, age_limit = FALSE){
  d <- ds_author_percent %>% 
    filter(percent_by_author > .15) %>% 
    filter(ds_clean == ds_name) 
  
  
  major_authors_ds <- lapply(d$author_clean, 
                             function(name){
                               main_d %>% 
                                 filter(ds_clean == ds_name) %>% 
                                 mutate(
                                   by_major_author = case_when(
                                     grepl(name, long_cite) ~ "yes", 
                                     !grepl(name, long_cite) ~ "no"
                                   )
                                 ) %>% 
                                 #filter(ds_clean %in% c("Mispronunciation sensitivity", "Prosocial agents")) %>% 
                                 #filter(mean_age_months < 36) %>% 
                                 mutate(major_author = name)
                             }) %>% 
    bind_rows()
  
  if (age_limit){
    major_authors_ds <- major_authors_ds %>% filter(mean_age_months < 36)
  }
  
  
  
  major_authors_ds %>% 
    ggplot(aes(x = mean_age_months, y = d_calc, color = by_major_author))+
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~major_author) + 
    labs(title =ds_name)
}


visualize_method_diff <- function(ds, moderators){
  
  method_ds <- ds %>% 
    select(moderators) %>% 
    mutate_if(is.double, as.character) %>% 
    mutate(total_n_no_author = nrow(ds %>% filter(by_major_author == "no")), 
           total_n_yes_author = nrow(ds %>% filter(by_major_author == "yes"))) %>% 
    pivot_longer(cols = moderators[! moderators %in% c('long_cite', 'by_major_author')], 
                 names_to = "moderator_type", 
                 values_to = "moderator_value") %>% 
    group_by(
      by_major_author, total_n_no_author,total_n_yes_author,
      moderator_type, moderator_value) %>% 
    count() 
  
  method_ds %>% 
    filter(by_major_author == "no") %>% 
    mutate(proportion = (n / total_n_no_author) )%>% 
    bind_rows(method_ds %>% 
                filter(by_major_author == "yes") %>% 
                mutate(proportion = n / total_n_yes_author)) %>% 
    ungroup() %>% 
    select(by_major_author, moderator_type, moderator_value, proportion) %>% 
    ggplot(aes(x = moderator_value,  
               y = proportion)) + 
    geom_point() + 
    facet_grid(by_major_author ~ moderator_type, scales = "free") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
}