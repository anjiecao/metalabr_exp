
get_moderator_comparison <- function(d, ds_name, moderator){
  # figure out how many levels included in this d
  d <- d %>% filter(ds_clean == ds_name)
  name = deparse(substitute(moderator))
  levels = d %>% select(!!sym(moderator)) %>% distinct() %>% filter(!is.na(!!sym(moderator))) %>% pull()
  
  
  
  raw_res <- lapply(levels, function(x){
    sub_d = d %>% filter(!!sym(moderator) == x)
    if (nrow(sub_d) > 1){
      res_d <- get_compare_IC_df(sub_d, ds_name = ds_name, NULL) %>% 
        filter(ic == "AICc") %>% 
        mutate(data = x)
    }
  }) %>% 
    bind_rows()
  
  res_wide <- raw_res %>% 
    select(REML, model_spec_clean, data) %>% 
    pivot_wider(names_from = model_spec_clean, 
                values_from = REML)
  
  
  res_wide$min <- apply(res_wide[c('Linear','Log', 'Quadratic', 'Const')], 1, min)
  
  res_wide <- res_wide %>% 
    mutate(d_linear = Linear - min, 
           d_log = Log - min, 
           d_quad = Quadratic - min, 
           d_const= Const - min) %>% 
    select(data, d_linear, d_log, d_quad, d_const) %>% 
    rename(Linear = d_linear, 
           Log = d_log, 
           Quadratic = d_quad, 
           Const = d_const) %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    mutate(ds_name = ds_name, 
           comparison = moderator)
  
  
  return (res_wide)
}

get_slope_estimate_raw <- function(d, ds_name, moderator){
  # figure out how many levels included in this d
  d <- d %>% filter(ds_clean == ds_name)
  name = deparse(substitute(moderator))
  levels = d %>% select(!!sym(moderator)) %>% distinct() %>% filter(!is.na(!!sym(moderator))) %>% pull()
  
  
  
  raw_res <- lapply(levels, function(x){
    sub_d = d %>% filter(!!sym(moderator) == x)
    if (nrow(sub_d) > 1){
      res_d <- get_model_fit_df(sub_d, ds_name, NULL) %>% 
        mutate(modelrator = name, level = x) %>% 
        mutate(level = as.character(level))
    }
  }) %>% 
    bind_rows() 
  
  
  
  
  return (raw_res)
}
