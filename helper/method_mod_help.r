
# currently only considering studies with behavioral method
get_best_fit_model_with_mod <- function(d, mod){
  

  subset_d <- d %>% 
    filter(!response_mode %in% c("NIRS", "EEG")) %>% 
    distinct(ds_clean,eval(as.symbol(mod))) %>% 
    group_by(ds_clean) %>% 
    count() %>% 
    filter(!n == 1) %>% 
    pull(ds_clean)
  
  # for model fits
  subset_model_compared_d <- lapply(subset_d, 
                               function(name){
                                 print(name)
                                 get_compare_IC_df(d, name, c(mod))
                               }) %>% 
    bind_rows() %>% 
    mutate(model_type = mod)
  
  
  # finding the best fit models when adding method moderator 
  min_AICc_subset_model_compared_d <- subset_model_compared_d %>% 
    filter(ic == "AICc") %>% 
    group_by(dataset) %>% 
    filter(REML == min(REML)) 
  
  # finding the stats of the models 
  subset_models_fits_df <- lapply(subset_d, 
                                    function(name){
                                      print(name)
                                      get_model_fit_df(d, name, c(mod))
                                    }) %>% 
    bind_rows()
  
  
  # finding the best fit behavioral model 
  best_fit_model_df <- subset_models_fits_df %>% 
    left_join(min_AICc_subset_model_compared_d, 
              by =  c("model_spec", "dataset", "model_spec_clean")) %>% 
    filter(!is.na(REML)) 
  
  return (best_fit_model_df)
  
}