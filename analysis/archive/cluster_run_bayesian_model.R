library(tidyverse)
library(here)
library(brms)
library(loo)


# Reading in data 
d <- read_csv(here("data/metalab_data_mini.csv"))

# Set up the models 

age_const <-bf(d_calc | se(d_var_calc) ~ 1 + (1 | short_cite/same_infant/unique_row))
age_linear <- bf(d_calc | se(d_var_calc) ~ mean_age_months + (1 | short_cite/same_infant/unique_row))
age_quad <- bf(d_calc | se(d_var_calc) ~ I(mean_age_months^2) + (1 | short_cite/same_infant/unique_row))
age_log <- bf(d_calc | se(d_var_calc) ~ log(mean_age_months) + (1 | short_cite/same_infant/unique_row))

# Set up priors (need to try out different priors later for sensitivity analysis)

priors <- c(prior(normal(0, 2.5), class = Intercept),
            prior(normal(1, 1), class = sd),
            prior(gamma(2, 0.1), class = nu))


# Set up function to run model 
run_model <- function(model, model_name, ds_name, data){
  m <- brm(model,  save_pars = save_pars(all = TRUE), data = data, 
           family = student, prior = priors, file = here("cache_model", paste0(ds_name, "_", model_name)),
           iter = 9000, warmup = 500,  chains = 2,control = list(adapt_delta = 0.999,max_treedepth = 20 ))
  
  m <- add_criterion(m, criterion = c("loo", "waic", "kfold"), overwrite = FALSE)
}

# currently selecting a dataset to run 
ds_name <- "Mutual exclusivity"
run_model(age_const, "const", ds_name, d %>% filter(ds_clean == ds_name))
run_model(age_linear, "linear", ds_name, d %>% filter(ds_clean == ds_name))
run_model(age_quad, "quad", ds_name, d %>% filter(ds_clean == ds_name))
run_model(age_log, "log", ds_name, d %>% filter(ds_clean == ds_name))


# Run 4 models on each dataset 



