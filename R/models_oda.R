# Settings ----------------------------------------------------------------

oda_setup <- function() {
  options(worker_options)
  
  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 4045  # From random.org
  
  # Priors
  prior_num <- c(set_prior("normal(0, 10)", class = "Intercept"),
                 set_prior("normal(0, 2.5)", class = "b"),
                 set_prior("cauchy(0, 1)", class = "sd"))
  
  prior_denom <- c(set_prior("normal(0, 10)", class = "Intercept"),
                   set_prior("normal(0, 2.5)", class = "b"),
                   set_prior("normal(0, 2.5)", class = "sd"))
  
  prior_out <- c(set_prior("normal(0, 20)", class = "Intercept"),
                 set_prior("normal(0, 3)", class = "b"),
                 set_prior("cauchy(0, 1)", class = "sd"),
                 set_prior("logistic(-2, 0.6)", class = "Intercept", dpar = "hu"))
  
  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              prior_num = prior_num, prior_denom = prior_denom, prior_out = prior_out))
}


# Treatment models --------------------------------------------------------

f_oda_treatment_total <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + total_oda_log_lag1 +
         # Human rights and politics
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         # Economics and development
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         # Conflict and disasters
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_advocacy <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_entry <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_funding <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}


# Outcome models ----------------------------------------------------------

f_oda_outcome_total <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ 
         barriers_total + (1 | gwcode),
       hu ~ 1),
    data = dat,
    family = hurdle_lognormal(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed,
    # Has to be rstan instead of cmdstanr bc it inexplicably
    # creates all sorts of nonnegative initialization errors when
    # using cmdstanr with lognormal() :(
    backend = "rstan"
  )
  
  return(model)
}

f_oda_outcome_advocacy <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ 
         advocacy + (1 | gwcode),
       hu ~ 1),
    data = dat,
    family = hurdle_lognormal(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed,
    backend = "rstan"
  )
  
  return(model)
}

f_oda_outcome_entry <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ 
         entry + (1 | gwcode),
       hu ~ 1),
    data = dat,
    family = hurdle_lognormal(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed,
    backend = "rstan"
  )
  
  return(model)
}

f_oda_outcome_funding <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ 
         funding + (1 | gwcode),
       hu ~ 1),
    data = dat,
    family = hurdle_lognormal(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed,
    backend = "rstan"
  )
  
  return(model)
}


# Example models for demonstrating lognormal interpretation ---------------

f_example_normal <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ barriers_total + (1 | gwcode)),
    data = dat,
    family = gaussian(), 
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(model)
}

f_example_hurdle <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ barriers_total + (1 | gwcode), 
       hu ~ 1),
    data = dat,
    family = hurdle_lognormal(), backend = "rstan",
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(model)
}
