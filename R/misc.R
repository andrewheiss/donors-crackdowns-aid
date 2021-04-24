matrix_from_vector <- function(x, ncol) {
  n_balanced <- ceiling(length(x) / ncol) * ncol
  matrix(c(x, rep(NA, n_balanced - length(x))), ncol = ncol)
}

cumprod_na <- function(x) {
  x[is.na(x)] <- 1
  return(cumprod(x))
}

# Via https://stackoverflow.com/a/55323097/120898
lhs <- function(x) {
  if (attr(terms(as.formula(x)), which = "response")) {
    all.vars(x)[1]
  } else {
    NULL
  }
}

calc_prob_dist <- function(form, pred, resid, data = df_country_aid_laws) {
  dnorm(data[[lhs(form$formula)]],
        pred[,1],
        sd(resid[,1], na.rm = TRUE))
}
