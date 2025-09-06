############  Power engine  #######################################

# ============================================================
# Population generator (SEM → population moments Σ, μ)
# Purpose: derive the implied population covariance and mean from
#   a specified lavaan model—inputs for LRT-based, parameter-specification
#   power analysis in two-group SEM.
# Note: expands lavaan’s lower-triangular storage to a full, named Σ.
# ============================================================
extract_pop_model <- function(model_text) {
  fit <- sem(model_text, sample.nobs = 10000, meanstructure = TRUE)
  cov_mat <- fitted(fit)$cov
  mean_vec <- fitted(fit)$mean

  # Handle lavaan's lower-triangular storage
  if (is.vector(cov_mat)) {
    cov_mat <- lavaan:::lav_matrix_lower2full(cov_mat)
    varnames <- names(mean_vec)
    dimnames(cov_mat) <- list(varnames, varnames)
  }
  list(cov = cov_mat, mean = mean_vec)
}


# ============================================================
# LRT-based power engine for two-group SEM (nested models)
# Purpose: given population moments (Σ, μ) per group and two analysis
#   models (null with cross-group constraints vs. alternative), compute
#   a priori power at level α via the noncentral χ² from lavTestLRT.
# Notes: builds Σ/μ from model text if missing; returns NA on fit errors.
# ============================================================
power_MGSEM <- function(model_pop_list, model_ana_list, n, alpha = .05) {
  stopifnot(length(model_pop_list) == 2, length(model_ana_list) == 2)

  # Ensure each group has mean/cov; if not, build from model text
  model_pop_list <- lapply(seq_along(model_pop_list), function(i) {
    pop <- model_pop_list[[i]]
    if (is.null(pop$mean) || is.null(pop$cov)) {
      if (is.null(pop$model)) stop("model_pop_list[[", i, "]] missing 'mean/cov' and 'model'")
      stat <- extract_pop_model(pop$model)
      pop$mean <- stat$mean
      pop$cov  <- stat$cov
    }
    pop
  })

  means <- lapply(model_pop_list, `[[`, "mean")
  covs  <- lapply(model_pop_list, `[[`, "cov")

  fit_1 <- tryCatch(
    sem(model = model_ana_list[[1]], sample.cov = covs, sample.mean = means,
        sample.nobs = n, meanstructure = TRUE,
        control = list(rel.tol = 1e-6, iter.max = 10000), do.fit = TRUE),
    error = function(e) e
  )
  fit_2 <- tryCatch(
    sem(model = model_ana_list[[2]], sample.cov = covs, sample.mean = means,
        sample.nobs = n, meanstructure = TRUE,
        control = list(rel.tol = 1e-6, iter.max = 10000), do.fit = TRUE),
    error = function(e) e
  )
  if (inherits(fit_1, "error") || inherits(fit_2, "error")) return(NA_real_)

  lrt     <- lavTestLRT(fit_1, fit_2)
  ncp     <- lrt[2, "Chisq diff"]
  df_diff <- lrt[2, "Df diff"]
  1 - pchisq(qchisq(1 - alpha, df_diff), df = df_diff, ncp = ncp)
}
