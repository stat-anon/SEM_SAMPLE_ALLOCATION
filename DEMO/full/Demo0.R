# ================================================================
# Validation with Classical Results (t-test special cases)
# Purpose:
#   Use single-indicator, two-group mean comparisons—i.e., standard
#   two-sample mean tests—to sanity-check the optimizer. In this setting,
#   the theoretical optimal ratio is:
#       n2 / n1 = (sigma2 / sigma1) * sqrt(c1 / c2)
#   We vary cost ratios, SD ratios, alpha, and target power, and compare
#   the optimizer’s chosen (n1, n2) against this formula.
# ================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("R", "99_all_in_1.R"))

# ------------------------------------------------
# Population-generating models for the t-test case
# One observed variable x1 per group; group means differ; variances vary.
#   modelpop1: Group 1 -> mean = 0.0, var = 1
#   modelpop2: Group 2 -> mean = 0.2, var = 1
#   modelpop3: Group 2 -> mean = 0.2, var = 4  (SD ratio = 2)
# ------------------------------------------------
modelpop1 <- '
x1~0*1
x1~~1*x1
'

modelpop2 <- '
x1~0.2*1
x1~~1*x1
'

modelpop3 <- '
x1~0.2*1
x1~~4*x1
'

# ------------------------------------------------
# Analysis models (nested) for mean difference
# model1 (null): equal means across groups
# model2 (alt) : means free across groups
# Group variances are free in both models (a3, a4).
# ------------------------------------------------
model1 <- '
x1~c(a1,a1)*1
x1~~c(a3,a4)*x1
'

model2 <- '
x1~c(a1,a2)*1
x1~~c(a3,a4)*x1
'

model_ana_list <- list(model1, model2)

# Pack population model specs (placeholders; we assign models below)
model_pop_list <- list(list(), list())
model_pop_list[[1]]$model <- modelpop1
model_pop_list[[2]]$model <- modelpop2  # will change to modelpop3 for some conditions

# ================================================================
# Conditions 1–6: Fixed-power (min-cost) under various settings
#   - unit_costs: (c1, c2)
#   - target_power: typically 0.80, one case at 0.95
#   - alpha: typically 0.05, one case at 0.10
#   - SD ratio manipulated by switching Group 2 variance (modelpop2 vs modelpop3)
# The adaptive step search reduces the n1 scan window and step sizes over time.
# ================================================================

# Condition 1: equal cost (1:1), equal SD (1:1), target power .80, alpha .05
rst1 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(1, 1),
  target_power   = .80, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Condition 2: cost ratio 4:1 (c1:c2), equal SD (1:1), target power .80
rst2 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  target_power   = .80, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Switch Group 2 population to variance = 4 (SD ratio = 2)
model_pop_list[[2]]$model <- modelpop3

# Condition 3: equal cost (1:1), SD ratio 1:2, target power .80
rst3 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(1, 1),
  target_power   = .80, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Condition 4: cost ratio 4:1 plus SD ratio 1:2, target power .80
rst4 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  target_power   = .80, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Condition 5: cost ratio 4:1, SD ratio 1:2, target power .95
rst5 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  target_power   = .95, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Condition 6: cost ratio 4:1, SD ratio 1:2, target power .80, alpha .10
rst6 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  target_power   = .80, alpha = .10,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# ================================================================
# Conditions 7–9: Fixed-budget (max-power) under various budgets
# Keep cost ratio 4:1 and SD ratio 1:2 (Group 2 variance = 4)
# ================================================================

# Condition 7: budget = 2000
rst7 <- adaptive_step_max_power(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  budget         = 2000, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Condition 8: budget = 200
rst8 <- adaptive_step_max_power(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  budget         = 200, alpha = .05,
  initial_range  = c(20, 2000),
  step_sizes     = c(200, 50, 10, 2, 1)
)

# Condition 9: budget = 8000 (narrow initial window around expected sizes)
rst9 <- adaptive_step_max_power(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs     = c(4, 1),
  budget         = 8000, alpha = .05,
  initial_range  = c(800, 1200),
  step_sizes     = c(200, 50, 10, 2, 1)
)


# ================================================================
# Assemble a summary table comparing the optimizer’s n2/n1 ratio
# with the classical formula: (sigma2/sigma1) * sqrt(c1/c2).
# The helper mk_row() standardizes each row’s information.
# ================================================================
mk_row <- function(id, tag, res, c_ratio, sd_ratio, alpha, target_power, plan, budget = NA) {
  # If the search failed to return a valid best row, return NA-filled placeholders.
  if (is.null(res$best) || any(is.na(res$best[, c("n1", "n2")]))) {
    return(tibble(
      scenario       = id,
      tag            = tag,
      plan           = plan,
      alpha          = alpha,
      target_power   = target_power,
      budget         = budget,
      c_ratio        = c_ratio,
      sd_ratio       = sd_ratio,
      expected_ratio = as.numeric(sd_ratio) *
        sqrt(as.numeric(strsplit(c_ratio, ":")[[1]][1]) /
             as.numeric(strsplit(c_ratio, ":")[[1]][2])),
      n1             = NA_integer_,
      n2             = NA_integer_,
      observed_ratio = NA_real_,
      delta_pct      = NA_real_
    ))
  }

  n1 <- res$best$n1
  n2 <- res$best$n2
  cr <- as.numeric(strsplit(c_ratio, ":")[[1]])
  sr <- as.numeric(sd_ratio)
  exp_ratio <- sr * sqrt(cr[1] / cr[2])

  tibble(
    scenario       = id,
    tag            = tag,
    plan           = plan,
    alpha          = alpha,
    target_power   = target_power,
    budget         = budget,
    c_ratio        = c_ratio,
    sd_ratio       = sd_ratio,
    expected_ratio = exp_ratio,
    n1             = n1,
    n2             = n2,
    observed_ratio = n2 / n1,
    delta_pct      = 100 * ((n2 / n1) / exp_ratio - 1)
  )
}

tb <- bind_rows(
  mk_row(1, "equal cost/var",   rst1, "1:1", 1,  .05, .80, "fixed_power"),
  mk_row(2, "cost 4:1",         rst2, "4:1", 1,  .05, .80, "fixed_power"),
  mk_row(3, "sd 1:2",           rst3, "1:1", 2,  .05, .80, "fixed_power"),
  mk_row(4, "cost 4:1 x sd 1:2",rst4, "4:1", 2,  .05, .80, "fixed_power"),
  mk_row(5, "power .95",        rst5, "4:1", 2,  .05, .95, "fixed_power"),
  mk_row(6, "alpha .10",        rst6, "4:1", 2,  .10, .80, "fixed_power"),
  mk_row(7, "budget 2000",      rst7, "4:1", 2,  .05, NA,  "fixed_budget", budget = 2000),
  mk_row(8, "budget 200",       rst8, "4:1", 2,  .05, NA,  "fixed_budget", budget = 200),
  mk_row(9, "budget 8000",      rst9, "4:1", 2,  .05, NA,  "fixed_budget", budget = 8000)
) %>%
  arrange(scenario)

tb


