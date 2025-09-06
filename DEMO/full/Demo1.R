# =====================================================================
# Note: The exhaustive-search variants below may require substantial computation time,
#       minutes to hours, depending on grid size and model complexity.
#       Use the adaptive search first; enable exhaustive scans only for validation. 
#       In the DEMO sections, exhaustive search is used deliberately to obtain finer-grained points 
#       for illustration and comparison."
# =====================================================================






# =====================================================================
# DEMO (Example 1): Factor means — fixed budget (max-power) & fixed power (min-cost)
# Reproduction script using the five-module toolbox loaded via 99_all_in_1.R
# - Defines population-generating models for Group 1/2
# - Defines nested analysis models (null vs. alternative)
# - Runs grid and adaptive searches under both planning modes
# - Records runtime and renders diagnostics (Figure-style panels)
# NOTE: This file only adds comments; it does not alter your code paths.
# =====================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("R", "99_all_in_1.R"))


# ---------------------------------------------------------------------
# Population-generating models (Group 1, Group 2)
# Design: 1-factor, five indicators, equal loadings (=1), residuals .60,
#         standardized factor (var=1). Group-mean contrast: 0 vs 0.20.
# These strings are consumed by extract_pop_model() when needed.
# ---------------------------------------------------------------------
modelpop1 <-'
f1 =~ 1*x1+1*x2+1*x3+1*x4+1*x5
x1~~.6*x1 
x2~~.6*x2 
x3~~.6*x3 
x4~~.6*x4 
x5~~.6*x5
x1~0*1
x2~1*1
x3~1*1
x4~1*1
x5~1*1
f1~~1*f1 
f1~0*1'

modelpop2 <-'
f1 =~ 1*x1+1*x2+1*x3+1*x4+1*x5
x1~~.6*x1 
x2~~.6*x2 
x3~~.6*x3 
x4~~.6*x4 
x5~~.6*x5
x1~0*1
x2~1*1
x3~1*1
x4~1*1
x5~1*1
f1~~1*f1 
f1~0.2*1'

# ---------------------------------------------------------------------
# Analysis models (nested): mean equality test on factor f1
# model1 = null (constrain f1 mean equal across groups: m1 = m1)
# model2 = alternative (allow f1 mean to differ: m1 vs m2)
# Metric invariance is imposed via equal loadings on x2–x5.
# ---------------------------------------------------------------------
model1 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3 + c(a4,a4)*x4 + c(a5,a5)*x5
x1~0*1
f1~c(m1,m1)*1
'

model2 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3 + c(a4,a4)*x4 + c(a5,a5)*x5
x1~0*1
f1~c(m1,m2)*1
'

# Pack analysis and population specifications
model_ana_list <- list(model1, model2)
model_pop_list <- lapply(list(modelpop1, modelpop2), function(m) list(model = m))


# =====================================================================
# FIXED BUDGET (Maximize power): three search strategies
# Budget = 2000; unit costs c1:c2 = 3:1. Compare exhaustive grid,
# staged grid (coarse→fine), and adaptive step refinement.
# Timing uses wall-clock deltas (t1, t2, t3).
# =====================================================================

##### exhaustive search (unit step) ------------------------------------
t <- Sys.time()
rst1 <- grid_search_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    budget=2000,  alpha = .05,
    n1s=seq(200,650,1)        # unit-step sweep over n1
)
t1 <- Sys.time()-t

##### fixed step search (coarse → fine, manual narrowing) -------------------
t <- Sys.time()
rst21 <- grid_search_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    budget=2000,  alpha = .05,
    n1s=seq(200,650,100)      # coarse
)
rst22 <- grid_search_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    budget=2000,  alpha = .05,
    n1s=seq(300,500,30)       # refine
)
rst23 <- grid_search_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    budget=2000,  alpha = .05,
    n1s=seq(390,450,10)       # refine
)
rst24 <- grid_search_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    budget=2000,  alpha = .05,
    n1s=seq(411,429,1)        # final near-optimal window
)
t2 <- Sys.time()-t

##### adaptive search (coarse → fine, automated) ------------------------
t <- Sys.time()
rst3 <- adaptive_step_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    budget=2000,  alpha = .05,
    initial_range = c(200, 650),   # n1 search window
    step_sizes = c(50,20,3,1)      # step cascade: coarse→fine
)
t3 <- Sys.time()-t

# Compared with equal sample sizes (n1 = n2 = 500)
# Compute the achieved power under equal allocation
show(power_MGSEM(model_pop_list, model_ana_list, n = c(500, 500), alpha = .05))


# =====================================================================
# FIXED POWER (Minimize cost): three search strategies
# Target power = .80; same unit costs. Compare exhaustive grid,
# staged grid, and adaptive step; record timings (t4–t6).
# Warm starts and bracket guesses accelerate the root-finding.
# =====================================================================

### exhaustive search (unit step, with warm starts) --------------------
t <- Sys.time()
rst4 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(400,800,1),       # unit-step sweep over n1
    warm_start_n2 = 600,      # prior guess for n2
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE, # local [a,b] around n2 from recent pairs
    widen_frac = 0.10,
    min_halfwidth = 5
)
t4 <- Sys.time()-t

### fixed step search (coarse → fine) --------------------------------------
t <- Sys.time()
rst51 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(400,800,100),     # coarse
    warm_start_n2 = 600,
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE,
    widen_frac = 0.10,
    min_halfwidth = 5
)
rst52 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(400,600,20),      # refine
    warm_start_n2 = 600,
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE,
    widen_frac = 0.10,
    min_halfwidth = 5
)
rst53 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(480,520,3),       # refine
    warm_start_n2 = 600,
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE,
    widen_frac = 0.10,
    min_halfwidth = 5
)
rst54 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(497,503,1),       # final near-optimal window
    warm_start_n2 = 600,
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE,
    widen_frac = 0.10,
    min_halfwidth = 5
)
t5 <- Sys.time()-t

### adaptive search (automated coarse → fine) ---------------------------
t <- Sys.time()
rst6 <- adaptive_step_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(3, 1), 
    target_power = .8,  alpha = .05,
    initial_range = c(400, 800),   # n1 search window
    step_sizes = c(100,33,11,4,1)  # step cascade: coarse→fine
)
t6 <- Sys.time()-t

# Equal-sample baseline (n1 = n2 = 629)
# Compute achieved power and total cost under equal allocation
# -------------------------------------------------------------------
p_equal <- power_MGSEM(model_pop_list, model_ana_list, n = c(629, 629), alpha = .05)
show(cost_equal <- 629 * 3 + 629)   # unit costs: c1 = 3, c2 = 1

# =====================================================================
# Diagnostics: paired panels for the two planning modes
# Left: fixed budget (power vs n1, cost line at budget, optional n2 panel)
# Right: fixed power (cost vs n1, power line at target, optional n2 panel)
# Axis bands (y_*_lim) are set to zoom on the neighborhood of the solution.
# =====================================================================

pic21 <- plot_allocation_diagnostics(
  rst1, mode="fixed_budget", budget=2000, 
  include_n2_panel=TRUE,
  y_cost_lim=c(1990,2010))

pic22 <- plot_allocation_diagnostics(
  rst4, mode="fixed_power", target_power=.80,
  include_n2_panel=TRUE,
  y_power_lim=c(.79, .83))

# Display side-by-side (requires patchwork)
(pic21|pic22)

# ---------------------------------------------------------------------
# Timings (wall-clock) for each search variant
# Useful for reporting “search effort” in the manuscript.
# ---------------------------------------------------------------------
t1
t2
t3
t4
t5
t6
