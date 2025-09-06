# =====================================================================
# QUICK DEMO (RECOMMENDED): Example 2 — Path-coefficient difference
# Fixed power (min-cost) via ADAPTIVE SEARCH ONLY
# - Condition r1: errors .60 in both groups; path b1=.40 (G1) vs b2=.60 (G2)
# - Unit-cost ratio c1:c2 = 2:1
# =====================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("R", "99_all_in_1.R"))


# --- Population (r1) --------------------------------------------------
modelpop1 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.6*x1; x2~~.6*x2; x3~~.6*x3
x4~~.6*x4; x5~~.6*x5; x6~~.6*x6
x1~0*1; x2~1*1; x3~1*1
x4~0*1; x5~1*1; x6~1*1
f1~~1*f1; f1~0*1
f2~.4*f1
f2~~.84*f2'

modelpop2 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.6*x1; x2~~.6*x2; x3~~.6*x3
x4~~.6*x4; x5~~.6*x5; x6~~.6*x6
x1~0*1; x2~1*1; x3~1*1
x4~0*1; x5~1*1; x6~1*1
f1~~1*f1; f1~0*1
f2~.6*f1
f2~~.64*f2'

# --- Analysis (nested): test b1 vs b2 on f1 → f2 ---------------------
model1 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3
f2 =~ x4 + c(a5,a5)*x5 + c(a6,a6)*x6
x1~0*1; x4~0*1
f2~c(b1,b1)*f1   # null: equal path across groups
'
model2 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3
f2 =~ x4 + c(a5,a5)*x5 + c(a6,a6)*x6
x1~0*1; x4~0*1
f2~c(b1,b2)*f1   # alt: free path across groups
'

# --- Pack specs -------------------------------------------------------
model_ana_list <- list(model1, model2)
model_pop_list <- lapply(list(modelpop1, modelpop2), function(m) list(model = m))

# --- Adaptive search: fixed power (target = .80), min cost ------------
rst_adapt <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list,
  unit_costs = c(2, 1),
  target_power = .80, alpha = .05,
  initial_range = c(300, 800),     # n1 search window
  step_sizes   = c(100, 20, 5, 1)  # coarse → fine
)

# --- Inspect the recommended allocation & time ------------------------
rst_adapt


# --- Optional diagnostics (fixed-power panels) ------------------------
plot_allocation_diagnostics(
  rst_adapt, mode = "fixed_power", target_power = .80,
  include_n2_panel = TRUE,
  y_power_lim = c(.78, .83)
)

