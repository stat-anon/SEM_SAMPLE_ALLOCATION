# =====================================================================
# QUICK DEMO (RECOMMENDED): Example 1 — Factor means
# - This is the streamlined, suggested workflow: use ADAPTIVE SEARCH ONLY.
# - Fixed budget (max-power) via adaptive_step_max_power()
# - Fixed power (min-cost) via adaptive_step_min_cost()
# - No exhaustive or staged grids here.
# =====================================================================

source('99_all_in_1.R')

message(
  "Quick Demo running with adaptive search only.\n",
  "Note: Exhaustive scans are omitted here and should be used only ",
  "for validation or to obtain very fine-grained curves."
)

# ---------------------------------------------------------------------
# Population-generating models (Group 1, Group 2)
# Design: 1-factor, five indicators, equal loadings (=1), residuals .60,
#         standardized factor (var=1). Group-mean contrast: 0 vs 0.20.
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
# model1 = null (constrain f1 mean equal across groups)
# model2 = alternative (allow f1 means to differ)
# Metric invariance imposed via equal loadings on x2–x5.
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

# Pack analysis and population specs (concise form)
model_ana_list <- list(model1, model2)
model_pop_list <- lapply(list(modelpop1, modelpop2), function(m) list(model = m))

# =====================================================================
# FIXED BUDGET (Maximize power) — Adaptive search
# Budget = 2000; unit costs c1:c2 = 3:1
# =====================================================================
t <- Sys.time()
rst3 <- adaptive_step_max_power(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list, 
  unit_costs = c(3, 1), 
  budget = 2000, alpha = .05,
  initial_range = c(200, 650),   # n1 search window
  step_sizes   = c(50, 20, 3, 1) # step cascade: coarse→fine
)
rst3
t3 <- Sys.time() - t

# Compared with equal sample sizes (n1 = n2 = 500)
# Compute the achieved power under equal allocation
show(power_MGSEM(model_pop_list, model_ana_list, n = c(500, 500), alpha = .05))

# =====================================================================
# FIXED POWER (Minimize cost) — Adaptive search
# Target power = .80; same unit costs
# =====================================================================
t <- Sys.time()
rst6 <- adaptive_step_min_cost(
  model_pop_list = model_pop_list,
  model_ana_list = model_ana_list, 
  unit_costs = c(3, 1), 
  target_power = .80, alpha = .05,
  initial_range = c(400, 800),   # n1 search window
  step_sizes   = c(100, 33, 11, 4, 1) # step cascade: coarse→fine
)
rst6
t6 <- Sys.time() - t

# compared with equal sample sizes
show(power_MGSEM(model_pop_list, model_ana_list, n=c(629,629), alpha = .05))
show(cost <- 629*3+629)

# =====================================================================
# Diagnostics: paired panels for the two planning modes
# Left: fixed budget (power vs n1; optional n2 panel)
# Right: fixed power (cost vs n1; optional n2 panel)
# =====================================================================
pic_budget <- plot_allocation_diagnostics(
  rst3, mode = "fixed_budget", budget = 2000, 
  include_n2_panel = TRUE,
  y_cost_lim = c(1990, 2010)
)

pic_power <- plot_allocation_diagnostics(
  rst6, mode = "fixed_power", target_power = .80,
  include_n2_panel = TRUE,
  y_power_lim = c(.79, .83)
)

# Display side-by-side (requires patchwork)
(pic_budget | pic_power)

# ---------------------------------------------------------------------
# Timings (wall-clock) for reporting
# ---------------------------------------------------------------------
t3
t6
