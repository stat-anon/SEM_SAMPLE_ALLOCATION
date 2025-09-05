# ===============================================================
# QUICK DEMO (RECOMMENDED): Example 3 — Factor-loadings (Fixed-Budget / Max-Power)
# - One scenario only:
#     Group 1: secondary loadings = .50
#     Group 2: secondary loadings = .70
# - For total budget 600, use ADAPTIVE search to find (n1, n2) that maximizes power
#   under C1:C2 = 1:1 (equal unit costs).
# NOTE: This script adds comments only; it does not modify core logic.
# ===============================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("R", "99_all_in_1.R"))

# -- Population template with placeholders a1..a6 (2nd & 3rd indicator on each factor)
modelpop1 <- '
f1 =~ 1*x1 +.5*x2 + .5*x3
f2 =~ 1*x4 + .5*x5 + .5*x6
f3 =~ 1*x7 + .5*x8 + .5*x9
x1~~.6*x1; x2~~.6*x2; x3~~.6*x3
x4~~.6*x4; x5~~.6*x5; x6~~.6*x6
x7~~.6*x7; x8~~.6*x8; x9~~.6*x9
x1~0*1; x2~1*1; x3~1*1
x4~0*1; x5~1*1; x6~1*1
x7~0*1; x8~1*1; x9~1*1
f1~~1*f1; f1~0*1
f2~~1*f2; f2~0*1
f3~~1*f3; f3~0*1
# structural paths among factors (kept equal across groups)
f1~.5*f2
f1~.5*f3
f2~.5*f3
'

modelpop2 <- '
f1 =~ 1*x1 + .7*x2 + .7*x3
f2 =~ 1*x4 + .7*x5 +.7*x6
f3 =~ 1*x7 + .7*x8 + .7*x9
x1~~.6*x1; x2~~.6*x2; x3~~.6*x3
x4~~.6*x4; x5~~.6*x5; x6~~.6*x6
x7~~.6*x7; x8~~.6*x8; x9~~.6*x9
x1~0*1; x2~1*1; x3~1*1
x4~0*1; x5~1*1; x6~1*1
x7~0*1; x8~1*1; x9~1*1
f1~~1*f1; f1~0*1
f2~~1*f2; f2~0*1
f3~~1*f3; f3~0*1
# structural paths among factors (kept equal across groups)
f1~.5*f2
f1~.5*f3
f2~.5*f3
'


# -- Analysis models:
#    model1: metric invariance (equal loadings across groups)
#    model2: free loadings across groups (alternative)
model1 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3
f2 =~ x4 + c(a5,a5)*x5 + c(a6,a6)*x6
f3 =~ x7 + c(a8,a8)*x8 + c(a9,a9)*x9
'
model2 <- '
f1 =~ x1 + c(a2,b2)*x2 + c(a3,b3)*x3
f2 =~ x4 + c(a5,b5)*x5 + c(a6,b6)*x6
f3 =~ x7 + c(a8,b8)*x8 + c(a9,b9)*x9'


# -- Pack population lists (Σ, μ will be built from model text if absent)
model_pop_list <- lapply(list(modelpop1, modelpop2), function(m) list(model = m))
model_ana_list <- list(model1, model2)

# -- Adaptive search over budgets: find max-power (n1, n2) for each B

rst <- adaptive_step_max_power(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list,
    unit_costs     = c(1, 1),
    budget         = 600,
    alpha          = .05,
    initial_range  = c(10, 2000),           # wide window; adaptive will shrink it
    step_sizes     = c(400, 100, 50, 25, 5, 1)
)
rst


# OPTIONAL: quick diagnostics 
plot_allocation_diagnostics(rst, mode = "fixed_budget", budget = 600,
                             include_n2_panel = TRUE)




