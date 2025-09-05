# =====================================================================
# Note: The exhaustive-search variants below may require substantial computation time,
#       minutes to hours, depending on grid size and model complexity.
#       Use the adaptive search first; enable exhaustive scans only for validation. 
#       In the DEMO sections, exhaustive search is used deliberately to obtain finer-grained points 
#       for illustration and comparison."
# =====================================================================






# =====================================================================
# DEMO (Example 2): Path-coefficient difference under fixed power (min-cost)
# Purpose: compare three measurement conditions by residual variances,
#   with Group-1 path = .40 and Group-2 path = .60 (b1 vs b2).
#   For each condition, run grid_search_min_cost to find the
#   least-cost allocation (n1, n2) achieving target power .80.
#   This script ONLY adds comments; it does not modify logic.
# =====================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("R", "99_all_in_1.R"))


# ---------------------------------------------------------------------
# Population-generating models
# Two latent factors (f1, f2); each measured by three indicators.
# Residual variances define measurement conditions:
#   - (pop1 vs pop2): errors .60 in both groups, path .40 vs .60
#   - (pop3 vs pop4): Group 1 errors .60, Group 2 errors .30 (higher reliability), path .40 vs .60
#   - (pop5 vs pop6): errors .30 in both groups, path .40 vs .60
# Factor means/intercepts are set to anchor the mean structure.
# ---------------------------------------------------------------------
modelpop1 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.6*x1 
x2~~.6*x2 
x3~~.6*x3 
x4~~.6*x4 
x5~~.6*x5
x6~~.6*x6
x1~0*1
x2~1*1
x3~1*1
x4~0*1
x5~1*1
x6~1*1
f1~~1*f1 
f1~0*1
f2~.4*f1         # structural path in Group 1 = .40
f2~~.84*f2'      # ensures Var(f2) consistent with the path and residual

modelpop2 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.6*x1 
x2~~.6*x2 
x3~~.6*x3 
x4~~.6*x4 
x5~~.6*x5
x6~~.6*x6
x1~0*1
x2~1*1
x3~1*1
x4~0*1
x5~1*1
x6~1*1
f1~~1*f1 
f1~0*1
f2~.6*f1         # structural path in Group 2 = .60
f2~~.64*f2'

modelpop3 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.6*x1 
x2~~.6*x2 
x3~~.6*x3 
x4~~.6*x4 
x5~~.6*x5
x6~~.6*x6
x1~0*1
x2~1*1
x3~1*1
x4~0*1
x5~1*1
x6~1*1
f1~~1*f1 
f1~0*1
f2~.4*f1
f2~~.84*f2'

modelpop4 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.3*x1 
x2~~.3*x2 
x3~~.3*x3 
x4~~.3*x4 
x5~~.3*x5
x6~~.3*x6
x1~0*1
x2~1*1
x3~1*1
x4~0*1
x5~1*1
x6~1*1
f1~~1*f1 
f1~0*1
f2~.6*f1
f2~~.64*f2'

modelpop5 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.3*x1 
x2~~.3*x2 
x3~~.3*x3 
x4~~.3*x4 
x5~~.3*x5
x6~~.3*x6
x1~0*1
x2~1*1
x3~1*1
x4~0*1
x5~1*1
x6~1*1
f1~~1*f1 
f1~0*1
f2~.4*f1
f2~~.84*f2'

modelpop6 <-'
f1 =~ 1*x1+1*x2+1*x3
f2 =~ 1*x4+1*x5+1*x6
x1~~.3*x1 
x2~~.3*x2 
x3~~.3*x3 
x4~~.3*x4 
x5~~.3*x5
x6~~.3*x6
x1~0*1
x2~1*1
x3~1*1
x4~0*1
x5~1*1
x6~1*1
f1~~1*f1 
f1~0*1
f2~.6*f1
f2~~.64*f2'

# ---------------------------------------------------------------------
# Analysis models (nested): test the between-group difference in the
# path f1 → f2 (b1 vs b2), with metric invariance on loadings.
# model1 (null): b1 == b1  (equal across groups)
# model2 (alt) : b1 vs b2  (freely estimated across groups)
# Intercepts for x1, x4 fixed at 0 to anchor the means.
# ---------------------------------------------------------------------
model1 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3 
f2 =~ x4 + c(a5,a5)*x5 + c(a6,a6)*x6 
x1~0*1
x4~0*1
f2~c(b1,b1)*f1
'

model2 <- '
f1 =~ x1 + c(a2,a2)*x2 + c(a3,a3)*x3 
f2 =~ x4 + c(a5,a5)*x5 + c(a6,a6)*x6 
x1~0*1
x4~0*1
f2~c(b1,b2)*f1
'

# Pack analysis specs; population specs are assigned per condition below
model_ana_list <- list(model1, model2)

model_pop_list <- list(
  list(),  # Group 1 placeholder (Σ, μ built from text if absent)
  list()   # Group 2 placeholder
)

# =====================================================================
# Fixed power (target = .80). Unit-cost ratio c1:c2 = 2:1.
# For each measurement condition, sweep n1 and solve minimal n2
# achieving .80 power; then compute and export the full grid.
# Warm starts and local bracket guesses accelerate the root solve.
# =====================================================================

### Condition r1: equal errors .60 in both groups ---------------------
model_pop_list[[1]]$model <- modelpop1
model_pop_list[[2]]$model <- modelpop2
rst1 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(2, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(270,800,1),       # candidate n1 grid
    warm_start_n2 = 600,      # warm start for n2
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE, # local [a,b] around n2 from recent pairs
    widen_frac = 0.10,
    min_halfwidth = 5
)

### Condition r2: Group 2 has lower errors (.30) → higher reliability --
model_pop_list[[1]]$model <- modelpop3
model_pop_list[[2]]$model <- modelpop4
rst2 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(2, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(270,800,1),
    warm_start_n2 = 600,
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE,
    widen_frac = 0.10,
    min_halfwidth = 5
)

### Condition r3: both groups have lower errors (.30) -----------------
model_pop_list[[1]]$model <- modelpop5
model_pop_list[[2]]$model <- modelpop6
rst3 <- grid_search_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list, 
    unit_costs = c(2, 1), 
    target_power = .8,  alpha = .05,
    n1s=seq(270,800,1),
    warm_start_n2 = 600,
    n2min_start   = 280,
    n2max_start   = 2000,
    use_bracket_guess = TRUE,
    widen_frac = 0.10,
    min_halfwidth = 5
)

# ---------------------------------------------------------------------
# Export full grids for Figure/Table reproduction (one CSV per condition)
# Each CSV includes columns: n1, n2, power, cost_used.
# ---------------------------------------------------------------------
write.csv(rst1$all,"rst201.csv",row.names=FALSE)
write.csv(rst2$all,"rst202.csv",row.names=FALSE)
write.csv(rst3$all,"rst203.csv",row.names=FALSE)





# =====================================================================
# Plot script for Example 2 (Fixed-Power / Min-Cost)
# - Reads three result CSVs (rst201–rst203) produced by the solver
# - Builds iso-power (n2 vs n1) curves at target power = 0.80
# - Computes total cost under two unit-cost schemes (C1:C2 = 3:1 and 2:1)
# - Marks the cost-minimizing n1 for each scenario
# - Assembles three panels side-by-side with a shared legend
# =====================================================================

library(cowplot)

# 1) Read results and tag scenarios
#    r1: equal residual variances (.60, .60)
#    r2: Group 2 has lower residual variances (.60 vs .30)
#    r3: both groups have lower residual variances (.30, .30)
df1 <- read_csv("rst201.csv") %>% mutate(scenario = "Equal reliability (r1)")
df2 <- read_csv("rst202.csv") %>% mutate(scenario = "Group 2 higher reliability (r2)")
df3 <- read_csv("rst203.csv") %>% mutate(scenario = "Both high reliability (r3)")

# 2) Combine data and compute total cost under two cost ratios
#    cost_31: C1:C2 = 3:1; cost_21: C1:C2 = 2:1
df_all <- bind_rows(df1, df2, df3) %>%
  mutate(
    scenario = factor(scenario),
    cost_31 = 3 * n1 + 1 * n2,
    cost_21 = 2 * n1 + 1 * n2
  )

# 3) Iso-power curve (n2 vs n1) at target power = 0.80
p_n1n2 <- ggplot(df_all, aes(x = n1, y = n2,
                             linetype = scenario, shape = scenario)) +
  geom_line(linewidth = 0.7, color = "black") +
  coord_cartesian(xlim = c(200, 800), ylim = c(200, 2000)) +
  labs(
    title = "Iso-Power (Target Power = 0.80): n2 vs n1",
    x = "n1 (Group 1 sample size)",
    y = "n2 (Group 2 sample size)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4) Cost vs n1 under C1:C2 = 3:1
#    Mark the minimizing n1 within each scenario
min_31 <- df_all %>%
  group_by(scenario) %>%
  slice_min(cost_31, n = 1, with_ties = FALSE)

p_cost_31 <- ggplot(df_all, aes(x = n1, y = cost_31,
                                linetype = scenario, shape = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = min_31, aes(x = n1, y = cost_31),
             size = 3, shape = 21, fill = "white") +
  geom_text(data = min_31, aes(x = n1, y = cost_31, label = n1),
            vjust = -1, size = 3) +
  geom_vline(data = min_31, aes(xintercept = n1),
             linetype = "dashed", alpha = 0.6) +
  coord_cartesian(xlim = c(200, 800), ylim = c(1100, 2200)) +
  labs(
    title = "Total Cost vs n1 (C1:C2 = 3:1)",
    x = "n1 (Group 1 sample size)",
    y = "Total cost"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 5) Cost vs n1 under C1:C2 = 2:1
#    Mark the minimizing n1 within each scenario
min_21 <- df_all %>%
  group_by(scenario) %>%
  slice_min(cost_21, n = 1, with_ties = FALSE)

p_cost_21 <- ggplot(df_all, aes(x = n1, y = cost_21,
                                linetype = scenario, shape = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = min_21, aes(x = n1, y = cost_21),
             size = 3, shape = 21, fill = "white") +
  geom_text(data = min_21, aes(x = n1, y = cost_21, label = n1),
            vjust = -1, size = 3) +
  geom_vline(data = min_21, aes(xintercept = n1),
             linetype = "dashed", alpha = 0.6) +
  coord_cartesian(xlim = c(200, 800), ylim = c(1100, 2200)) +
  labs(
    title = "Total Cost vs n1 (C1:C2 = 2:1)",
    x = "n1 (Group 1 sample size)",
    y = "Total cost"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 6) Assemble panels (top row = three plots; bottom row = shared legend)
#    We keep the legend from the iso-power plot and hide in others.
p_n1n2_leg <- p_n1n2 +
  guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

leg <- cowplot::get_legend(p_n1n2_leg)

p_n1n2_nl   <- p_n1n2   + theme(legend.position = "none")
p_cost_31_nl <- p_cost_31 + theme(legend.position = "none")
p_cost_21_nl <- p_cost_21 + theme(legend.position = "none")

top_row <- p_n1n2_nl | p_cost_31_nl | p_cost_21_nl
cowplot::plot_grid(top_row, leg, ncol = 1, rel_heights = c(1, 0.1))

