# =====================================================================
# Note: The exhaustive-search variants below may require substantial computation time,
#       minutes to hours, depending on grid size and model complexity.
#       Use the adaptive search first; enable exhaustive scans only for validation. 
#       In the DEMO sections, exhaustive search is used deliberately to obtain finer-grained points 
#       for illustration and comparison."
# =====================================================================






# ===============================================================
# Example 3 generator + solver (Fixed-Budget / Max-Power)
# - Builds a 3-factor, 9-indicator population with placeholder
#   loadings a1..a6 on the second/third indicators per factor.
# - Creates five loading-contrast conditions for Group 2
#   (0.60, 0.65, 0.70, 0.75, 0.80 vs Group 1 fixed at 0.50).
# - For each condition and budget level B, runs adaptive_step_max_power
#   to find the power-maximizing allocation (n1, n2) under C1:C2 = 1:1.
# - Stores the best row per (condition, B) to rst301.csv
# ===============================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("R", "99_all_in_1.R"))

library('stringr')

# -- Population template with placeholders a1..a6 for loadings (2nd & 3rd indicators on each factor)
modelpop <- '
f1 =~ 1*x1 + a1*x2 + a2*x3
f2 =~ 1*x4 + a3*x5 + a4*x6
f3 =~ 1*x7 + a5*x8 + a6*x9
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

# -- Helper: replace a1..a6 with numeric strings (e.g., "0.500")
replace_a <- function(model, vals, vals_fmt) {
  repl <- setNames(paste0(vals_fmt), names(vals))
  model_new <- str_replace_all(model, repl)
  return(model_new)
}

# -- Group 1 population: loadings = .50 for placeholders
vals <- rep(.5, 6); names(vals) <- paste0("a", 1:6)
vals_fmt   <- formatC(vals, digits = 3, format = "f")
modelpop1  <- replace_a(modelpop, vals, vals_fmt)

# -- Group 2 baseline (not used directly below; we vary it in the loop)
vals <- rep(.6, 6); names(vals) <- paste0("a", 1:6)
vals_fmt   <- formatC(vals, digits = 3, format = "f")
modelpop2  <- replace_a(modelpop, vals, vals_fmt)

# -- Pack population lists (Σ, μ will be built from model text by the engine)
model_pop_list <- list(list(), list())
model_pop_list[[1]]$model <- modelpop1
model_pop_list[[2]]$model <- modelpop2  # will be overwritten per condition below

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
f3 =~ x7 + c(a8,b8)*x8 + c(a9,b9)*x9
'
model_ana_list <- list(model1, model2)

# -- Sweep loading-contrast conditions for Group 2
#    Group 1 stays at .50; Group 2 varies from .60 to .80 by .05 (5 conditions)
results <- list(); idx <- 1
for (condition in 1:5) {
  lambda   <- .60 + (condition - 1) * .05
  vals     <- rep(lambda, 6); names(vals) <- paste0("a", 1:6)
  vals_fmt <- formatC(vals, digits = 3, format = "f")
  modelpop2temp <- replace_a(modelpop, vals, vals_fmt)
  model_pop_list[[2]]$model <- modelpop2temp

  # -- For each total budget B, locate the power-maximizing allocation (n1, n2)
  for (B in seq(400, 1600, 100)) {
    rsttemp <- adaptive_step_max_power(
      model_pop_list  = model_pop_list,
      model_ana_list  = model_ana_list,
      unit_costs      = c(1, 1),
      budget          = B,
      alpha           = .05,
      initial_range   = c(10, 2000),
      step_sizes      = c(400, 100, 50, 25, 5, 1)
    )
    best <- as.data.frame(rsttemp$best)  # columns: n1, n2, power, cost_used (≈ B), etc.
    best$condition <- condition          # store condition index (1..5)
    results[[idx]] <- best
    idx <- idx + 1
  }
}

# -- Bind and save results for plotting
rst <- bind_rows(results)
write.csv(rst, "rst301.csv", row.names = FALSE)



# ===============================================================
# Example 3 plotting
# - Reads rst301.csv (best row per budget & condition)
# - Builds two panels:
#   (top) optimal sample-size ratio n2/n1 vs budget B
#   (bottom) achieved power vs budget B, with 0.80 reference
# - Marks for each loading-contrast the smallest B that reaches
#   power ≥ .80 (vertical dotted line)
# ===============================================================


df <- read.csv("rst301.csv", header = TRUE)

# If your CSV contains 'condition' (1..5) rather than 'combo',
# promote it to a labeled factor for plotting:
if (!"combo" %in% names(df) && "condition" %in% names(df)) {
  df <- df %>% mutate(combo = condition)
}

# Keep a sparser subset along the budget grid if the file has dense rows
df <- df %>%
  group_by(combo) %>%
  slice(seq(1, n(), by = 5)) %>%
  ungroup()

# Rename cost_used -> B (total budget) and label the five loading contrasts
df <- df %>%
  rename(B = cost_used) %>%
  mutate(
    combo = factor(
      combo, levels = 1:5,
      labels = c(
        "loadings .50 vs .60",
        "loadings .50 vs .65",
        "loadings .50 vs .70",
        "loadings .50 vs .75",
        "loadings .50 vs .80"
      )
    ),
    ratio = n2 / n1
  )

# For each contrast, find the smallest budget that attains power ≥ .80
Bstar <- df %>%
  group_by(combo) %>%
  filter(power >= 0.80) %>%
  slice_min(B, with_ties = FALSE) %>%
  ungroup() %>%
  select(combo, B)

# Line types and shapes for visual separation across contrasts
lt_vals <- c("solid", "longdash", "dashed", "dotdash", "twodash")
sh_vals <- c(16, 17, 15, 3, 18)

# (Top) Optimal sample-size ratio n2/n1 vs Budget
p1 <- ggplot(df, aes(x = B, y = ratio, linetype = combo, shape = combo)) +
  geom_line(color = "black", linewidth = 0.6) +
  geom_point(color = "black", size = 2, alpha = 0.5) +
  geom_vline(data = Bstar, aes(xintercept = B),
             linetype = "dotted", linewidth = 0.6, color = "black",
             show.legend = FALSE) +
  labs(
    title = "Optimal Sample Ratio (n2 / n1) vs Budget",
    x = NULL, y = NULL, linetype = "condition", shape = "condition"
  ) +
  scale_y_continuous(limits = c(0.5, 0.9)) +
  scale_x_continuous(limits = c(350, 1350)) +
  scale_linetype_manual(values = lt_vals) +
  scale_shape_manual(values = sh_vals) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# (Bottom) Achieved power vs Budget (with 0.80 reference line)
p2 <- ggplot(df, aes(x = B, y = power, linetype = combo, shape = combo)) +
  geom_line(color = "black", linewidth = 0.6) +
  geom_point(color = "black", size = 2, alpha = 0.5) +
  geom_vline(data = Bstar, aes(xintercept = B),
             linetype = "dotted", linewidth = 0.6, color = "black",
             show.legend = FALSE) +
  geom_hline(yintercept = 0.80, linetype = "dashed", linewidth = 0.6, color = "black") +
  labs(
    title = "Power vs Budget",
    x = "Budget", y = NULL, linetype = "condition", shape = "condition"
  ) +
  scale_y_continuous(limits = c(0.2, 1.00)) +
  scale_x_continuous(limits = c(350, 1350)) +
  scale_linetype_manual(values = lt_vals) +
  scale_shape_manual(values = sh_vals) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Assemble panels and collect legend on the right
(p1 / p2) +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "right")
