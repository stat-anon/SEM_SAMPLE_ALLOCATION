# ============================================================
# 99_all_in_1.R  —  one-click runner for MGSEM allocation tutorial
# Purpose:
#   - Ensure required packages
#   - Source the five modules (population/power, solvers, search, heuristics, viz)
#   - Provide thin wrappers for fixed-budget and fixed-power planning
#   - (Optional) run_all(): skeleton to reproduce figures/tables
# ============================================================

# ---- (A) packages ------------------------------------------------------------
.required_pkgs <- c("lavaan", "dplyr", "progress", "ggplot2", "patchwork","tibble")

.ensure_pkg <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p)
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}
.ensure_pkg(.required_pkgs)

# ---- (B) source modules ------------------------------------------------------
# Edit these filenames to match your actual 5 files.
FILES <- c(
  "01_power_engine.R",          # contains: extract_pop_model(), power_MGSEM()
  "02_constraint_solvers.R",    # contains: find_n2_given_power(), find_n2_given_budget()
  "03_optimization_strategies.R",# contains: grid_search_max_power(), grid_search_min_cost(),
                                 #           adaptive_step_max_power(), adaptive_step_min_cost()
  "04_heuristics_tiebreakers.R", # contains: guess_n2_bracket(), find_best_min_cost(), find_best_max_power()
  "05_visualization.R"           # contains: plot_allocation_diagnostics()
)

for (f in FILES) {
  if (!file.exists(f)) {
    message(sprintf("[warn] File not found: %s  (please adjust FILES[])", f))
  } else {
    source(f, local = TRUE)
    message(sprintf("[ok]   Sourced: %s", f))
  }
}

# ---- (C) thin wrappers (user-facing) ----------------------------------------
# Fixed budget: maximize power
plan_fixed_budget <- function(model_pop_list, model_ana_list,
                              unit_costs = c(1,1), budget = 1000,
                              n1_range = c(100, 1000),
                              step_sizes = c(100, 20, 3, 1),
                              alpha = .05,
                              include_n2_panel = TRUE) {
  res <- adaptive_step_max_power(
    initial_range = n1_range,
    step_sizes = step_sizes,
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list,
    unit_costs = unit_costs,
    budget = budget,
    alpha = alpha
  )
  p <- plot_allocation_diagnostics(
    res, mode = "fixed_budget", budget = budget,
    include_n2_panel = include_n2_panel
  )
  list(results = res, plot = p)
}

# Fixed power: minimize total cost
plan_fixed_power <- function(model_pop_list, model_ana_list,
                             unit_costs = c(1,1), target_power = .80,
                             n1_range = c(100, 1000),
                             step_sizes = c(50, 10, 1),
                             alpha = .05,
                             include_n2_panel = TRUE) {
  res <- adaptive_step_min_cost(
    model_pop_list = model_pop_list,
    model_ana_list = model_ana_list,
    unit_costs = unit_costs,
    target_power = target_power,
    alpha = alpha,
    initial_range = n1_range,
    step_sizes = step_sizes
  )
  p <- plot_allocation_diagnostics(
    res, mode = "fixed_power", target_power = target_power,
    include_n2_panel = include_n2_panel
  )
  list(results = res, plot = p)
}

# ---- (D) run_all(): skeleton (fill your models; saves figs) -----------------
# NOTE:
#   1) Replace the placeholders `model_pop_list_*` / `model_ana_list_*`
#      with your Example 1–3 model objects.
#   2) This function writes figures into ./fig/.
run_all <- function() {
  dir.create("fig", showWarnings = FALSE, recursive = TRUE)

  # ---- Example 1 (factor means) --------------------------------------------
  # Provide your own objects:
  # model_pop_list_ex1 <- list(
  #   list(model = "  # lavaan model text for Group 1 ..."),
  #   list(model = "  # lavaan model text for Group 2 ...")
  # )
  # model_ana_list_ex1 <- list(
  #   "  # null model (e.g., equal factor means) ...",
  #   "  # alternative model (freed factor means) ..."
  # )
  # out1_fp <- plan_fixed_power(model_pop_list_ex1, model_ana_list_ex1,
  #                             unit_costs = c(3,1), target_power = .80,
  #                             n1_range = c(200, 900))
  # out1_fb <- plan_fixed_budget(model_pop_list_ex1, model_ana_list_ex1,
  #                              unit_costs = c(3,1), budget = 2000,
  #                              n1_range = c(200, 800))
  # ggplot2::ggsave("fig/Example1_fixed_power.png", out1_fp$plot, width = 7, height = 6, dpi = 300)
  # ggplot2::ggsave("fig/Example1_fixed_budget.png", out1_fb$plot, width = 7, height = 6, dpi = 300)

  # ---- Example 2 (path coefficient) ----------------------------------------
  # model_pop_list_ex2 <- ...
  # model_ana_list_ex2 <- ...
  # out2 <- plan_fixed_power(model_pop_list_ex2, model_ana_list_ex2,
  #                          unit_costs = c(3,1), target_power = .80,
  #                          n1_range = c(150, 1200))
  # ggplot2::ggsave("fig/Example2_fixed_power.png", out2$plot, width = 7, height = 6, dpi = 300)

  # ---- Example 3 (factor loadings / metric invariance) ---------------------
  # model_pop_list_ex3 <- ...
  # model_ana_list_ex3 <- ...
  # out3 <- plan_fixed_budget(model_pop_list_ex3, model_ana_list_ex3,
  #                           unit_costs = c(1,1), budget = 1200,
  #                           n1_range = c(100, 800))
  # ggplot2::ggsave("fig/Example3_fixed_budget.png", out3$plot, width = 7, height = 6, dpi = 300)

  # ---- Session info ---------------------------------------------------------
  writeLines(c(capture.output(sessionInfo()), ""), "sessionInfo.txt")
  message("[ok] All done. Figures saved under ./fig; sessionInfo.txt written.")
}

# ---- (E) optional: auto-run when script is executed -------------------------
if (sys.nframe() == 0L) {
  message("[info] 99_all_in_1.R loaded. To run all examples, call: run_all()")
}
