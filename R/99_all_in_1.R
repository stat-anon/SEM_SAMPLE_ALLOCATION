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
FILES <- c(
  "01_power_engine.R",          # contains: extract_pop_model(), power_MGSEM()
  "02_constraint_solvers.R",    # contains: find_n2_given_power(), find_n2_given_budget()
  "03_optimization_strategies.R",# contains: grid_search_max_power(), grid_search_min_cost(),
                                 #           adaptive_step_max_power(), adaptive_step_min_cost()
  "04_heuristics_tiebreakers.R", # contains: guess_n2_bracket(), find_best_min_cost(), find_best_max_power()
  "05_visualization.R"           # contains: plot_allocation_diagnostics()
)

for (f in FILES) {
  path <- here::here("R", f)          # 統一用 here 組出完整路徑
  if (!file.exists(path)) {
    message(sprintf("[warn] File not found: %s  (please adjust FILES[])", f))
  } else {
    source(path, local = FALSE)        # 匯入到全域環境，讓 demo 能直接呼叫
    message(sprintf("[ok]   Sourced: %s", f))
  }
}

