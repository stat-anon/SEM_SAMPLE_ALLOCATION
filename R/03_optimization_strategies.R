############  Optimization strategies  #######################################

# ============================================================
# Grid search (fixed budget): maximize power over candidate n1
# Purpose: for each n1, derive n2 from the linear budget constraint,
#   evaluate LRT-based power, and return the full grid plus the
#   best allocation under fixed-budget (max-power) planning.
# ============================================================
grid_search_max_power <- function(n1s, model_pop_list, model_ana_list,
                                  unit_costs = c(1, 1), budget = 1000, alpha = .05,
                                  show_progress = TRUE,
                                  progress_callback = NULL) {
  pb <- if (show_progress && is.null(progress_callback)) progress::progress_bar$new(
    total = length(n1s), format = " grid max-power [:bar] :percent"
  ) else NULL

  results <- data.frame(n1 = n1s, n2 = NA_integer_, cost_used = NA_real_, power = NA_real_)
  for (i in seq_along(n1s)) {
    n1 <- n1s[i]
    res <- find_n2_given_budget(n1, unit_costs, budget)
    n2 <- res$n2; cost_used <- res$cost_used
    pwr <- if (!is.na(n2) && n2 >= 1) power_MGSEM(model_pop_list, model_ana_list, c(n1, n2), alpha) else NA_real_
    results[i, ] <- c(n1, n2, cost_used, pwr)

    if (!is.null(progress_callback)) progress_callback() else if (!is.null(pb)) pb$tick()
  }
  best <- find_best_max_power(results)
  list(all = results, best = best)
}

# ============================================================
# Grid search (fixed power): minimize total cost over candidate n1
# Purpose: for each n1, solve the minimal n2 achieving target power via
#   robust bracketing + bisection (with local bracket guesses and warm starts),
#   then compute total cost; return full grid and the min-cost allocation.
# ============================================================
grid_search_min_cost <- function(
  n1s,
  unit_costs = c(1, 1),
  target_power = .80,
  model_pop_list,
  model_ana_list,
  alpha = 0.05,
  n2min_start = 20,
  n2max_start = 10000,
  max_n2max = 100000,
  expand_step = 2,
  max_expand_attempts = 20,
  warm_start_n2 = NULL,
  show_progress = TRUE,
  progress_callback = NULL,
  # local bracket guess
  use_bracket_guess = TRUE,
  widen_frac = 0.20,     # ±20% margin
  min_halfwidth = 10L    # minimum half-width
) {
  n1s <- sort(n1s)
  last_successful_n2 <- warm_start_n2
  last_pairs <- data.frame(n1 = integer(0), n2 = integer(0))  # keep most recent successful pairs (max 2)

  # Local 1–2 point linear guesser (scoped copy for this function)
  guess_n2_bracket <- function(n1, last_pairs, widen_frac = 0.20,
                               min_halfwidth = 10L, cap = 1e6L) {
    lp <- last_pairs
    lp <- lp[stats::complete.cases(lp$n1, lp$n2), c("n1","n2"), drop = FALSE]
    if (nrow(lp) == 0) return(NULL)
    n2_hat <- as.integer(lp$n2[nrow(lp)])
    if (nrow(lp) >= 2) {
      x1 <- lp$n1[nrow(lp) - 1L]; y1 <- lp$n2[nrow(lp) - 1L]
      x2 <- lp$n1[nrow(lp)];       y2 <- lp$n2[nrow(lp)]
      if (x2 != x1) {
        b <- (y2 - y1) / (x2 - x1); a <- y2 - b * x2
        n2_hat <- as.integer(round(a + b * n1))
      }
    }
    n2_hat <- max(1L, min(as.integer(cap), n2_hat))
    half   <- max(as.integer(min_halfwidth), as.integer(ceiling(abs(n2_hat) * widen_frac)))
    a <- max(1L, n2_hat - half)
    b <- min(as.integer(cap), n2_hat + half)
    list(a = a, b = b, n2_hat = n2_hat)
  }

  pb <- if (show_progress && is.null(progress_callback)) {
    progress::progress_bar$new(total = length(n1s), format = " grid min-cost [:bar] :percent")
  } else NULL

  rows <- lapply(n1s, function(n1) {
    on.exit({
      if (!is.null(progress_callback)) progress_callback() else if (!is.null(pb)) pb$tick()
    }, add = TRUE)

    # Warm-start bounds
    n2min <- if (!is.null(last_successful_n2)) floor(min(last_successful_n2/2, n2min_start)) else n2min_start
    n2max <- if (!is.null(last_successful_n2)) ceiling(max(last_successful_n2,   n2max_start)) else n2max_start

    # Use local bracket guess if available
    if (use_bracket_guess) {
      prop <- try(guess_n2_bracket(n1, last_pairs, widen_frac = widen_frac,
                                   min_halfwidth = min_halfwidth, cap = max_n2max),
                  silent = TRUE)
      if (!inherits(prop, "try-error") && !is.null(prop)) {
        n2min <- max(1L, prop$a)
        n2max <- max(n2min + 1L, prop$b)
      }
    }

    # Quick bracket check at endpoints (allow touching the target)
    valid_bounds <- FALSE; expand_attempts <- 0L
    while (n2max <= max_n2max && expand_attempts < max_expand_attempts) {
      fa <- tryCatch(power_MGSEM(model_pop_list, model_ana_list, c(n1, n2min), alpha) - target_power,
                     error = function(e) NA_real_)
      fb <- tryCatch(power_MGSEM(model_pop_list, model_ana_list, c(n1, n2max), alpha) - target_power,
                     error = function(e) NA_real_)
      if (!is.na(fa) && !is.na(fb) && ((fa <= 0 && fb >= 0) || (fa >= 0 && fb <= 0))) {
        valid_bounds <- TRUE; break
      }
      # If not bracketed, symmetrically expand once and retry
      n2min <- max(1L, floor(n2min / 2))
      n2max <- min(max_n2max, ceiling(n2max * expand_step))
      expand_attempts <- expand_attempts + 1L
    }

    # If still not bracketed, fall back to robust solver (which will self-bracket and handle NAs)
    if (!valid_bounds) {
      res <- find_n2_given_power(model_pop_list, model_ana_list, n1, n2min, n2max,
                                 alpha = alpha, target_power = target_power)
      if (!is.na(res$n2) && !is.na(res$power) && res$n2 >= 1 && res$power >= target_power) {
        last_successful_n2 <<- res$n2
        last_pairs <<- rbind(last_pairs, data.frame(n1 = n1, n2 = res$n2))
        if (nrow(last_pairs) > 2) last_pairs <<- last_pairs[(nrow(last_pairs)-1):nrow(last_pairs), , drop = FALSE]
        cost_used <- n1 * unit_costs[1] + res$n2 * unit_costs[2]
        return(data.frame(n1 = n1, n2 = res$n2, power = res$power, cost_used = cost_used))
      } else {
        return(data.frame(n1 = n1, n2 = NA, power = NA, cost_used = NA))
      }
    }

    # Valid bracket found -> call robust solver (includes left-trim)
    res <- find_n2_given_power(model_pop_list, model_ana_list, n1, n2min, n2max,
                               alpha = alpha, target_power = target_power)
    if (!is.na(res$n2) && !is.na(res$power) && res$n2 >= 1 && res$power >= target_power) {
      last_successful_n2 <<- res$n2
      last_pairs <<- rbind(last_pairs, data.frame(n1 = n1, n2 = res$n2))
      if (nrow(last_pairs) > 2) last_pairs <<- last_pairs[(nrow(last_pairs)-1):nrow(last_pairs), , drop = FALSE]
      cost_used <- n1 * unit_costs[1] + res$n2 * unit_costs[2]
      return(data.frame(n1 = n1, n2 = res$n2, power = res$power, cost_used = cost_used))
    }

    data.frame(n1 = n1, n2 = NA, power = NA, cost_used = NA)
  })

  all_df <- do.call(rbind, rows)
  best <- find_best_min_cost(all_df)
  list(all = all_df, best = best, warm_start_n2 = last_successful_n2)
}

# ============================================================
# Adaptive search (fixed budget): coarse→fine refinement of n1
# Purpose: sweep n1 with descending step sizes, reuse grid results,
#   and iteratively narrow the search window around the current best
#   max-power allocation subject to the budget constraint.
# ============================================================
adaptive_step_max_power <- function(initial_range, step_sizes,
                                    model_pop_list, model_ana_list,
                                    unit_costs = c(1, 1), budget = 1000,
                                    alpha = .05) {
  all_results <- data.frame(n1 = numeric(0), n2 = numeric(0), power = numeric(0), cost_used = numeric(0))
  n1_min <- initial_range[1]; n1_max <- initial_range[2]
  step_sizes <- sort(step_sizes, decreasing = TRUE)
  tried_n1 <- integer(0); best <- NULL

  for (step in step_sizes) {
    n1s <- setdiff(seq(n1_min, n1_max, by = step), tried_n1)
    if (!length(n1s)) next

    pb <- progress::progress_bar$new(total = length(n1s),
                                     format = sprintf(" adaptive max-power (step=%d) [:bar] :percent", step))
    tick_cb <- function() pb$tick()

    fixed_result <- grid_search_max_power(
      n1s, model_pop_list, model_ana_list, unit_costs, budget, alpha,
      show_progress = FALSE,
      progress_callback = tick_cb
    )

    all_results <- dplyr::bind_rows(all_results, fixed_result$all) |> dplyr::distinct(n1, .keep_all = TRUE)
    tried_n1 <- c(tried_n1, n1s)
    best <- fixed_result$best
    n1_min <- max(initial_range[1], best$n1 - step)
    n1_max <- min(initial_range[2], best$n1 + step)
  }

  best <- find_best_max_power(all_results)
  list(all = dplyr::arrange(all_results, n1), best = best)
}

# ============================================================
# Adaptive search (fixed power): coarse→fine refinement of n1
# Purpose: sweep n1 with descending step sizes, reuse warm-start n2 and
#   grid results to locate the min-cost allocation achieving target power.
# ============================================================
adaptive_step_min_cost <- function(
  model_pop_list, model_ana_list,
  unit_costs = c(1, 1), target_power = .80,
  alpha = .05, initial_range = c(100, 1000),
  step_sizes = c(50, 10, 1)
) {
  all_results <- data.frame(n1 = numeric(), n2 = numeric(), power = numeric(), cost_used = numeric())
  tried_n1 <- integer(0); best <- NULL
  n1_min <- initial_range[1]; n1_max <- initial_range[2]
  step_sizes <- sort(step_sizes, decreasing = TRUE)
  warm_n2 <- NULL

  for (step in step_sizes) {
    n1s <- setdiff(seq(n1_min, n1_max, by = step), tried_n1)
    if (!length(n1s)) next

    pb <- progress::progress_bar$new(total = length(n1s),
                                     format = sprintf(" adaptive min-cost (step=%d) [:bar] :percent", step))
    tick_cb <- function() pb$tick()

    fixed_result <- grid_search_min_cost(
      n1s = n1s,
      model_pop_list = model_pop_list,
      model_ana_list = model_ana_list,
      unit_costs = unit_costs,
      target_power = target_power,
      alpha = alpha,
      warm_start_n2 = warm_n2,
      show_progress = FALSE,
      progress_callback = tick_cb
    )

    all_results <- dplyr::bind_rows(all_results, fixed_result$all) |> dplyr::distinct(n1, .keep_all = TRUE)
    best <- fixed_result$best
    tried_n1 <- c(tried_n1, n1s)
    warm_n2 <- fixed_result$warm_start_n2

    n1_min <- max(initial_range[1], best$n1 - step)
    n1_max <- min(initial_range[2], best$n1 + step)
  }

  min_cost   <- min(all_results$cost_used, na.rm = TRUE)
  candidates <- all_results[all_results$cost_used == min_cost, , drop = FALSE]
  final_best <- if (!is.finite(min_cost)) all_results[1, , drop = FALSE] else candidates[which.max(candidates$power), , drop = FALSE]

  best <- find_best_min_cost(all_results)
  list(all = dplyr::arrange(all_results, n1), best = best)
}
