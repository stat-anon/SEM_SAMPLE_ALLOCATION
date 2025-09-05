############  Constraint solvers  #######################################

# ============================================================
# Fixed-power solver: minimal n2 for a given n1 (target power)
# Purpose: solve for the smallest integer n2 such that power(n1, n2) ≥ target,
#   using robust auto-bracketing and integer bisection; then left-trim to minimal n2.
# Notes: f(n2) = power − target is monotone in n2; includes NA-safe evaluations and caps.
# ============================================================
find_n2_given_power <- function(model_pop_list, model_ana_list, n1,
                                n2min, n2max,
                                alpha = .05, target_power = .80,
                                max_iter = 1000,
                                c_up = 1.5, c_down = 1.5,
                                max_expand = 20,
                                max_n2_cap = 1e6) {
  f_raw <- function(n2) {
    p <- power_MGSEM(model_pop_list, model_ana_list, n = c(n1, n2), alpha)
    if (is.na(p)) return(NA_real_)
    p - target_power
  }

  # Safe evaluation: if NA, try small integer nudges inward
  f_safe <- function(n2, a, b) {
    val <- f_raw(n2)
    if (!is.na(val)) return(val)
    for (step in c(1, -1, 2, -2, 3, -3, 5, -5)) {
      n_try <- n2 + step
      if (n_try > a && n_try < b) {
        val2 <- f_raw(n_try)
        if (!is.na(val2)) return(val2)
      }
    }
    NA_real_
  }

  a0 <- max(1L, as.integer(n2min))
  b0 <- max(a0 + 1L, as.integer(n2max))
  a <- a0; b <- b0
  # Cap the right bound to prevent runaway expansion
  b_cap <- as.integer(min(max_n2_cap, max(b0 * 100, 1e5)))

  fa <- f_safe(a, a, b)
  fb <- f_safe(b, a, b)

  # Target bracketing: fa <= 0 <= fb (since f = power - target, monotone in n2)
  expand_count <- 0L
  while ((is.na(fa) || is.na(fb) || !(fa <= 0 && fb >= 0)) && expand_count < max_expand) {
    expand_count <- expand_count + 1L

    # Adjust only the side that needs it
    if (is.na(fa) || fa > 0) {
      new_a <- max(1L, floor(a / c_down))
      if (new_a == a) new_a <- max(1L, a - 1L)
      a <- new_a
    }
    if (is.na(fb) || fb < 0) {
      new_b <- ceiling(b * c_up)
      if (new_b <= b) new_b <- b + 1L
      b <- min(new_b, b_cap)
    }

    fa <- f_safe(a, a, b)
    fb <- f_safe(b, a, b)

    if (b >= b_cap && (is.na(fa) || is.na(fb) || !(fa <= 0 && fb >= 0))) break
  }

  if (is.na(fa) || is.na(fb) || !(fa <= 0 && fb >= 0)) {
    return(list(n2 = NA_integer_, power = NA_real_, status = "no_bracket"))
  }

  # Bisection
  iter <- 0L
  while ((b - a) > 1L && iter < max_iter) {
    iter <- iter + 1L
    mid  <- floor((a + b) / 2L)
    fmid <- f_safe(mid, a, b)

    if (is.na(fmid)) {
      # If mid is NA, move the bracket using the available endpoint sign
      if (!is.na(fb) && fb >= 0) {
        b <- mid; fb <- f_safe(b, a, b)
      } else if (!is.na(fa) && fa <= 0) {
        a <- mid; fa <- f_safe(a, a, b)
      } else {
        break
      }
      next
    }

    if (fmid >= 0) { b <- mid; fb <- fmid } else { a <- mid; fa <- fmid }
  }

  # Take right end (guaranteed feasible), then left-trim to the minimal n2 still meeting the target
  final_n2 <- as.integer(b)
  tol <- 1e-8
  repeat {
    if (final_n2 <= 1L) break
    p_prev <- power_MGSEM(model_pop_list, model_ana_list, c(n1, final_n2 - 1L), alpha)
    if (is.na(p_prev)) break
    if (p_prev >= target_power - tol) {
      final_n2 <- final_n2 - 1L
    } else break
  }

  final_power <- power_MGSEM(model_pop_list, model_ana_list, c(n1, final_n2), alpha)
  list(n2 = final_n2, power = final_power, status = "ok", expands = expand_count, iters = iter)
}

# ============================================================
# Fixed-budget constraint: derive n2 and used cost for a given n1
# Purpose: enforce unit_costs · (n1, n2) ≤ budget via a closed-form
#   linear relation; returns integer n2 and the corresponding total cost.
# ============================================================
find_n2_given_budget <- function(n1, unit_costs = c(1, 1), budget) {
  stopifnot(length(unit_costs) == 2, all(unit_costs > 0), n1 >= 0)
  n2 <- floor((budget - n1 * unit_costs[1]) / unit_costs[2])
  if (n2 < 0) return(list(n2 = NA_integer_, cost_used = NA_real_))
  list(n2 = n2, cost_used = n1 * unit_costs[1] + n2 * unit_costs[2])
}
