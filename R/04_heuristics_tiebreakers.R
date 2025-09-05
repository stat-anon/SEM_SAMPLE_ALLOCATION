############  Heuristics and tie-breakers  #######################################

# ============================================================
# Local bracket guess for n2 (fixed-power solver)
# Purpose: propose an integer bracket [a, b] around n2 via
#   recent (n1, n2) pairs—last value or a 2-point linear extrapolation—
#   then widen by a fraction and cap. Speeds subsequent bisection.
# ============================================================
guess_n2_bracket <- function(n1, last_pairs, widen_frac = 0.20,
                             min_halfwidth = 10L, cap = 1e6L) {
  # last_pairs: data.frame with columns n1, n2 (most recent at bottom)
  lp <- last_pairs
  lp <- lp[stats::complete.cases(lp$n1, lp$n2), c("n1","n2"), drop = FALSE]
  if (nrow(lp) == 0) return(NULL)

  # Start from most recent point
  n2_hat <- as.integer(lp$n2[nrow(lp)])

  # If two points available, do local linear extrapolation: n2 ~ a + b*n1
  if (nrow(lp) >= 2) {
    x1 <- lp$n1[nrow(lp) - 1L]; y1 <- lp$n2[nrow(lp) - 1L]
    x2 <- lp$n1[nrow(lp)];       y2 <- lp$n2[nrow(lp)]
    if (x2 != x1) {
      b <- (y2 - y1) / (x2 - x1)
      a <- y2 - b * x2
      n2_hat <- as.integer(round(a + b * n1))
    }
  }

  n2_hat <- max(1L, min(as.integer(cap), n2_hat))
  half   <- max(min_halfwidth, as.integer(ceiling(abs(n2_hat) * widen_frac)))
  a <- max(1L, n2_hat - half)
  b <- min(as.integer(cap), n2_hat + half)

  list(a = a, b = b, n2_hat = n2_hat)
}


# ============================================================
# Tie-breaker: select best row under fixed-power (min-cost) planning
# Rule: among valid rows, prefer lowest total cost, then highest power,
#   then earliest row (stable, deterministic). Ensures reproducible choice
#   when multiple allocations achieve target power.
# ============================================================
find_best_min_cost <- function(df) {
  if (nrow(df) == 0) return(df)
  df$.__rowid <- seq_len(nrow(df))  # remember original order

  ok <- is.finite(df$cost_used) & !is.na(df$power)
  if (!any(ok)) {
    out <- df[1, , drop = FALSE]
    return(within(out, rm(.__rowid)))
  }
  df2 <- df[ok, , drop = FALSE]

  ord <- with(df2, order(cost_used, -power, .__rowid))
  out <- df2[ord[1], , drop = FALSE]
  within(out, rm(.__rowid))
}

# ============================================================
# Tie-breaker: select best row under fixed-budget (max-power) planning
# Rule: among valid rows, prefer highest power, then lowest cost,
#   then earliest row (stable, deterministic). Ensures reproducible choice
#   when multiple allocations fit within the budget.
# ============================================================
find_best_max_power <- function(df) {
  if (nrow(df) == 0) return(df)
  df$.__rowid <- seq_len(nrow(df))

  ok <- !is.na(df$power) & is.finite(df$cost_used)
  if (!any(ok)) {
    out <- df[1, , drop = FALSE]
    return(within(out, rm(.__rowid)))
  }
  df2 <- df[ok, , drop = FALSE]

  ord <- with(df2, order(-power, cost_used, .__rowid))
  out <- df2[ord[1], , drop = FALSE]
  within(out, rm(.__rowid))
}

