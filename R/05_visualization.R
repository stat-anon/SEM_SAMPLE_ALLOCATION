############  Visualization  #######################################

# ============================================================
# Visualization: allocation diagnostics for planning modes
# Purpose: given search results (res$all, res$best), render compact
#   diagnostics for fixed-power or fixed-budget planning:
#   (optional) n2 vs n1, power vs n1, and total cost vs n1,
#   auto-detecting mode when set to "auto" and highlighting the selected design.
# ============================================================
plot_allocation_diagnostics <- function(
  res,
  mode = c("auto", "fixed_power", "fixed_budget"),
  target_power = NULL,
  budget = NULL,
  tol_power = 1e-3,
  label_best = TRUE,
  title = NULL,
  show_band = FALSE,
  band_alpha = 0.06,
  band_fill  = "grey85",
  include_n2_panel = FALSE,   # add n2 vs n1 panel at the top
  y_n2_lim    = NULL,         # e.g., c(0, 1800)
  y_power_lim = NULL,         # e.g., c(.7995, .8005)
  y_cost_lim  = NULL          # e.g., c(600, 2200)
) {
  stopifnot(is.list(res), !is.null(res$all))
  mode <- match.arg(mode)

  df <- res$all
  df <- df[stats::complete.cases(df[, c("n1","power","cost_used")]), , drop = FALSE]
  df$n1 <- as.integer(df$n1)
  df <- df[order(df$n1), , drop = FALSE]

  # auto-detect mode
  if (mode == "auto") {
    uniq_cost <- unique(df$cost_used)
    if (length(uniq_cost) == 1L) {
      mode <- "fixed_budget"
      budget <- if (is.null(budget)) uniq_cost[1] else budget
    } else {
      mode <- "fixed_power"
      if (is.null(target_power)) target_power <- stats::median(df$power, na.rm = TRUE)
    }
  }
  if (mode == "fixed_budget" && is.null(budget)) {
    uc <- unique(df$cost_used); budget <- if (length(uc)) uc[1] else NA_real_
  }
  if (mode == "fixed_power" && is.null(target_power)) {
    target_power <- stats::median(df$power, na.rm = TRUE)
  }

  # best row
  if (!is.null(res$best) && all(c("n1","power","cost_used") %in% names(res$best))) {
    best <- res$best[1, intersect(c("n1","power","cost_used","n2"), names(res$best)), drop = FALSE]
  } else {
    ridx <- if (mode == "fixed_budget") which.max(df$power) else which.min(df$cost_used)
    best <- df[ridx, intersect(c("n1","power","cost_used","n2"), names(df)), drop = FALSE]
  }

  # (A) n2 vs n1 (optional)
  p_n2 <- NULL
  if (include_n2_panel && "n2" %in% names(df)) {
    p_n2 <- ggplot2::ggplot(df, ggplot2::aes(n1, n2)) +
      ggplot2::geom_step(direction = "hv") +
      { if (label_best && "n2" %in% names(best)) ggplot2::geom_point(data = best, ggplot2::aes(n1, n2), size = 2) else NULL } +
      ggplot2::labs(x = "n1", y = "n2") +
      ggplot2::theme_minimal(base_size = 11)
    if (!is.null(y_n2_lim)) p_n2 <- p_n2 + ggplot2::coord_cartesian(ylim = y_n2_lim)
  }

  # (B) power vs n1
  p_power <- ggplot2::ggplot(df, ggplot2::aes(n1, power)) +
    ggplot2::geom_line()
  if (mode == "fixed_power") {
    if (show_band) {
      p_power <- p_power +
        ggplot2::annotate("rect",
          xmin = -Inf, xmax = Inf,
          ymin = target_power - tol_power, ymax = target_power + tol_power,
          alpha = band_alpha, fill = band_fill
        )
    }
    p_power <- p_power + ggplot2::geom_hline(yintercept = target_power, linetype = 2)
  }
  if (label_best) p_power <- p_power + ggplot2::geom_point(data = best, ggplot2::aes(n1, power), size = 2)
  p_power <- p_power + ggplot2::labs(x = "n1", y = "Power") + ggplot2::theme_minimal(base_size = 11)
  if (!is.null(y_power_lim)) p_power <- p_power + ggplot2::coord_cartesian(ylim = y_power_lim)

  # (C) cost vs n1
  p_cost <- ggplot2::ggplot(df, ggplot2::aes(n1, cost_used)) +
    ggplot2::geom_line() +
    { if (mode == "fixed_budget") ggplot2::geom_hline(yintercept = budget, linetype = 2) else NULL } +
    { if (label_best) ggplot2::geom_point(data = best, ggplot2::aes(n1, cost_used), size = 2) else NULL } +
    ggplot2::labs(x = "n1", y = "Total cost") +
    ggplot2::theme_minimal(base_size = 11)
  if (!is.null(y_cost_lim)) p_cost <- p_cost + ggplot2::coord_cartesian(ylim = y_cost_lim)

  # Title placement
  if (is.null(title)) {
    title <- if (mode == "fixed_budget") "Diagnostics (fixed budget)" else "Diagnostics (fixed power)"
  }
  if (!is.null(p_n2)) p_n2 <- p_n2 + ggplot2::ggtitle(title) else p_power <- p_power + ggplot2::ggtitle(title)

  # Assemble
  if (include_n2_panel && !is.null(p_n2)) {
    p_n2 + p_power + p_cost + patchwork::plot_layout(ncol = 1, heights = c(1,1,1))
  } else {
    p_power + p_cost + patchwork::plot_layout(ncol = 1, heights = c(1,1))
  }
}

