#' Plot the KMO trajectory across purification iterations
#'
#' For every construct in an \code{efa_trail} object, plots the overall
#' KMO/MSA value at each purification iteration, so the effect of iterative
#' item removal on sampling adequacy is visible at a glance. One panel is
#' drawn per construct, side by side.
#'
#' @param x An object of class \code{"efa_trail"}, as returned by
#'   \code{\link{efa_trail}}.
#' @param ... Further graphical parameters (currently unused).
#'
#' @return Invisibly returns \code{x}. Called for its plotting side effect.
#'
#' @examples
#' \dontrun{
#' result <- efa_trail(synthetic_reading, list(
#'   SA = grep("^SA", names(synthetic_reading), value = TRUE),
#'   CS = grep("^CS", names(synthetic_reading), value = TRUE)
#' ))
#' plot(result)
#' }
#' @export
plot.efa_trail <- function(x, ...) {
  if (!inherits(x, "efa_trail")) {
    stop("'x' must be an object returned by efa_trail().", call. = FALSE)
  }

  construct_names <- names(x)
  n_constructs <- length(construct_names)

  op <- graphics::par(mfrow = c(1, n_constructs), mar = c(4, 4, 3, 1))
  on.exit(graphics::par(op))

  for (cn in construct_names) {
    r <- x[[cn]]
    kmo <- vapply(r$iterations, function(it) {
      if (is.null(it$overall_kmo) || is.na(it$overall_kmo)) NA_real_ else it$overall_kmo
    }, numeric(1))
    iter <- seq_along(kmo)
    y_range <- range(kmo, na.rm = TRUE)
    y_pad <- max(0.01, diff(y_range) * 0.15)

    graphics::plot(
      iter, kmo, type = "b", pch = 19, lwd = 2, col = "#0F6E56",
      xlab = "Iteration", ylab = "Overall KMO",
      main = sprintf("%s: %d -> %d items (%d factor%s)",
                      cn, length(r$original_items), length(r$final_items),
                      r$n_factors_final, if (r$n_factors_final == 1) "" else "s"),
      ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
    )
    graphics::grid(col = "grey85", lty = "dotted")
  }

  invisible(x)
}
