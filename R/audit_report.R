#' Generate an audit-trail report from an efa_trail object
#'
#' Produces a human-readable, publication-ready summary of the iterative
#' purification performed by \code{\link{efa_trail}} for each construct:
#' sample-adequacy statistics (KMO, Bartlett's test), the retained factor
#' count, and a step-by-step elimination log -- automating the kind of
#' appendix table researchers have traditionally written by hand.
#'
#' @param x An object of class \code{"efa_trail"}, as returned by
#'   \code{\link{efa_trail}}.
#' @param format One of \code{"text"} (default, console-friendly) or
#'   \code{"markdown"} (headings and bullet structure suitable for a
#'   manuscript supplement or R Markdown document).
#'
#' @return Invisibly returns a character vector of the report lines (one
#'   element per line). The report is also printed as a side effect.
#'
#' @examples
#' \dontrun{
#' result <- efa_trail(synthetic_reading, list(
#'   SA = grep("^SA", names(synthetic_reading), value = TRUE)
#' ))
#' audit_report(result)
#' audit_report(result, format = "markdown")
#' }
#' @export
audit_report <- function(x, format = c("text", "markdown")) {
  if (!inherits(x, "efa_trail")) {
    stop("'x' must be an object returned by efa_trail().", call. = FALSE)
  }
  format <- match.arg(format)
  lines <- character(0)
  h1 <- if (format == "markdown") "## " else ""
  h2 <- if (format == "markdown") "- " else "  "

  for (construct_name in names(x)) {
    r <- x[[construct_name]]

    lines <- c(lines, sprintf("%sConstruct: %s", h1, construct_name), "")
    lines <- c(lines, sprintf(
      "Started with %d items; retained %d items across %d factor(s) after %d iteration(s).",
      length(r$original_items), length(r$final_items),
      r$n_factors_final, length(r$iterations)
    ), "")

    for (it in r$iterations) {
      kmo_txt <- if (is.na(it$overall_kmo)) "NA" else sprintf("%.3f", it$overall_kmo)
      lines <- c(lines, sprintf(
        "%sIteration %d (%d items): overall MSA/KMO = %s, factors retained = %s",
        h2, it$iteration, it$n_items, kmo_txt, it$n_factors
      ))
      if (!is.na(it$removed_item)) {
        lines <- c(lines, sprintf(
          "    -> removed %s (%s)", it$removed_item, it$removal_reason
        ))
      } else if (!is.null(it$status) && it$status == "stable") {
        lines <- c(lines, "    -> no further problematic items; structure stable")
      } else if (!is.null(it$status) && it$status == "stopped") {
        lines <- c(lines, sprintf("    -> stopped: %s", it$removal_reason))
      }
    }

    if (length(r$removed_items) > 0) {
      lines <- c(lines, "", sprintf(
        "Items removed for %s: %s",
        construct_name, paste(r$removed_items, collapse = ", ")
      ))
    }
    lines <- c(lines, sprintf(
      "Final retained items for %s: %s",
      construct_name, paste(r$final_items, collapse = ", ")
    ), "")
  }

  cat(paste(lines, collapse = "\n"), "\n")
  invisible(lines)
}
