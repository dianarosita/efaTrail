#' Sequential multi-construct exploratory factor analysis with audit trail
#'
#' Runs exploratory factor analysis (EFA) separately for each named construct
#' (sub-scale) of a multidimensional instrument, iteratively removing items
#' with a low factor loading, a strong cross-loading, or high uniqueness, and
#' records every iteration so the process can later be turned into a
#' publication-ready audit-trail report with \code{\link{audit_report}}.
#'
#' Existing EFA tools (e.g. the \pkg{psych} or \pkg{EFAtools} packages, or the
#' \pkg{ItemRest} package) purify a single item pool at a time and leave the
#' researcher to repeat the process by hand for every sub-scale of a
#' multidimensional instrument, and to write up each iteration manually.
#' \code{efa_trail()} instead takes the whole instrument at once, loops the
#' purification construct-by-construct, and keeps a structured log of every
#' decision -- the same information researchers have traditionally compiled
#' by hand as a supplementary appendix.
#'
#' @param data A data.frame or matrix of item responses, one column per item.
#' @param constructs A named list; each element is a character vector of
#'   column names in \code{data} that belong to that construct.
#' @param min_loading Minimum acceptable primary factor loading
#'   (default \code{0.50}).
#' @param max_uniqueness Maximum acceptable uniqueness (default \code{0.70}).
#' @param cross_loading_gap Minimum required gap between an item's primary
#'   and secondary loading (default \code{0.10}). Items with a smaller gap
#'   are flagged as cross-loading.
#' @param fm Factor extraction method passed to \code{psych::fa()}
#'   (default \code{"ml"}, maximum likelihood).
#' @param rotate Rotation method passed to \code{psych::fa()}
#'   (default \code{"varimax"}).
#' @param max_iter Maximum number of purification iterations per construct
#'   (default \code{20}).
#' @param min_items Minimum number of items to retain per construct;
#'   purification stops before going below this floor (default \code{3}).
#'
#' @return An object of class \code{"efa_trail"}: a named list with one
#'   element per construct. Each element contains \code{original_items},
#'   \code{final_items}, \code{removed_items}, \code{n_factors_final}, the
#'   full \code{iterations} log, and the final \code{fa_model}
#'   (a \code{psych::fa} object).
#'
#' @examples
#' \dontrun{
#' data(synthetic_reading)
#' constructs <- list(
#'   SA = grep("^SA", names(synthetic_reading), value = TRUE),
#'   CS = grep("^CS", names(synthetic_reading), value = TRUE)
#' )
#' result <- efa_trail(synthetic_reading, constructs)
#' print(result)
#' audit_report(result)
#' }
#'
#' @seealso \code{\link{audit_report}}
#' @export
efa_trail <- function(data,
                       constructs,
                       min_loading = 0.50,
                       max_uniqueness = 0.70,
                       cross_loading_gap = 0.10,
                       fm = "ml",
                       rotate = "varimax",
                       max_iter = 20,
                       min_items = 3) {

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required. Install it with install.packages('psych').",
         call. = FALSE)
  }
  if (!is.list(constructs) || is.null(names(constructs)) ||
      any(names(constructs) == "")) {
    stop("'constructs' must be a named list of item-name character vectors.",
         call. = FALSE)
  }

  results <- list()

  for (construct_name in names(constructs)) {

    items <- constructs[[construct_name]]
    missing_items <- setdiff(items, names(data))
    if (length(missing_items) > 0) {
      stop(sprintf(
        "Construct '%s' references columns not found in data: %s",
        construct_name, paste(missing_items, collapse = ", ")
      ), call. = FALSE)
    }

    current_items <- items
    iteration_log <- list()
    iter <- 0
    removed_items <- character(0)
    fa_res <- NULL

    repeat {
      iter <- iter + 1
      construct_data <- data[, current_items, drop = FALSE]

      kmo_res <- tryCatch(psych::KMO(construct_data), error = function(e) NULL)
      bartlett_res <- tryCatch(
        psych::cortest.bartlett(
          stats::cor(construct_data, use = "pairwise.complete.obs"),
          n = nrow(construct_data)
        ),
        error = function(e) NULL
      )

      n_factors <- tryCatch({
        pa <- utils::capture.output(
          pa_res <- psych::fa.parallel(construct_data, fm = fm, fa = "fa",
                                        plot = FALSE, n.iter = 20)
        )
        max(1, pa_res$nfact)
      }, error = function(e) {
        warning(sprintf(
          "fa.parallel() failed for construct '%s' at %d items (%s); defaulting to 1 factor.",
          construct_name, length(current_items), conditionMessage(e)
        ), call. = FALSE)
        1
      })

      fa_res <- tryCatch(
        psych::fa(construct_data, nfactors = n_factors, fm = fm, rotate = rotate),
        error = function(e) NULL
      )

      if (is.null(fa_res)) {
        iteration_log[[iter]] <- list(
          iteration = iter,
          n_items = length(current_items),
          n_factors = n_factors,
          overall_kmo = if (!is.null(kmo_res)) kmo_res$MSA else NA,
          bartlett_p = if (!is.null(bartlett_res)) bartlett_res$p.value else NA,
          loadings_table = NULL,
          removed_item = NA_character_,
          removal_reason = "EFA failed to converge; purification stopped.",
          status = "stopped"
        )
        break
      }

      loadings_mat <- unclass(fa_res$loadings)
      uniqueness <- fa_res$uniquenesses

      primary <- apply(abs(loadings_mat), 1, max)
      if (ncol(loadings_mat) >= 2) {
        # NOTE: apply() must not be allowed to auto-simplify here -- when
        # ncol(loadings_mat) == 1 it silently returns a transposed vector
        # instead of a matrix, which previously produced bogus "secondary"
        # loadings and made every item look cross-loaded. Guarding on
        # ncol >= 2 and defaulting to 0 otherwise avoids that entirely.
        secondary <- apply(abs(loadings_mat), 1, function(x) sort(x, decreasing = TRUE)[2])
      } else {
        secondary <- rep(0, nrow(loadings_mat))
      }
      gap <- primary - secondary

      flagged <- data.frame(
        item = current_items,
        primary_loading = round(as.numeric(primary), 3),
        gap_to_secondary = round(as.numeric(gap), 3),
        uniqueness = round(as.numeric(uniqueness[current_items]), 3),
        row.names = NULL,
        stringsAsFactors = FALSE
      )

      flagged$reason <- NA_character_
      flagged$reason[flagged$primary_loading < min_loading] <- "low loading"
      flagged$reason[is.na(flagged$reason) &
                        flagged$gap_to_secondary < cross_loading_gap] <- "cross-loading"
      flagged$reason[is.na(flagged$reason) &
                        flagged$uniqueness > max_uniqueness] <- "high uniqueness"

      problem <- flagged[!is.na(flagged$reason), ]

      iteration_log[[iter]] <- list(
        iteration = iter,
        n_items = length(current_items),
        n_factors = n_factors,
        overall_kmo = if (!is.null(kmo_res)) kmo_res$MSA else NA,
        bartlett_p = if (!is.null(bartlett_res)) bartlett_res$p.value else NA,
        loadings_table = flagged,
        removed_item = NA_character_,
        removal_reason = NA_character_,
        status = "checked"
      )

      if (nrow(problem) == 0 || length(current_items) <= min_items || iter >= max_iter) {
        iteration_log[[iter]]$status <- "stable"
        break
      }

      worst <- problem[order(problem$primary_loading), ][1, ]
      current_items <- setdiff(current_items, worst$item)
      removed_items <- c(removed_items, worst$item)
      iteration_log[[iter]]$removed_item <- worst$item
      iteration_log[[iter]]$removal_reason <- worst$reason
    }

    results[[construct_name]] <- list(
      construct = construct_name,
      original_items = items,
      final_items = current_items,
      removed_items = removed_items,
      n_factors_final = iteration_log[[length(iteration_log)]]$n_factors,
      iterations = iteration_log,
      fa_model = fa_res
    )
  }

  class(results) <- "efa_trail"
  results
}

#' @export
print.efa_trail <- function(x, ...) {
  for (nm in names(x)) {
    r <- x[[nm]]
    cat(sprintf(
      "Construct %s: %d -> %d items retained (%d factor(s), %d removed, %d iteration(s))\n",
      nm, length(r$original_items), length(r$final_items),
      r$n_factors_final, length(r$removed_items), length(r$iterations)
    ))
  }
  invisible(x)
}
