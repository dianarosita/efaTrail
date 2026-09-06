#' Synthetic reading-strategy item responses
#'
#' A simulated Likert-scale (1-6) data set with 200 respondents used to
#' demonstrate \code{\link{efa_trail}} and \code{\link{audit_report}}. It
#' mirrors the two-factor structure -- and the exact set of weak items --
#' documented in the authors' own manual item-purification record for a
#' real reading-strategy instrument, but contains no real respondent data.
#' See \code{data-raw/generate_synthetic.R} for the generating code and
#' the package README for why real data is not distributed.
#'
#' @format A data frame with 200 rows and 32 item columns, all on a 1-6
#'   Likert scale. Columns prefixed \code{SA} (\code{SA1}-\code{SA23})
#'   belong to the "Schema Activation" construct; items \code{SA7},
#'   \code{SA9}, \code{SA10}, \code{SA11}, \code{SA13}, \code{SA14},
#'   \code{SA18}, \code{SA19}, \code{SA20}, \code{SA21}, and \code{SA23}
#'   carry a genuine two-factor signal, while the remaining \code{SA}
#'   items are simulated noise, included so \code{efa_trail()} has
#'   something to purify. Columns prefixed \code{CS} belong to a
#'   simpler, illustrative "Cognitive Strategies" construct (2 factors),
#'   included only to demonstrate multi-construct orchestration -- it
#'   does not replicate the full 3-factor, 25-item purification of the
#'   real Cognitive Strategies sub-scale.
#' @source Simulated. See \code{data-raw/generate_synthetic.R}.
#' @name synthetic_reading
#' @docType data
#' @keywords datasets
NULL
