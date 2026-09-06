# data-raw/generate_synthetic.R
#
# Generates the synthetic Likert-scale dataset bundled with efaTrail as
# `synthetic_reading`.
#
# IMPORTANT -- why this data is simulated, not real:
# The package was motivated by a real multidimensional reading-strategy
# instrument the authors developed and purified by hand (documented in an
# unpublished item-purification appendix). Survey respondents in that study
# were told, as part of informed consent, that their individual responses
# would NOT be published. To respect that commitment, no real respondent
# data is included anywhere in this package or its repository. Instead,
# this script simulates a dataset with the SAME two-factor loading pattern
# and the SAME items that the authors' own manual audit trail identified as
# weak -- so running efa_trail() on `synthetic_reading` reproduces the
# authors' documented result (23 -> 11 items retained for the "SA"
# construct) without exposing a single real response.
#
# The second construct ("CS") is a simpler illustrative 2-factor construct
# included only to demonstrate multi-construct orchestration; it does not
# attempt to replicate the full 3-factor, 25-item purification the authors
# carried out on the real Cognitive Strategies sub-scale.

set.seed(2026)
n <- 200

simulate_construct <- function(n, strong_items, weak_items, factor_map,
                                loadings_strong = c(0.55, 0.85),
                                loadings_weak = c(0.15, 0.35)) {
  n_factors <- length(unique(factor_map[strong_items]))
  # NOTE: factors are generated independently (no shared/general-factor term).
  # An earlier version added a shared component across factors, which made
  # the two engineered SA factors correlated enough that Parallel Analysis
  # (paired with the package's orthogonal varimax rotation) sometimes
  # detected only 1 factor instead of 2. Keeping factors orthogonal here
  # matches the orthogonal rotation efa_trail() uses by default.
  factors <- matrix(stats::rnorm(n * n_factors), nrow = n)

  all_items <- c(strong_items, weak_items)
  out <- matrix(NA_real_, nrow = n, ncol = length(all_items),
                dimnames = list(NULL, all_items))

  for (it in strong_items) {
    f <- factor_map[[it]]
    load <- stats::runif(1, loadings_strong[1], loadings_strong[2])
    noise <- stats::rnorm(n, sd = sqrt(1 - load^2))
    out[, it] <- load * factors[, f] + noise
  }
  for (it in weak_items) {
    f <- sample(seq_len(n_factors), 1)
    load <- stats::runif(1, loadings_weak[1], loadings_weak[2])
    noise <- stats::rnorm(n, sd = sqrt(1 - load^2))
    out[, it] <- load * factors[, f] + stats::rnorm(n, sd = 0.9)
  }

  out <- apply(out, 2, function(col) {
    z <- (col - mean(col)) / stats::sd(col)
    lik <- round(pmin(6, pmax(1, 3.5 + 1.3 * z)))
    as.integer(lik)
  })
  as.data.frame(out)
}

# --- Schema Activation (SA): mirrors the real 23-item pool and 2-factor
#     structure, including the exact 11 items that survived purification
#     in the authors' documented audit trail.
sa_factor1_strong <- c("SA7", "SA13", "SA14", "SA18", "SA19", "SA20", "SA21", "SA23")
sa_factor2_strong <- c("SA9", "SA10", "SA11")
sa_weak <- c("SA1", "SA2", "SA3", "SA4", "SA5", "SA6",
             "SA8", "SA12", "SA15", "SA16", "SA17", "SA22")

sa_factor_map <- c(
  stats::setNames(rep(1, length(sa_factor1_strong)), sa_factor1_strong),
  stats::setNames(rep(2, length(sa_factor2_strong)), sa_factor2_strong)
)

sa_data <- simulate_construct(
  n,
  strong_items = c(sa_factor1_strong, sa_factor2_strong),
  weak_items = sa_weak,
  factor_map = sa_factor_map,
  loadings_strong = c(0.65, 0.85)  # narrowed from c(0.55, 0.85): at the
  # wider range, a "strong" item could occasionally draw a loading near
  # 0.55, which sometimes fell below the uniqueness/loading thresholds
  # after several purification rounds and was removed along with the
  # genuinely weak items. Narrowing keeps every strong item comfortably
  # clear of the thresholds so the 23 -> 11 result is reproducible.
)
sa_data <- sa_data[, paste0("SA", 1:23)]

# --- Cognitive Strategies (CS): simpler illustrative 2-factor construct,
#     included to demonstrate multi-construct orchestration (see note above).
cs_factor1_strong <- c("CS24", "CS25", "CS26")
cs_factor2_strong <- c("CS36", "CS37")
cs_weak <- c("CS27", "CS28", "CS29", "CS30")

cs_factor_map <- c(
  stats::setNames(rep(1, length(cs_factor1_strong)), cs_factor1_strong),
  stats::setNames(rep(2, length(cs_factor2_strong)), cs_factor2_strong)
)

cs_data <- simulate_construct(
  n,
  strong_items = c(cs_factor1_strong, cs_factor2_strong),
  weak_items = cs_weak,
  factor_map = cs_factor_map
)
cs_data <- cs_data[, c("CS24", "CS25", "CS26", "CS27", "CS28", "CS29", "CS30",
                        "CS36", "CS37")]

synthetic_reading <- cbind(sa_data, cs_data)

usethis::use_data(synthetic_reading, overwrite = TRUE)
write.csv(synthetic_reading, "inst/extdata/synthetic_reading.csv", row.names = FALSE)
