# efaTrail

Sequential multi-construct exploratory factor analysis (EFA) with
automated audit-trail reporting, for R.

## Why

Developing a multidimensional instrument usually means running EFA
separately for *each* sub-scale (construct), removing weak items one at a
time, re-running, and writing up every iteration by hand — often as a long
supplementary appendix (KMO tables, Parallel Analysis results, an
elimination log per construct).

Existing tools purify a single item pool at a time:

- [`ItemRest`](https://cran.r-project.org/package=ItemRest) (R, CRAN) —
  automated item-removal strategies for one EFA at a time.
- [`efa_utils`](https://pypi.org/project/efa_utils/) (Python, PyPI) — an
  `iterative_efa()` function with the same one-pool scope.
- [`EFAtools`](https://cran.r-project.org/package=EFAtools) (R, CRAN) — a
  comprehensive EFA toolkit, also for a single factor structure.

`efaTrail` sits one layer up: it takes the **whole instrument** (every
construct at once), loops the purification construct-by-construct, and
turns the process into a structured, reproducible **audit trail** —
automating the appendix, not just the item removal.

## Installation

```r
# install.packages("remotes")
remotes::install_github("trizahra10019-spec/efaTrail")
```

Requires the [`psych`](https://cran.r-project.org/package=psych) package
(installed automatically as a dependency).

## Quick example

```r
library(efaTrail)

# a data.frame with one column per item, item names prefixed by construct
# (see data-raw/generate_synthetic.R for how the bundled example was made)
synthetic_reading <- read.csv(
  system.file("extdata", "synthetic_reading.csv", package = "efaTrail")
)

constructs <- list(
  SA = grep("^SA", names(synthetic_reading), value = TRUE),
  CS = grep("^CS", names(synthetic_reading), value = TRUE)
)

result <- efa_trail(synthetic_reading, constructs)
print(result)
#> Construct SA: 23 -> 11 items retained (2 factor(s), 12 removed, 13 iteration(s))
#> Construct CS: 9 -> 3 items retained (1 factor(s), 6 removed, 7 iteration(s))

audit_report(result)               # console-friendly text report
audit_report(result, format = "markdown")  # for a manuscript supplement
```

## About the example data

`synthetic_reading` is **entirely simulated** — see
[`data-raw/generate_synthetic.R`](data-raw/generate_synthetic.R) for the
generating code. It mirrors the two-factor structure and the exact set of
weak items documented in the authors' own manual item-purification record
for a real reading-strategy instrument, but contains no real respondent
data: participants in the original study were told their individual
responses would not be published, and this package honors that.

## Citation

If you use `efaTrail`, please also see the related package
[`condfair`](https://github.com/trizahra10019-spec/condfair) (ability-conditioned
fairness metrics for automated scoring), published in *Applied
Psychological Measurement*.

## License

MIT — see [LICENSE.md](LICENSE.md).
