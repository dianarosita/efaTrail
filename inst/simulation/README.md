# Recovery simulation

`run_simulation.R` generates constructs with a known ground-truth factor
structure (a known set of "true" items and "noise" items), runs
`efa_trail()` on each, and checks whether it recovers that structure
correctly (right factor count, right items retained). Default `n_reps`
is 1,000; results stabilize well before that (see below), so this is a
safe default that does not take excessively long to run.

## Data-adequacy criteria

Sample size for each replication is set proportionally to its number of
items (a 7-12:1 subject-to-item ratio, following Comrey & Lee, 1992),
with a floor of N = 150. Each replication's overall KMO is recorded.

## Authoritative results (run in R, this package, n = 921)

The manuscript reports results from an actual R run of this script
(stopped at n = 921 once checkpoints confirmed stability):

| n   | factor recovery | precision | recall |
|-----|-----------------|-----------|--------|
| 300 | 94.3%           | .9975     | .9481  |
| 600 | 95.0%           | .9984     | .9534  |
| 900 | 95.1%           | .9981     | .9535  |
| 921 (final) | 95.2%   | .9982     | .9541  |

At n = 921: 100% of replications met a minimum 5:1 subject-to-item
ratio, 99.7% achieved KMO >= .60 (mean KMO = .760), and 71.0% of
replications achieved perfect recovery of the true item set.

`simulation_results_reference.csv` holds results from an independent
Python cross-check (n = 2,000) run before finalizing the R version,
included for transparency only — the R results above are authoritative.

## Reproducing

```r
source("inst/simulation/run_simulation.R")
```

This prints checkpointed progress and a final summary, and writes
`simulation_results.csv`. All data here is entirely simulated — no real
respondent data, no licensing or consent restrictions.
