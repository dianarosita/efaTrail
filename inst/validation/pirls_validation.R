# inst/validation/pirls_validation.R
#
# Real-world, large-scale validation of efa_trail() on the PIRLS 2011
# Indonesia student background questionnaire (N = 4,791 fourth-grade
# students). Demonstrates the package's multi-construct orchestration on
# three of PIRLS's officially defined reading-attitude scales at once.
#
# IMPORTANT: the PIRLS 2011 International Database is NOT redistributed
# with this package. Obtain it yourself from:
#   https://timssandpirls.bc.edu/pirls2011/international-database.html
# under IEA's non-commercial research-use terms, and adjust the file path
# below to wherever you extracted the Indonesia student questionnaire file
# (asgidnr3.sav, inside P11_SPSSData_pt2.zip).
#
# Usage (after obtaining the data yourself):
#   source("inst/validation/pirls_validation.R")

if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Install 'haven' first: install.packages('haven')")
}
if (!requireNamespace("efaTrail", quietly = TRUE)) {
  stop("Install/load efaTrail first: install.packages('.', repos = NULL, type = 'source')")
}
library(haven)
library(efaTrail)

# --- EDIT this path to your local copy of the PIRLS 2011 Indonesia file ---
pirls_path <- "asgidnr3.sav"

pirls <- read_sav(pirls_path)

# Reverse-code negatively worded items (scale is 1-4; reverse = 5 - score)
reverse_items <- c("ASBR07A", "ASBR07D", "ASBR08C", "ASBR08E", "ASBR08G", "ASBR05D")
pirls[reverse_items] <- lapply(pirls[reverse_items], function(x) 5 - as.numeric(x))

constructs_pirls <- list(
  LikeReading      = c("ASBR07A", "ASBR07B", "ASBR07C", "ASBR07D", "ASBR07E", "ASBR07F"),
  ConfidentReading = c("ASBR08A", "ASBR08B", "ASBR08C", "ASBR08D", "ASBR08E", "ASBR08F", "ASBR08G"),
  EngagedReading   = c("ASBR05A", "ASBR05B", "ASBR05C", "ASBR05D", "ASBR05E", "ASBR05F", "ASBR05G")
)

for (v in unlist(constructs_pirls)) {
  pirls[[v]] <- as.numeric(pirls[[v]])
}

result_pirls <- efa_trail(pirls, constructs_pirls)
print(result_pirls)
audit_report(result_pirls)
plot(result_pirls)
