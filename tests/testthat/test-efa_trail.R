test_that("efa_trail() purifies the synthetic SA construct as expected", {
  skip_if_not_installed("psych")

  data_path <- system.file("extdata", "synthetic_reading.csv", package = "efaTrail")
  if (data_path == "") {
    data_path <- file.path("..", "..", "inst", "extdata", "synthetic_reading.csv")
  }
  skip_if(!file.exists(data_path), "synthetic_reading.csv not found")

  synthetic_reading <- utils::read.csv(data_path)

  constructs <- list(
    SA = grep("^SA", names(synthetic_reading), value = TRUE)
  )

  result <- efa_trail(synthetic_reading, constructs, min_items = 3)

  expect_s3_class(result, "efa_trail")
  expect_true("SA" %in% names(result))
  expect_true(length(result$SA$final_items) < length(result$SA$original_items))
  expect_true(length(result$SA$final_items) >= 3)
  expect_equal(
    length(result$SA$final_items) + length(result$SA$removed_items),
    length(result$SA$original_items)
  )
})

test_that("efa_trail() handles multiple constructs independently", {
  skip_if_not_installed("psych")

  data_path <- system.file("extdata", "synthetic_reading.csv", package = "efaTrail")
  if (data_path == "") {
    data_path <- file.path("..", "..", "inst", "extdata", "synthetic_reading.csv")
  }
  skip_if(!file.exists(data_path), "synthetic_reading.csv not found")

  synthetic_reading <- utils::read.csv(data_path)

  constructs <- list(
    SA = grep("^SA", names(synthetic_reading), value = TRUE),
    CS = grep("^CS", names(synthetic_reading), value = TRUE)
  )

  result <- efa_trail(synthetic_reading, constructs, min_items = 2)

  expect_setequal(names(result), c("SA", "CS"))
  expect_true(all(vapply(result, function(r) length(r$final_items) > 0, logical(1))))
})

test_that("efa_trail() rejects an unnamed constructs list", {
  df <- as.data.frame(matrix(rnorm(100), ncol = 10))
  names(df) <- paste0("X", 1:10)
  expect_error(efa_trail(df, list(paste0("X", 1:10))), "named list")
})

test_that("efa_trail() rejects columns missing from data", {
  df <- as.data.frame(matrix(rnorm(50), ncol = 5))
  names(df) <- paste0("X", 1:5)
  expect_error(
    efa_trail(df, list(F1 = c("X1", "X2", "NOT_A_COLUMN"))),
    "not found in data"
  )
})

test_that("audit_report() requires an efa_trail object", {
  expect_error(audit_report(list(a = 1)), "efa_trail")
})
