test_that("Checking `check_now`", {
  data <- data.frame(onset_date = seq(as.Date("1990/01/01"), as.Date("1990/01/05"), by = "1 day"))

  # Shouldn't work with a number or a date
  expect_error(check_now(.disease_data = data, now = "hello", onset_date = "onset_date"))
  expect_error(check_now(.disease_data = data, now = 2.1, onset_date = "onset_date"))

  # Shouldn't work for now before data or after data
  expect_error(check_now(.disease_data = data, now = as.Date("1989/12/31"), onset_date = "onset_date"))
  expect_error(check_now(.disease_data = data, now = as.Date("1990/01/06"), onset_date = "onset_date"))

  # Should work with data in range
  expect_no_warning(check_now(.disease_data = data, now = as.Date("1990/01/04"), onset_date = "onset_date"))

  # Should work with null
  expect_no_warning(check_now(.disease_data = data, now = NULL, onset_date = "onset_date"))
})
