test_that("Checking `check_now`", {
  data <- data.frame(true_date = seq(as.Date("1990/01/01"), as.Date("1990/01/05"), by = "1 day"),
                     report_date = seq(as.Date("1990/01/01"), as.Date("1990/01/05"), by = "1 day"))

  # Shouldn't work with a number or a date
  expect_error(check_now(.disease_data = data, now = "hello", true_date = "true_date", report_date = "report_date"))
  expect_error(check_now(.disease_data = data, now = 2.1, true_date = "true_date", report_date = "report_date"))

  # Shouldn't work for now before data or after data
  expect_error(check_now(.disease_data = data, now = as.Date("1989/12/31"), true_date = "true_date", report_date = "report_date"))
  expect_error(check_now(.disease_data = data, now = as.Date("1990/01/06"), true_date = "true_date", report_date = "report_date"))

  # Should work with data in range
  expect_no_warning(check_now(.disease_data = data, now = as.Date("1990/01/04"), true_date = "true_date", report_date = "report_date"))

  # Should work with null
  expect_no_warning(check_now(.disease_data = data, now = NULL, true_date = "true_date", report_date = "report_date"))
})
