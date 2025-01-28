#Simulate process
set.seed(28753)
sims <- denguedat |>
  dplyr::filter(onset_week <= lubridate::ymd("1990-03-30"))


#File for testing that the nowcast function runs
test_that("Testing `nowcast.R`", {

  #Skip if on CRAN
  skip_on_cran()

  #UNSTRATIFIED
  #----------------------------------------------------------------------------------------
  #Check sampling inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, true_date = "onset_week", report_date = "report_week", chains = 1,
            cores = 1, iter = 1000, seed = 238479, method = "sampling")
    )
  })

  #Check variational inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, true_date = "onset_week", report_date = "report_week",
                   method = "variational")
    )
  })

  #Check optim inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, true_date = "onset_week", report_date = "report_week",
              method = "optimization")
    )
  })

  #STRATIFIED
  #----------------------------------------------------------------------------------------
  #Check sampling inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, true_date = "onset_week", report_date = "report_week", chains = 1,
                     cores = 1, iter = 25, strata = "gender", method = "sampling")
    )
  })

  #Check variational inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, true_date = "onset_week", report_date = "report_week",
                   method = "variational", strata = "gender")
    )
  })

  suppressWarnings({
    expect_no_error(
      nowcast(sims, true_date = "onset_week", report_date = "report_week",
              method = "optimization", strata = "gender")
    )
  })


})
