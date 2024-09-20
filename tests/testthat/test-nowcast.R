#Simulate process
set.seed(28753)
sims <- simulate_process_for_testing(num_steps = 10, num_delays = 4, num_strata = 3)


#File for testing that the nowcast function runs
test_that("Testing `nowcast.R`", {

  #Skip if on CRAN
  skip_on_cran()

  #UNSTRATIFIED
  #----------------------------------------------------------------------------------------
  #Check sampling inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, onset_date = "onset_date", report_date = "report_date", chains = 1,
            cores = 1, iter = 25)
    )
  })

  #Check variational inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, onset_date = "onset_date", report_date = "report_date",
                   method = "variational")
    )
  })

  #Check optim inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, onset_date = "onset_date", report_date = "report_date",
              method = "optimization")
    )
  })

  #STRATIFIED
  #----------------------------------------------------------------------------------------
  #Check sampling inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, onset_date = "onset_date", report_date = "report_date", chains = 1,
                     cores = 1, iter = 25, strata = ".strata")
    )
  })

  #Check variational inference works
  suppressWarnings({
    expect_no_error(
      nowcast(sims, onset_date = "onset_date", report_date = "report_date",
                   method = "variational", strata = ".strata")
    )
  })

  suppressWarnings({
    expect_no_error(
      nowcast(sims, onset_date = "onset_date", report_date = "report_date",
              method = "optimization", strata = ".strata")
    )
  })


})
