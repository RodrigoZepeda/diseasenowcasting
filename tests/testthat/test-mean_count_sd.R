#Generate the cases idx
set.seed(2758)
n_obs     <- 45
num_steps <- 100
num_delays <- 5
num_strata <- 3

case_idx <-lapply(
  1:n_obs, function(x){c(
    "t" = sample(1:num_steps, 1, FALSE),
    "d" = sample(1:num_delays, 1, FALSE),
    "s" = sample(1:num_strata, 1, FALSE)
  )}
)
cases <- data.frame(cases = rlnorm(n_obs, meanlog = log(24), sdlog = 0.5))


test_that("`count_cases` works", {

  #Check the count actually counts
  ccount <- count_cases(case_idx = case_idx, num_strata = num_strata,
                        num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                        pstream__ = rstan::get_stream())

  cdf <- dplyr::bind_rows(case_idx) |>
    dplyr::count(d, s) |>
    tidyr::pivot_wider(id_cols = s, names_from = d, values_from = n, values_fill = 0) |>
    dplyr::select(-!!as.symbol("s")) |>
    as.matrix()
  dimnames(cdf) <- NULL

  expect_equal(ccount, cdf)

})

test_that("`mean_cases` works", {

  #Check the count actually counts
  cmean <- mean_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                        num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                        pstream__ = rstan::get_stream())

  cdf <- dplyr::bind_rows(case_idx) |>
    dplyr::bind_cols(cases) |>
    dplyr::group_by(d, s) |>
    dplyr::summarise(mu = mean(cases), .groups = "drop") |>
    tidyr::pivot_wider(id_cols = s, names_from = d, values_from = mu, values_fill = 0) |>
    dplyr::select(-!!as.symbol("s")) |>
    as.matrix()
  dimnames(cdf) <- NULL

  expect_equal(cmean, cdf)

})

test_that("`sd_cases` works", {

  #Check the count actually counts
  csd <- sd_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                  num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                  pstream__ = rstan::get_stream())

  cdf <- dplyr::bind_rows(case_idx) |>
    dplyr::bind_cols(cases) |>
    dplyr::group_by(d, s) |>
    dplyr::summarise(mu = sd(cases)*sqrt((dplyr::n() - 1)/dplyr::n()), .groups = "drop") |>
    tidyr::pivot_wider(id_cols = s, names_from = d, values_from = mu, values_fill = 0) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0))) |>
    dplyr::select(-!!as.symbol("s")) |>
    as.matrix()
  dimnames(cdf) <- NULL

  expect_equal(csd, cdf)

})

test_that("`normalization` works", {

  #Check the count actually counts
  mu   <- mean_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                      num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                      pstream__ = rstan::get_stream())

  #Check the count actually counts
  sigma <- sd_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                  num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                  pstream__ = rstan::get_stream())


  #Check the count actually counts
  csd <- normalize_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                         num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                         mu = mu, sigma = sigma, pstream__ = rstan::get_stream())

  cdf <- dplyr::bind_rows(case_idx) |>
    dplyr::bind_cols(cases) |>
    dplyr::left_join(
      dplyr::bind_rows(case_idx) |>
        dplyr::bind_cols(cases) |>
        dplyr::group_by(s,d) |>
        dplyr::summarise(mu = mean(cases), sigma = sd(cases)*sqrt((dplyr::n() - 1)/dplyr::n()), .groups = "drop"),
      by = c("s","d")
    ) |>
    dplyr::mutate(normalized_cases = dplyr::if_else(!is.na(sigma) & sigma > 0, (cases - mu)/sigma, cases))

  expect_equal(csd, cdf |> dplyr::pull(normalized_cases))

})

test_that("`inverse normalization` works", {

  #Get mean
  mu   <- mean_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                     num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                     pstream__ = rstan::get_stream())

  #Get standard dev
  sigma <- sd_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                    num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                    pstream__ = rstan::get_stream())

  #Normalize the cases to unnormalize them
  csd <- normalize_cases(cases_real = cases$cases, case_idx = case_idx, num_strata = num_strata,
                         num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                         mu = mu, sigma = sigma, pstream__ = rstan::get_stream())

  #Check that it returns to original value
  cinv <- inv_normalize_cases(normalized = csd, case_idx = case_idx, num_strata = num_strata,
                              num_delays = num_delays, n_rows = n_obs, d_col = 2, s_col = 3,
                              mu = mu, sigma = sigma, pstream__ = rstan::get_stream())

  expect_equal(cinv, cases$cases)

})

test_that("`inverse normalization` works for the second version", {

  # Define the data
  cases  <- c(10, 11, 9, 21, 16, 15, 12, 8, 8, 25, 20, 17, 11, 9, 0, 14, 21, 28)
  time   <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
  delay  <- c(0, 0, 1, 1, 2, 2, 0, 0, 1, 1, 2, 2,  0, 0, 1, 1, 2, 2) + 1
  strata <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)

  # Create a data frame
  case_data <- data.frame(
    cases = cases,
    t = time,
    d = delay,
    s = strata
  ) |>
    dplyr::arrange(s, d)

  case_idx <- case_data[,-1]
  case_idx <- split(as.matrix(case_idx), seq(nrow(case_idx)))

  #Get mean
  mu   <- mean_cases(cases_real = case_data$cases, case_idx = case_idx, num_strata = 2,
                     num_delays = 3, n_rows = nrow(case_data), d_col = 2, s_col = 3,
                     pstream__ = rstan::get_stream())

  #Get standard dev
  sigma <- sd_cases(cases_real = case_data$cases, case_idx = case_idx, num_strata = 2,
                    num_delays = 3, n_rows = nrow(case_data), d_col = 2, s_col = 3,
                    pstream__ = rstan::get_stream())

  #Normalize the cases to unnormalize them
  #FIXME: Make this test
  cdf <- case_data |>
    dplyr::left_join(
      case_data |>
        dplyr::group_by(s,d) |>
        dplyr::summarise(mu = mean(cases), sigma = sd(cases)*sqrt((dplyr::n() - 1)/dplyr::n()), .groups = "drop"),
      by = c("s","d")
    ) |>
    dplyr::mutate(normalized_cases = dplyr::if_else(!is.na(sigma) & sigma > 0, (cases - mu)/sigma, cases)) |>
    tidyr::pivot_wider(id_cols = c(d, s), names_from = t, values_from = normalized_cases) |>
    dplyr::select(-d,-s) |>
    as.matrix()

  cdf <- split(t(cdf), seq(ncol(cdf)))

  #Check that it returns to original value
  cinv <- inv_normalize_cases_2(normalized = cdf, case_idx = case_idx, num_strata = 2,
                                num_delays = 3, num_steps = 3, tsize = 6, d_col = 2, s_col = 3,
                                mu = mu, sigma = sigma, pstream__ = rstan::get_stream())

  inv_cases <- sapply(cinv, rbind) |> as.matrix() |> round()

  #Comparison
  comparison <- case_data |>
    tidyr::pivot_wider(id_cols = c(d,s), names_from = t, values_from = cases) |>
    dplyr::select(-d, -s) |>
    as.matrix()
  dimnames(comparison) <- NULL

  expect_equal(inv_cases, comparison)

})
