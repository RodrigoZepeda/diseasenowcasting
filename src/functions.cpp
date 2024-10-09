// Generated by rstantools.  Do not edit by hand.

// [[Rcpp::depends(StanHeaders)]]
// [[Rcpp::depends(rstan)]]
// [[Rcpp::plugins(rstan)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(BH)]]
#include <stan/math/prim/fun/Eigen.hpp>
#include <stan/math/prim/meta.hpp>
#include <boost/integer/integer_log2.hpp>
#include <RcppEigen.h>
#ifndef USE_STANC3
#define USE_STANC3
#endif
// Code generated by stanc v2.32.2
#include <stan/model/model_header.hpp>
namespace model778bca09eb7_User_defined_functions_namespace {
using stan::model::model_base_crtp;
using namespace stan::math;
stan::math::profile_map profiles__;
static constexpr std::array<const char*, 89> locations_array__ =
  {" (found before start of program)",
  " (in 'User-defined functions', line 17, column 9 to column 19)",
  " (in 'User-defined functions', line 17, column 21 to column 31)",
  " (in 'User-defined functions', line 17, column 2 to column 81)",
  " (in 'User-defined functions', line 20, column 4 to column 54)",
  " (in 'User-defined functions', line 19, column 2 to line 20, column 54)",
  " (in 'User-defined functions', line 21, column 2 to column 17)",
  " (in 'User-defined functions', line 3, column 50 to line 22, column 1)",
  " (in 'User-defined functions', line 40, column 9 to column 19)",
  " (in 'User-defined functions', line 40, column 21 to column 31)",
  " (in 'User-defined functions', line 40, column 2 to column 78)",
  " (in 'User-defined functions', line 42, column 9 to column 19)",
  " (in 'User-defined functions', line 42, column 21 to column 31)",
  " (in 'User-defined functions', line 42, column 2 to column 111)",
  " (in 'User-defined functions', line 46, column 6 to column 110)",
  " (in 'User-defined functions', line 45, column 60 to line 47, column 5)",
  " (in 'User-defined functions', line 45, column 4 to line 47, column 5)",
  " (in 'User-defined functions', line 44, column 21 to line 48, column 3)",
  " (in 'User-defined functions', line 44, column 2 to line 48, column 3)",
  " (in 'User-defined functions', line 49, column 2 to column 12)",
  " (in 'User-defined functions', line 24, column 92 to line 50, column 1)",
  " (in 'User-defined functions', line 67, column 9 to column 19)",
  " (in 'User-defined functions', line 67, column 21 to column 31)",
  " (in 'User-defined functions', line 67, column 2 to column 81)",
  " (in 'User-defined functions', line 69, column 9 to column 19)",
  " (in 'User-defined functions', line 69, column 21 to column 31)",
  " (in 'User-defined functions', line 69, column 2 to column 111)",
  " (in 'User-defined functions', line 70, column 9 to column 19)",
  " (in 'User-defined functions', line 70, column 21 to column 31)",
  " (in 'User-defined functions', line 70, column 2 to column 122)",
  " (in 'User-defined functions', line 73, column 4 to column 116)",
  " (in 'User-defined functions', line 72, column 2 to line 73, column 116)",
  " (in 'User-defined functions', line 79, column 8 to column 25)",
  " (in 'User-defined functions', line 78, column 13 to line 80, column 7)",
  " (in 'User-defined functions', line 77, column 8 to column 51)",
  " (in 'User-defined functions', line 76, column 29 to line 78, column 7)",
  " (in 'User-defined functions', line 76, column 6 to line 80, column 7)",
  " (in 'User-defined functions', line 75, column 27 to line 81, column 5)",
  " (in 'User-defined functions', line 75, column 4 to line 81, column 5)",
  " (in 'User-defined functions', line 74, column 25 to line 82, column 3)",
  " (in 'User-defined functions', line 74, column 2 to line 82, column 3)",
  " (in 'User-defined functions', line 83, column 2 to column 15)",
  " (in 'User-defined functions', line 52, column 95 to line 84, column 1)",
  " (in 'User-defined functions', line 90, column 10 to column 34)",
  " (in 'User-defined functions', line 90, column 4 to column 52)",
  " (in 'User-defined functions', line 91, column 4 to column 18)",
  " (in 'User-defined functions', line 92, column 4 to column 21)",
  " (in 'User-defined functions', line 95, column 6 to column 63)",
  " (in 'User-defined functions', line 96, column 6 to column 66)",
  " (in 'User-defined functions', line 100, column 8 to column 38)",
  " (in 'User-defined functions', line 99, column 13 to line 101, column 7)",
  " (in 'User-defined functions', line 98, column 8 to column 63)",
  " (in 'User-defined functions', line 97, column 28 to line 99, column 7)",
  " (in 'User-defined functions', line 97, column 6 to line 101, column 7)",
  " (in 'User-defined functions', line 93, column 23 to line 102, column 5)",
  " (in 'User-defined functions', line 93, column 4 to line 102, column 5)",
  " (in 'User-defined functions', line 103, column 4 to column 22)",
  " (in 'User-defined functions', line 88, column 28 to line 104, column 1)",
  " (in 'User-defined functions', line 110, column 10 to column 34)",
  " (in 'User-defined functions', line 110, column 4 to column 52)",
  " (in 'User-defined functions', line 111, column 4 to column 18)",
  " (in 'User-defined functions', line 112, column 4 to column 21)",
  " (in 'User-defined functions', line 115, column 6 to column 63)",
  " (in 'User-defined functions', line 116, column 6 to column 66)",
  " (in 'User-defined functions', line 120, column 8 to column 38)",
  " (in 'User-defined functions', line 119, column 13 to line 121, column 7)",
  " (in 'User-defined functions', line 118, column 8 to column 61)",
  " (in 'User-defined functions', line 117, column 28 to line 119, column 7)",
  " (in 'User-defined functions', line 117, column 6 to line 121, column 7)",
  " (in 'User-defined functions', line 113, column 23 to line 122, column 5)",
  " (in 'User-defined functions', line 113, column 4 to line 122, column 5)",
  " (in 'User-defined functions', line 123, column 4 to column 22)",
  " (in 'User-defined functions', line 108, column 28 to line 124, column 1)",
  " (in 'User-defined functions', line 129, column 10 to column 19)",
  " (in 'User-defined functions', line 129, column 21 to column 26)",
  " (in 'User-defined functions', line 129, column 4 to column 83)",
  " (in 'User-defined functions', line 137, column 12 to column 99)",
  " (in 'User-defined functions', line 136, column 17 to line 138, column 11)",
  " (in 'User-defined functions', line 135, column 12 to column 120)",
  " (in 'User-defined functions', line 134, column 31 to line 136, column 11)",
  " (in 'User-defined functions', line 134, column 10 to line 138, column 11)",
  " (in 'User-defined functions', line 133, column 30 to line 139, column 9)",
  " (in 'User-defined functions', line 133, column 8 to line 139, column 9)",
  " (in 'User-defined functions', line 131, column 29 to line 140, column 7)",
  " (in 'User-defined functions', line 131, column 6 to line 140, column 7)",
  " (in 'User-defined functions', line 130, column 27 to line 141, column 5)",
  " (in 'User-defined functions', line 130, column 4 to line 141, column 5)",
  " (in 'User-defined functions', line 142, column 4 to column 28)",
  " (in 'User-defined functions', line 127, column 44 to line 143, column 1)"};
Eigen::Matrix<double,-1,-1>
count_cases(const std::vector<std::vector<int>>& case_idx, const int&
            num_strata, const int& num_delays, const int& n_rows, const int&
            d_col, const int& s_col, std::ostream* pstream__);
Eigen::Matrix<double,-1,-1>
mean_cases(const std::vector<double>& cases_real,
           const std::vector<std::vector<int>>& case_idx, const int&
           num_strata, const int& num_delays, const int& n_rows, const int&
           d_col, const int& s_col, std::ostream* pstream__);
Eigen::Matrix<double,-1,-1>
sd_cases(const std::vector<double>& cases_real,
         const std::vector<std::vector<int>>& case_idx, const int&
         num_strata, const int& num_delays, const int& n_rows, const int&
         d_col, const int& s_col, std::ostream* pstream__);
template <typename T7__, typename T8__,
          stan::require_all_t<stan::is_eigen_matrix_dynamic<T7__>,
                              stan::is_vt_not_complex<T7__>,
                              stan::is_eigen_matrix_dynamic<T8__>,
                              stan::is_vt_not_complex<T8__>>* = nullptr>
std::vector<
  stan::promote_args_t<stan::base_type_t<T7__>, stan::base_type_t<T8__>>>
normalize_cases(const std::vector<double>& cases_real,
                const std::vector<std::vector<int>>& case_idx, const int&
                num_strata, const int& num_delays, const int& n_rows,
                const int& d_col, const int& s_col, const T7__& mu_arg__,
                const T8__& sigma_arg__, std::ostream* pstream__);
template <typename T7__, typename T8__,
          stan::require_all_t<stan::is_eigen_matrix_dynamic<T7__>,
                              stan::is_vt_not_complex<T7__>,
                              stan::is_eigen_matrix_dynamic<T8__>,
                              stan::is_vt_not_complex<T8__>>* = nullptr>
std::vector<
  stan::promote_args_t<stan::base_type_t<T7__>, stan::base_type_t<T8__>>>
inv_normalize_cases(const std::vector<double>& normalized,
                    const std::vector<std::vector<int>>& case_idx, const int&
                    num_strata, const int& num_delays, const int& n_rows,
                    const int& d_col, const int& s_col, const T7__& mu_arg__,
                    const T8__& sigma_arg__, std::ostream* pstream__);
template <typename T0__, typename T8__, typename T9__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_eigen_matrix_dynamic<T8__>,
                              stan::is_vt_not_complex<T8__>,
                              stan::is_eigen_matrix_dynamic<T9__>,
                              stan::is_vt_not_complex<T9__>>* = nullptr>
std::vector<
  std::vector<
    stan::promote_args_t<T0__, stan::base_type_t<T8__>,
      stan::base_type_t<T9__>>>>
inv_normalize_cases_2(const std::vector<std::vector<T0__>>& normalized_mat,
                      const std::vector<std::vector<int>>& case_idx,
                      const int& num_strata, const int& num_delays,
                      const int& num_steps, const int& tsize, const int&
                      d_col, const int& s_col, const T8__& mu_arg__,
                      const T9__& sigma_arg__, std::ostream* pstream__);
Eigen::Matrix<double,-1,-1>
count_cases(const std::vector<std::vector<int>>& case_idx, const int&
            num_strata, const int& num_delays, const int& n_rows, const int&
            d_col, const int& s_col, std::ostream* pstream__) {
  using local_scalar_t__ = double;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 1;
    stan::math::validate_non_negative_index("n_count", "num_strata",
      num_strata);
    current_statement__ = 2;
    stan::math::validate_non_negative_index("n_count", "num_delays",
      num_delays);
    Eigen::Matrix<local_scalar_t__,-1,-1> n_count =
      Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(num_strata, num_delays,
        DUMMY_VAR__);
    current_statement__ = 3;
    stan::model::assign(n_count,
      stan::math::rep_matrix(0, num_strata, num_delays),
      "assigning variable n_count");
    current_statement__ = 5;
    for (int n = 1; n <= n_rows; ++n) {
      current_statement__ = 4;
      stan::model::assign(n_count,
        (stan::model::rvalue(n_count, "n_count",
           stan::model::index_uni(
             stan::model::rvalue(case_idx, "case_idx",
               stan::model::index_uni(n), stan::model::index_uni(s_col))),
           stan::model::index_uni(
             stan::model::rvalue(case_idx, "case_idx",
               stan::model::index_uni(n), stan::model::index_uni(d_col)))) +
        1), "assigning variable n_count",
        stan::model::index_uni(
          stan::model::rvalue(case_idx, "case_idx",
            stan::model::index_uni(n), stan::model::index_uni(s_col))),
        stan::model::index_uni(
          stan::model::rvalue(case_idx, "case_idx",
            stan::model::index_uni(n), stan::model::index_uni(d_col))));
    }
    current_statement__ = 6;
    return n_count;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
Eigen::Matrix<double,-1,-1>
mean_cases(const std::vector<double>& cases_real,
           const std::vector<std::vector<int>>& case_idx, const int&
           num_strata, const int& num_delays, const int& n_rows, const int&
           d_col, const int& s_col, std::ostream* pstream__) {
  using local_scalar_t__ = double;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 8;
    stan::math::validate_non_negative_index("mu", "num_strata", num_strata);
    current_statement__ = 9;
    stan::math::validate_non_negative_index("mu", "num_delays", num_delays);
    Eigen::Matrix<local_scalar_t__,-1,-1> mu =
      Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(num_strata, num_delays,
        DUMMY_VAR__);
    current_statement__ = 10;
    stan::model::assign(mu,
      stan::math::rep_matrix(0.0, num_strata, num_delays),
      "assigning variable mu");
    current_statement__ = 11;
    stan::math::validate_non_negative_index("n_count", "num_strata",
      num_strata);
    current_statement__ = 12;
    stan::math::validate_non_negative_index("n_count", "num_delays",
      num_delays);
    Eigen::Matrix<local_scalar_t__,-1,-1> n_count =
      Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(num_strata, num_delays,
        DUMMY_VAR__);
    current_statement__ = 13;
    stan::model::assign(n_count,
      count_cases(case_idx, num_strata, num_delays, n_rows, d_col, s_col,
        pstream__), "assigning variable n_count");
    current_statement__ = 18;
    for (int n = 1; n <= n_rows; ++n) {
      current_statement__ = 16;
      if (stan::math::logical_gt(
            stan::model::rvalue(n_count, "n_count",
              stan::model::index_uni(
                stan::model::rvalue(case_idx, "case_idx",
                  stan::model::index_uni(n), stan::model::index_uni(s_col))),
              stan::model::index_uni(
                stan::model::rvalue(case_idx, "case_idx",
                  stan::model::index_uni(n), stan::model::index_uni(d_col)))),
            0.0)) {
        current_statement__ = 14;
        stan::model::assign(mu,
          (stan::model::rvalue(mu, "mu",
             stan::model::index_uni(
               stan::model::rvalue(case_idx, "case_idx",
                 stan::model::index_uni(n), stan::model::index_uni(s_col))),
             stan::model::index_uni(
               stan::model::rvalue(case_idx, "case_idx",
                 stan::model::index_uni(n), stan::model::index_uni(d_col))))
          +
          (stan::model::rvalue(cases_real, "cases_real",
             stan::model::index_uni(n)) /
          stan::model::rvalue(n_count, "n_count",
            stan::model::index_uni(
              stan::model::rvalue(case_idx, "case_idx",
                stan::model::index_uni(n), stan::model::index_uni(s_col))),
            stan::model::index_uni(
              stan::model::rvalue(case_idx, "case_idx",
                stan::model::index_uni(n), stan::model::index_uni(d_col)))))),
          "assigning variable mu",
          stan::model::index_uni(
            stan::model::rvalue(case_idx, "case_idx",
              stan::model::index_uni(n), stan::model::index_uni(s_col))),
          stan::model::index_uni(
            stan::model::rvalue(case_idx, "case_idx",
              stan::model::index_uni(n), stan::model::index_uni(d_col))));
      }
    }
    current_statement__ = 19;
    return mu;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
Eigen::Matrix<double,-1,-1>
sd_cases(const std::vector<double>& cases_real,
         const std::vector<std::vector<int>>& case_idx, const int&
         num_strata, const int& num_delays, const int& n_rows, const int&
         d_col, const int& s_col, std::ostream* pstream__) {
  using local_scalar_t__ = double;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 21;
    stan::math::validate_non_negative_index("sigma", "num_strata", num_strata);
    current_statement__ = 22;
    stan::math::validate_non_negative_index("sigma", "num_delays", num_delays);
    Eigen::Matrix<local_scalar_t__,-1,-1> sigma =
      Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(num_strata, num_delays,
        DUMMY_VAR__);
    current_statement__ = 23;
    stan::model::assign(sigma,
      stan::math::rep_matrix(0.0, num_strata, num_delays),
      "assigning variable sigma");
    current_statement__ = 24;
    stan::math::validate_non_negative_index("n_count", "num_strata",
      num_strata);
    current_statement__ = 25;
    stan::math::validate_non_negative_index("n_count", "num_delays",
      num_delays);
    Eigen::Matrix<local_scalar_t__,-1,-1> n_count =
      Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(num_strata, num_delays,
        DUMMY_VAR__);
    current_statement__ = 26;
    stan::model::assign(n_count,
      count_cases(case_idx, num_strata, num_delays, n_rows, d_col, s_col,
        pstream__), "assigning variable n_count");
    current_statement__ = 27;
    stan::math::validate_non_negative_index("mu", "num_strata", num_strata);
    current_statement__ = 28;
    stan::math::validate_non_negative_index("mu", "num_delays", num_delays);
    Eigen::Matrix<local_scalar_t__,-1,-1> mu =
      Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(num_strata, num_delays,
        DUMMY_VAR__);
    current_statement__ = 29;
    stan::model::assign(mu,
      mean_cases(cases_real, case_idx, num_strata, num_delays, n_rows, d_col,
        s_col, pstream__), "assigning variable mu");
    current_statement__ = 31;
    for (int n = 1; n <= n_rows; ++n) {
      current_statement__ = 30;
      stan::model::assign(sigma,
        (stan::model::rvalue(sigma, "sigma",
           stan::model::index_uni(
             stan::model::rvalue(case_idx, "case_idx",
               stan::model::index_uni(n), stan::model::index_uni(s_col))),
           stan::model::index_uni(
             stan::model::rvalue(case_idx, "case_idx",
               stan::model::index_uni(n), stan::model::index_uni(d_col)))) +
        stan::math::pow(
          (stan::model::rvalue(cases_real, "cases_real",
             stan::model::index_uni(n)) -
          stan::model::rvalue(mu, "mu",
            stan::model::index_uni(
              stan::model::rvalue(case_idx, "case_idx",
                stan::model::index_uni(n), stan::model::index_uni(s_col))),
            stan::model::index_uni(
              stan::model::rvalue(case_idx, "case_idx",
                stan::model::index_uni(n), stan::model::index_uni(d_col))))),
          2)), "assigning variable sigma",
        stan::model::index_uni(
          stan::model::rvalue(case_idx, "case_idx",
            stan::model::index_uni(n), stan::model::index_uni(s_col))),
        stan::model::index_uni(
          stan::model::rvalue(case_idx, "case_idx",
            stan::model::index_uni(n), stan::model::index_uni(d_col))));
    }
    current_statement__ = 40;
    for (int d = 1; d <= num_delays; ++d) {
      current_statement__ = 38;
      for (int s = 1; s <= num_strata; ++s) {
        current_statement__ = 36;
        if (stan::math::logical_gt(
              stan::model::rvalue(n_count, "n_count",
                stan::model::index_uni(s), stan::model::index_uni(d)), 0.0)) {
          current_statement__ = 34;
          stan::model::assign(sigma,
            stan::math::sqrt(
              (stan::model::rvalue(sigma, "sigma", stan::model::index_uni(s),
                 stan::model::index_uni(d)) /
              stan::model::rvalue(n_count, "n_count",
                stan::model::index_uni(s), stan::model::index_uni(d)))),
            "assigning variable sigma", stan::model::index_uni(s),
            stan::model::index_uni(d));
        } else {
          current_statement__ = 32;
          stan::model::assign(sigma, 0.0, "assigning variable sigma",
            stan::model::index_uni(s), stan::model::index_uni(d));
        }
      }
    }
    current_statement__ = 41;
    return sigma;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T7__, typename T8__,
          stan::require_all_t<stan::is_eigen_matrix_dynamic<T7__>,
                              stan::is_vt_not_complex<T7__>,
                              stan::is_eigen_matrix_dynamic<T8__>,
                              stan::is_vt_not_complex<T8__>>*>
std::vector<
  stan::promote_args_t<stan::base_type_t<T7__>, stan::base_type_t<T8__>>>
normalize_cases(const std::vector<double>& cases_real,
                const std::vector<std::vector<int>>& case_idx, const int&
                num_strata, const int& num_delays, const int& n_rows,
                const int& d_col, const int& s_col, const T7__& mu_arg__,
                const T8__& sigma_arg__, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<stan::base_type_t<T7__>,
                             stan::base_type_t<T8__>>;
  int current_statement__ = 0;
  const auto& mu = stan::math::to_ref(mu_arg__);
  const auto& sigma = stan::math::to_ref(sigma_arg__);
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 43;
    stan::math::validate_non_negative_index("normalized",
      "num_elements(cases_real)", stan::math::num_elements(cases_real));
    std::vector<local_scalar_t__> normalized =
      std::vector<local_scalar_t__>(stan::math::num_elements(cases_real),
        DUMMY_VAR__);
    local_scalar_t__ mu_value = DUMMY_VAR__;
    local_scalar_t__ sigma_value = DUMMY_VAR__;
    current_statement__ = 55;
    for (int n = 1; n <= n_rows; ++n) {
      current_statement__ = 47;
      mu_value = stan::model::rvalue(mu, "mu",
                   stan::model::index_uni(
                     stan::model::rvalue(case_idx, "case_idx",
                       stan::model::index_uni(n),
                       stan::model::index_uni(s_col))),
                   stan::model::index_uni(
                     stan::model::rvalue(case_idx, "case_idx",
                       stan::model::index_uni(n),
                       stan::model::index_uni(d_col))));
      current_statement__ = 48;
      sigma_value = stan::model::rvalue(sigma, "sigma",
                      stan::model::index_uni(
                        stan::model::rvalue(case_idx, "case_idx",
                          stan::model::index_uni(n),
                          stan::model::index_uni(s_col))),
                      stan::model::index_uni(
                        stan::model::rvalue(case_idx, "case_idx",
                          stan::model::index_uni(n),
                          stan::model::index_uni(d_col))));
      current_statement__ = 53;
      if (stan::math::logical_gt(sigma_value, 0.0)) {
        current_statement__ = 51;
        stan::model::assign(normalized,
          ((stan::model::rvalue(cases_real, "cases_real",
              stan::model::index_uni(n)) - mu_value) / sigma_value),
          "assigning variable normalized", stan::model::index_uni(n));
      } else {
        current_statement__ = 49;
        stan::model::assign(normalized,
          stan::model::rvalue(cases_real, "cases_real",
            stan::model::index_uni(n)), "assigning variable normalized",
          stan::model::index_uni(n));
      }
    }
    current_statement__ = 56;
    return normalized;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T7__, typename T8__,
          stan::require_all_t<stan::is_eigen_matrix_dynamic<T7__>,
                              stan::is_vt_not_complex<T7__>,
                              stan::is_eigen_matrix_dynamic<T8__>,
                              stan::is_vt_not_complex<T8__>>*>
std::vector<
  stan::promote_args_t<stan::base_type_t<T7__>, stan::base_type_t<T8__>>>
inv_normalize_cases(const std::vector<double>& normalized,
                    const std::vector<std::vector<int>>& case_idx, const int&
                    num_strata, const int& num_delays, const int& n_rows,
                    const int& d_col, const int& s_col, const T7__& mu_arg__,
                    const T8__& sigma_arg__, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<stan::base_type_t<T7__>,
                             stan::base_type_t<T8__>>;
  int current_statement__ = 0;
  const auto& mu = stan::math::to_ref(mu_arg__);
  const auto& sigma = stan::math::to_ref(sigma_arg__);
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 58;
    stan::math::validate_non_negative_index("real_cases",
      "num_elements(normalized)", stan::math::num_elements(normalized));
    std::vector<local_scalar_t__> real_cases =
      std::vector<local_scalar_t__>(stan::math::num_elements(normalized),
        DUMMY_VAR__);
    local_scalar_t__ mu_value = DUMMY_VAR__;
    local_scalar_t__ sigma_value = DUMMY_VAR__;
    current_statement__ = 70;
    for (int n = 1; n <= n_rows; ++n) {
      current_statement__ = 62;
      mu_value = stan::model::rvalue(mu, "mu",
                   stan::model::index_uni(
                     stan::model::rvalue(case_idx, "case_idx",
                       stan::model::index_uni(n),
                       stan::model::index_uni(s_col))),
                   stan::model::index_uni(
                     stan::model::rvalue(case_idx, "case_idx",
                       stan::model::index_uni(n),
                       stan::model::index_uni(d_col))));
      current_statement__ = 63;
      sigma_value = stan::model::rvalue(sigma, "sigma",
                      stan::model::index_uni(
                        stan::model::rvalue(case_idx, "case_idx",
                          stan::model::index_uni(n),
                          stan::model::index_uni(s_col))),
                      stan::model::index_uni(
                        stan::model::rvalue(case_idx, "case_idx",
                          stan::model::index_uni(n),
                          stan::model::index_uni(d_col))));
      current_statement__ = 68;
      if (stan::math::logical_gt(sigma_value, 0.0)) {
        current_statement__ = 66;
        stan::model::assign(real_cases,
          ((stan::model::rvalue(normalized, "normalized",
              stan::model::index_uni(n)) * sigma_value) + mu_value),
          "assigning variable real_cases", stan::model::index_uni(n));
      } else {
        current_statement__ = 64;
        stan::model::assign(real_cases,
          stan::model::rvalue(normalized, "normalized",
            stan::model::index_uni(n)), "assigning variable real_cases",
          stan::model::index_uni(n));
      }
    }
    current_statement__ = 71;
    return real_cases;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T0__, typename T8__, typename T9__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_eigen_matrix_dynamic<T8__>,
                              stan::is_vt_not_complex<T8__>,
                              stan::is_eigen_matrix_dynamic<T9__>,
                              stan::is_vt_not_complex<T9__>>*>
std::vector<
  std::vector<
    stan::promote_args_t<T0__, stan::base_type_t<T8__>,
      stan::base_type_t<T9__>>>>
inv_normalize_cases_2(const std::vector<std::vector<T0__>>& normalized_mat,
                      const std::vector<std::vector<int>>& case_idx,
                      const int& num_strata, const int& num_delays,
                      const int& num_steps, const int& tsize, const int&
                      d_col, const int& s_col, const T8__& mu_arg__,
                      const T9__& sigma_arg__, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__,
                             stan::base_type_t<T8__>,
                             stan::base_type_t<T9__>>;
  int current_statement__ = 0;
  const auto& mu = stan::math::to_ref(mu_arg__);
  const auto& sigma = stan::math::to_ref(sigma_arg__);
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 73;
    stan::math::validate_non_negative_index("unnormalized_mat", "num_steps",
      num_steps);
    current_statement__ = 74;
    stan::math::validate_non_negative_index("unnormalized_mat", "tsize",
      tsize);
    std::vector<std::vector<local_scalar_t__>> unnormalized_mat =
      std::vector<std::vector<local_scalar_t__>>(num_steps,
        std::vector<local_scalar_t__>(tsize, DUMMY_VAR__));
    current_statement__ = 75;
    stan::model::assign(unnormalized_mat,
      stan::math::rep_array(0, num_steps, tsize),
      "assigning variable unnormalized_mat");
    current_statement__ = 86;
    for (int s = 1; s <= num_strata; ++s) {
      current_statement__ = 84;
      for (int d = 1; d <= num_delays; ++d) {
        current_statement__ = 82;
        for (int t = 1; t <= num_steps; ++t) {
          current_statement__ = 80;
          if (stan::math::logical_gt(
                stan::model::rvalue(sigma, "sigma",
                  stan::model::index_uni(s), stan::model::index_uni(d)), 0.0)) {
            current_statement__ = 78;
            stan::model::assign(unnormalized_mat,
              ((stan::model::rvalue(normalized_mat, "normalized_mat",
                  stan::model::index_uni(t),
                  stan::model::index_uni(((num_delays * (s - 1)) + d))) *
              stan::model::rvalue(sigma, "sigma", stan::model::index_uni(s),
                stan::model::index_uni(d))) +
              stan::model::rvalue(mu, "mu", stan::model::index_uni(s),
                stan::model::index_uni(d))),
              "assigning variable unnormalized_mat",
              stan::model::index_uni(t),
              stan::model::index_uni(((num_delays * (s - 1)) + d)));
          } else {
            current_statement__ = 76;
            stan::model::assign(unnormalized_mat,
              stan::model::rvalue(normalized_mat, "normalized_mat",
                stan::model::index_uni(t),
                stan::model::index_uni(((num_delays * (s - 1)) + d))),
              "assigning variable unnormalized_mat",
              stan::model::index_uni(t),
              stan::model::index_uni(((num_delays * (s - 1)) + d)));
          }
        }
      }
    }
    current_statement__ = 87;
    return unnormalized_mat;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
}
// [[Rcpp::export]]
 Eigen::Matrix<double,-1,-1> 
count_cases(const std::vector<std::vector<int>>& case_idx, const int&
            num_strata, const int& num_delays, const int& n_rows, const int&
            d_col, const int& s_col, std::ostream* pstream__ = 0) {
  return model778bca09eb7_User_defined_functions_namespace::count_cases(
           case_idx, num_strata, num_delays, n_rows, d_col, s_col, pstream__);
}
// [[Rcpp::export]]
 Eigen::Matrix<double,-1,-1> 
mean_cases(const std::vector<double>& cases_real,
           const std::vector<std::vector<int>>& case_idx, const int&
           num_strata, const int& num_delays, const int& n_rows, const int&
           d_col, const int& s_col, std::ostream* pstream__ = 0) {
  return model778bca09eb7_User_defined_functions_namespace::mean_cases(
           cases_real, case_idx, num_strata, num_delays, n_rows, d_col,
           s_col, pstream__);
}
// [[Rcpp::export]]
 Eigen::Matrix<double,-1,-1> 
sd_cases(const std::vector<double>& cases_real,
         const std::vector<std::vector<int>>& case_idx, const int&
         num_strata, const int& num_delays, const int& n_rows, const int&
         d_col, const int& s_col, std::ostream* pstream__ = 0) {
  return model778bca09eb7_User_defined_functions_namespace::sd_cases(
           cases_real, case_idx, num_strata, num_delays, n_rows, d_col,
           s_col, pstream__);
}
// [[Rcpp::export]]
 std::vector<   stan::promote_args_t<stan::base_type_t<double>, stan::base_type_t<double>>> 
normalize_cases(const std::vector<double>& cases_real,
                const std::vector<std::vector<int>>& case_idx, const int&
                num_strata, const int& num_delays, const int& n_rows,
                const int& d_col, const int& s_col,
                const Eigen::Matrix<double,-1,-1>& mu,
                const Eigen::Matrix<double,-1,-1>& sigma, std::ostream*
                pstream__ = 0) {
  return model778bca09eb7_User_defined_functions_namespace::normalize_cases(
           cases_real, case_idx, num_strata, num_delays, n_rows, d_col,
           s_col, mu, sigma, pstream__);
}
// [[Rcpp::export]]
 std::vector<   stan::promote_args_t<stan::base_type_t<double>, stan::base_type_t<double>>> 
inv_normalize_cases(const std::vector<double>& normalized,
                    const std::vector<std::vector<int>>& case_idx, const int&
                    num_strata, const int& num_delays, const int& n_rows,
                    const int& d_col, const int& s_col,
                    const Eigen::Matrix<double,-1,-1>& mu,
                    const Eigen::Matrix<double,-1,-1>& sigma, std::ostream*
                    pstream__ = 0) {
  return model778bca09eb7_User_defined_functions_namespace::inv_normalize_cases(
           normalized, case_idx, num_strata, num_delays, n_rows, d_col,
           s_col, mu, sigma, pstream__);
}
// [[Rcpp::export]]
 std::vector<   std::vector<     stan::promote_args_t<double, stan::base_type_t<double>,       stan::base_type_t<double>>>> 
inv_normalize_cases_2(const std::vector<std::vector<double>>& normalized_mat,
                      const std::vector<std::vector<int>>& case_idx,
                      const int& num_strata, const int& num_delays,
                      const int& num_steps, const int& tsize, const int&
                      d_col, const int& s_col,
                      const Eigen::Matrix<double,-1,-1>& mu,
                      const Eigen::Matrix<double,-1,-1>& sigma, std::ostream*
                      pstream__ = 0) {
  return model778bca09eb7_User_defined_functions_namespace::inv_normalize_cases_2(
           normalized_mat, case_idx, num_strata, num_delays, num_steps,
           tsize, d_col, s_col, mu, sigma, pstream__);
}
