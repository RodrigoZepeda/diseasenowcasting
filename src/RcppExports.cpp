// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "diseasenowcasting_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// count_cases
Eigen::Matrix<double,-1,-1> count_cases(const std::vector<std::vector<int>>& case_idx, const int& num_strata, const int& num_delays, const int& n_rows, const int& d_col, const int& s_col, std::ostream* pstream__);
RcppExport SEXP _diseasenowcasting_count_cases(SEXP case_idxSEXP, SEXP num_strataSEXP, SEXP num_delaysSEXP, SEXP n_rowsSEXP, SEXP d_colSEXP, SEXP s_colSEXP, SEXP pstream__SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::vector<int>>& >::type case_idx(case_idxSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_strata(num_strataSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_delays(num_delaysSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_rows(n_rowsSEXP);
    Rcpp::traits::input_parameter< const int& >::type d_col(d_colSEXP);
    Rcpp::traits::input_parameter< const int& >::type s_col(s_colSEXP);
    Rcpp::traits::input_parameter< std::ostream* >::type pstream__(pstream__SEXP);
    rcpp_result_gen = Rcpp::wrap(count_cases(case_idx, num_strata, num_delays, n_rows, d_col, s_col, pstream__));
    return rcpp_result_gen;
END_RCPP
}
// mean_cases
Eigen::Matrix<double,-1,-1> mean_cases(const std::vector<double>& cases_real, const std::vector<std::vector<int>>& case_idx, const int& num_strata, const int& num_delays, const int& n_rows, const int& d_col, const int& s_col, std::ostream* pstream__);
RcppExport SEXP _diseasenowcasting_mean_cases(SEXP cases_realSEXP, SEXP case_idxSEXP, SEXP num_strataSEXP, SEXP num_delaysSEXP, SEXP n_rowsSEXP, SEXP d_colSEXP, SEXP s_colSEXP, SEXP pstream__SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type cases_real(cases_realSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::vector<int>>& >::type case_idx(case_idxSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_strata(num_strataSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_delays(num_delaysSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_rows(n_rowsSEXP);
    Rcpp::traits::input_parameter< const int& >::type d_col(d_colSEXP);
    Rcpp::traits::input_parameter< const int& >::type s_col(s_colSEXP);
    Rcpp::traits::input_parameter< std::ostream* >::type pstream__(pstream__SEXP);
    rcpp_result_gen = Rcpp::wrap(mean_cases(cases_real, case_idx, num_strata, num_delays, n_rows, d_col, s_col, pstream__));
    return rcpp_result_gen;
END_RCPP
}
// sd_cases
Eigen::Matrix<double,-1,-1> sd_cases(const std::vector<double>& cases_real, const std::vector<std::vector<int>>& case_idx, const int& num_strata, const int& num_delays, const int& n_rows, const int& d_col, const int& s_col, std::ostream* pstream__);
RcppExport SEXP _diseasenowcasting_sd_cases(SEXP cases_realSEXP, SEXP case_idxSEXP, SEXP num_strataSEXP, SEXP num_delaysSEXP, SEXP n_rowsSEXP, SEXP d_colSEXP, SEXP s_colSEXP, SEXP pstream__SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type cases_real(cases_realSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::vector<int>>& >::type case_idx(case_idxSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_strata(num_strataSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_delays(num_delaysSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_rows(n_rowsSEXP);
    Rcpp::traits::input_parameter< const int& >::type d_col(d_colSEXP);
    Rcpp::traits::input_parameter< const int& >::type s_col(s_colSEXP);
    Rcpp::traits::input_parameter< std::ostream* >::type pstream__(pstream__SEXP);
    rcpp_result_gen = Rcpp::wrap(sd_cases(cases_real, case_idx, num_strata, num_delays, n_rows, d_col, s_col, pstream__));
    return rcpp_result_gen;
END_RCPP
}
// normalize_cases
std::vector<   stan::promote_args_t<stan::base_type_t<double>, stan::base_type_t<double>>> normalize_cases(const std::vector<double>& cases_real, const std::vector<std::vector<int>>& case_idx, const int& num_strata, const int& num_delays, const int& n_rows, const int& d_col, const int& s_col, const Eigen::Matrix<double,-1,-1>& mu, const Eigen::Matrix<double,-1,-1>& sigma, std::ostream* pstream__);
RcppExport SEXP _diseasenowcasting_normalize_cases(SEXP cases_realSEXP, SEXP case_idxSEXP, SEXP num_strataSEXP, SEXP num_delaysSEXP, SEXP n_rowsSEXP, SEXP d_colSEXP, SEXP s_colSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP pstream__SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type cases_real(cases_realSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::vector<int>>& >::type case_idx(case_idxSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_strata(num_strataSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_delays(num_delaysSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_rows(n_rowsSEXP);
    Rcpp::traits::input_parameter< const int& >::type d_col(d_colSEXP);
    Rcpp::traits::input_parameter< const int& >::type s_col(s_colSEXP);
    Rcpp::traits::input_parameter< const Eigen::Matrix<double,-1,-1>& >::type mu(muSEXP);
    Rcpp::traits::input_parameter< const Eigen::Matrix<double,-1,-1>& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< std::ostream* >::type pstream__(pstream__SEXP);
    rcpp_result_gen = Rcpp::wrap(normalize_cases(cases_real, case_idx, num_strata, num_delays, n_rows, d_col, s_col, mu, sigma, pstream__));
    return rcpp_result_gen;
END_RCPP
}
// inv_normalize_cases
std::vector<   stan::promote_args_t<stan::base_type_t<double>, stan::base_type_t<double>>> inv_normalize_cases(const std::vector<double>& normalized, const std::vector<std::vector<int>>& case_idx, const int& num_strata, const int& num_delays, const int& n_rows, const int& d_col, const int& s_col, const Eigen::Matrix<double,-1,-1>& mu, const Eigen::Matrix<double,-1,-1>& sigma, std::ostream* pstream__);
RcppExport SEXP _diseasenowcasting_inv_normalize_cases(SEXP normalizedSEXP, SEXP case_idxSEXP, SEXP num_strataSEXP, SEXP num_delaysSEXP, SEXP n_rowsSEXP, SEXP d_colSEXP, SEXP s_colSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP pstream__SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type normalized(normalizedSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::vector<int>>& >::type case_idx(case_idxSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_strata(num_strataSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_delays(num_delaysSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_rows(n_rowsSEXP);
    Rcpp::traits::input_parameter< const int& >::type d_col(d_colSEXP);
    Rcpp::traits::input_parameter< const int& >::type s_col(s_colSEXP);
    Rcpp::traits::input_parameter< const Eigen::Matrix<double,-1,-1>& >::type mu(muSEXP);
    Rcpp::traits::input_parameter< const Eigen::Matrix<double,-1,-1>& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< std::ostream* >::type pstream__(pstream__SEXP);
    rcpp_result_gen = Rcpp::wrap(inv_normalize_cases(normalized, case_idx, num_strata, num_delays, n_rows, d_col, s_col, mu, sigma, pstream__));
    return rcpp_result_gen;
END_RCPP
}
// inv_normalize_cases_2
std::vector<   std::vector<     stan::promote_args_t<double, stan::base_type_t<double>,       stan::base_type_t<double>>>> inv_normalize_cases_2(const std::vector<std::vector<double>>& normalized_mat, const std::vector<std::vector<int>>& case_idx, const int& num_strata, const int& num_delays, const int& num_steps, const int& tsize, const int& d_col, const int& s_col, const Eigen::Matrix<double,-1,-1>& mu, const Eigen::Matrix<double,-1,-1>& sigma, std::ostream* pstream__);
RcppExport SEXP _diseasenowcasting_inv_normalize_cases_2(SEXP normalized_matSEXP, SEXP case_idxSEXP, SEXP num_strataSEXP, SEXP num_delaysSEXP, SEXP num_stepsSEXP, SEXP tsizeSEXP, SEXP d_colSEXP, SEXP s_colSEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP pstream__SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::vector<double>>& >::type normalized_mat(normalized_matSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::vector<int>>& >::type case_idx(case_idxSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_strata(num_strataSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_delays(num_delaysSEXP);
    Rcpp::traits::input_parameter< const int& >::type num_steps(num_stepsSEXP);
    Rcpp::traits::input_parameter< const int& >::type tsize(tsizeSEXP);
    Rcpp::traits::input_parameter< const int& >::type d_col(d_colSEXP);
    Rcpp::traits::input_parameter< const int& >::type s_col(s_colSEXP);
    Rcpp::traits::input_parameter< const Eigen::Matrix<double,-1,-1>& >::type mu(muSEXP);
    Rcpp::traits::input_parameter< const Eigen::Matrix<double,-1,-1>& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< std::ostream* >::type pstream__(pstream__SEXP);
    rcpp_result_gen = Rcpp::wrap(inv_normalize_cases_2(normalized_mat, case_idx, num_strata, num_delays, num_steps, tsize, d_col, s_col, mu, sigma, pstream__));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_stan_fit4generated_quantities_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4nowcasting_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_diseasenowcasting_count_cases", (DL_FUNC) &_diseasenowcasting_count_cases, 7},
    {"_diseasenowcasting_mean_cases", (DL_FUNC) &_diseasenowcasting_mean_cases, 8},
    {"_diseasenowcasting_sd_cases", (DL_FUNC) &_diseasenowcasting_sd_cases, 8},
    {"_diseasenowcasting_normalize_cases", (DL_FUNC) &_diseasenowcasting_normalize_cases, 10},
    {"_diseasenowcasting_inv_normalize_cases", (DL_FUNC) &_diseasenowcasting_inv_normalize_cases, 10},
    {"_diseasenowcasting_inv_normalize_cases_2", (DL_FUNC) &_diseasenowcasting_inv_normalize_cases_2, 11},
    {"_rcpp_module_boot_stan_fit4generated_quantities_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4generated_quantities_mod, 0},
    {"_rcpp_module_boot_stan_fit4nowcasting_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4nowcasting_mod, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_diseasenowcasting(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
