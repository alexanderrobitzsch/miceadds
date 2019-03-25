//// File Name: miceadds_rcpp_ml_mcmc_sub.h
//// File Version: 0.888

#ifndef _MICEADDS_MICEADDS_RCPP_ML_MCMC_SUB_H
#define _MICEADDS_MICEADDS_RCPP_ML_MCMC_SUB_H
 
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

arma::mat miceadds_rcpp_arma_chol_ridge(arma::mat sigma0, double ridge);

arma::colvec miceadds_rcpp_mvrnorm(arma::colvec mu, arma::mat sigma, double ridge);

arma::mat miceadds_rcpp_rwishart(int df, arma::mat S, double ridge);

arma::mat miceadds_rcpp_riwishart(int df, arma::mat S, double ridge);

double miceadds_rcpp_rtnorm_double( double mu, double sigma, double lower, double upper );

arma::colvec miceadds_rcpp_rtnorm( arma::colvec mu, arma::colvec sigma,
    arma::colvec lower, arma::colvec upper );

Rcpp::NumericVector miceadds_rcpp_arma2vec(arma::colvec x);

Rcpp::NumericVector miceadds_rcpp_pnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma);

Rcpp::NumericVector miceadds_rcpp_qnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma);

arma::colvec miceadds_rcpp_rtnorm2( arma::colvec mu, double sigma0,
    arma::colvec lower, arma::colvec upper, double minval, double maxval );

arma::mat miceadds_rcpp_ml_mcmc_compute_xtx( arma::mat X);

arma::mat miceadds_rcpp_ml_mcmc_compute_ztz( arma::mat Z, Rcpp::IntegerVector idcluster,
    int ncluster);

arma::mat miceadds_rcpp_crossprod( arma::mat A, arma::mat B);

arma::colvec miceadds_rcpp_ml_mcmc_predict_fixed( arma::mat X, arma::colvec beta);

arma::colvec miceadds_rcpp_ml_mcmc_subtract_fixed( arma::colvec y,
        arma::mat X, arma::colvec beta);

arma::colvec miceadds_rcpp_ml_mcmc_predict_random( arma::mat Z, arma::mat u,
    Rcpp::IntegerVector idcluster);

arma::colvec miceadds_rcpp_ml_mcmc_predict_random_list( Rcpp::List Z_list,
    Rcpp::List u_list, Rcpp::List idcluster_list, int NR, int N );

arma::colvec miceadds_rcpp_ml_mcmc_predict_fixed_random( arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list,
    Rcpp::List idcluster_list, int NR );

arma::colvec miceadds_rcpp_ml_mcmc_subtract_random( arma::colvec y, arma::mat Z,
    arma::mat u, Rcpp::IntegerVector idcluster, bool onlyintercept );

arma::colvec miceadds_rcpp_ml_mcmc_sample_beta( arma::mat xtx_inv, arma::mat X,
    Rcpp::List Z_list, arma::colvec y, Rcpp::List u_list, Rcpp::List idcluster_list,
    double sigma2, Rcpp::List onlyintercept_list, int NR, double ridge );

Rcpp::List miceadds_rcpp_ml_mcmc_sample_u( arma::mat X, arma::colvec beta,
    Rcpp::List Z_list, arma::colvec y, Rcpp::List ztz_list, Rcpp::List idcluster_list,
    Rcpp::List ncluster_list, double sigma2, Rcpp::List Psi_list,
    Rcpp::List onlyintercept_list, int NR, Rcpp::List u0_list, double ridge );

arma::mat miceadds_rcpp_crossprod_one_matrix(arma::mat X);

arma::mat miceadds_rcpp_ml_mcmc_sample_covariance_matrix( arma::mat u,
    int nu0, arma::mat S0, double ridge );

Rcpp::List miceadds_rcpp_ml_mcmc_sample_psi( Rcpp::List u_list,
    Rcpp::List nu0_list, Rcpp::List S0_list, int NR, double ridge );

double miceadds_rcpp_ml_mcmc_sample_variance( arma::colvec e,
            int nu0, double sigma2_0, double ridge );

double miceadds_rcpp_ml_mcmc_sample_sigma2( arma::colvec y, arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list,
    Rcpp::List onlyintercept_list, int nu0, double sigma2_0, int NR, double ridge );

Rcpp::NumericMatrix miceadds_rcpp_ml_mcmc_save_sampled_values( int NR,
    Rcpp::List parameter_index, Rcpp::List est_parameter, int npar, arma::colvec beta,
    Rcpp::List Psi_list, double sigma2, Rcpp::NumericMatrix sampled_values,
    bool est_sigma2, int ss, bool est_thresh, int K, arma::colvec alpha );

int miceadds_rcpp_ml_mcmc_print_progress( int print_iter, int ii,
    int print_iter_temp);

arma::colvec miceadds_rcpp_ml_mcmc_sample_latent_probit( arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list,
    int NR, Rcpp::IntegerVector y_int, arma::colvec alpha, double minval, double maxval );

Rcpp::NumericVector miceadds_rcpp_ml_mcmc_probit_fill_index_lower( Rcpp::IntegerVector y_int,
    arma::colvec alpha );

Rcpp::NumericVector miceadds_rcpp_ml_mcmc_probit_fill_index_upper( Rcpp::IntegerVector y_int,
        arma::colvec alpha );

Rcpp::NumericVector miceadds_rcpp_ml_mcmc_probit_category_prob( Rcpp::IntegerVector y_int,
    arma::colvec alpha, Rcpp::NumericVector mu1, bool use_log );

double miceadds_rcpp_ml_mcmc_probit_loglike( Rcpp::IntegerVector y_int,
    arma::colvec alpha, Rcpp::NumericVector mu1, bool use_log );

double miceadds_rcpp_rnorm_double( double mu, double sigma );

arma::colvec miceadds_rcpp_ml_mcmc_sample_thresholds( arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list,
    int NR, int K, arma::colvec alpha, Rcpp::NumericVector sd_proposal,
    Rcpp::IntegerVector y_int );

#endif // _MICEADDS_MICEADDS_RCPP_ML_MCMC_SUB_H
