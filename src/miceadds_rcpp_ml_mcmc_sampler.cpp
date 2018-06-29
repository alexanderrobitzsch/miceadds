//// File Name: miceadds_rcpp_ml_mcmc_sampler.cpp
//// File Version: 0.894


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

// [include_header_file]
#include "miceadds_rcpp_ml_mcmc_sub.h"

using namespace Rcpp;
using namespace arma;

// [[Rcpp::interfaces(r, cpp)]]



///********************************************************************
///** miceadds_rcpp_ml_mcmc_sampler
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_ml_mcmc_sampler( arma::colvec y_obs, arma::mat X,
    arma::mat xtx_inv, Rcpp::List ztz_list, Rcpp::List Z_list, arma::colvec beta_init,
    Rcpp::List Psi_list_init, double sigma2_init, arma::colvec alpha_init,
    Rcpp::List u_list_init, Rcpp::List idcluster_list, Rcpp::List onlyintercept_list,
    Rcpp::List ncluster_list, int sigma2_nu0, double sigma2_sigma2_0,
    Rcpp::List psi_nu0_list, Rcpp::List psi_S0_list, int NR, bool est_sigma2,
    bool est_probit, Rcpp::List parameter_index, Rcpp::List est_parameter, int npar,
    int iter, Rcpp::IntegerVector save_iter, bool verbose, int print_iter,
    bool est_thresh, int K, Rcpp::NumericVector sd_proposal )
{
    //--- handle inits
    arma::colvec y = y_obs;
    arma::colvec beta = beta_init;
    arma::colvec alpha = alpha_init;
    Rcpp::List Psi_list = Psi_list_init;
    double sigma2 = sigma2_init;
    Rcpp::List u_list = u_list_init;
    int N = y.n_rows;
    Rcpp::IntegerVector y_int(N);
    y_int.fill(999);
    //--- integer vector for ordinal data
    if (est_probit){
        for (int nn=0;nn<N; nn++){
            y_int[nn] = y(nn,0);
        }
    }
    //--- matrix with saved parameter values
    int NSV = Rcpp::max(save_iter) + 1;
    Rcpp::NumericMatrix sampled_values(NSV, npar);
    int print_iter_temp=0;

    //************
    //**** MCMC sampling algorithm
    for (int ii=0; ii<iter; ii++){

        //-- sampling y in case of probit model
        if (est_probit){
            y = miceadds_rcpp_ml_mcmc_sample_latent_probit( X, beta,
                    Z_list, u_list, idcluster_list, NR, y_int, alpha, .001, .999);
        }

        //-- sampling beta
        beta = miceadds_rcpp_ml_mcmc_sample_beta( xtx_inv, X, Z_list, y, u_list,
                    idcluster_list, sigma2, onlyintercept_list, NR);

        //-- sampling Psi matrix
        Psi_list = miceadds_rcpp_ml_mcmc_sample_psi( u_list, psi_nu0_list, psi_S0_list, NR );

        //-- sample random effects u
        u_list = miceadds_rcpp_ml_mcmc_sample_u( X, beta, Z_list, y, ztz_list, idcluster_list,
                        ncluster_list, sigma2, Psi_list, onlyintercept_list, NR, u_list );

        //-- sample sigma2
        if (est_sigma2){
            sigma2 = miceadds_rcpp_ml_mcmc_sample_sigma2( y, X,  beta, Z_list, u_list,
                    idcluster_list, onlyintercept_list, sigma2_nu0, sigma2_sigma2_0, NR );
        }

        //-- sample thresholds in case of ordinal data
        if (est_thresh){
            alpha = miceadds_rcpp_ml_mcmc_sample_thresholds( X, beta, Z_list, u_list,
                idcluster_list, NR, K, alpha, sd_proposal, y_int );
        }

        //-- save parameters
        sampled_values = miceadds_rcpp_ml_mcmc_save_sampled_values(
                NR,    parameter_index, est_parameter, npar, beta, Psi_list, sigma2,
                sampled_values, est_sigma2, save_iter[ii], est_thresh, K, alpha );

        //-- print progress (verbose = TRUE)
        if (verbose){
            print_iter_temp = miceadds_rcpp_ml_mcmc_print_progress( print_iter, ii,
                        print_iter_temp);
        }

    }

    //--- output
    return Rcpp::List::create(
                Rcpp::Named("beta") = beta,
                Rcpp::Named("Psi_list") = Psi_list,
                Rcpp::Named("sigma2") = sigma2,
                Rcpp::Named("alpha") = alpha,
                Rcpp::Named("y") = y,
                Rcpp::Named("y_int") = y_int,
                Rcpp::Named("NR") = NR,
                Rcpp::Named("u_list") = u_list,
                Rcpp::Named("sampled_values") = sampled_values,
                Rcpp::Named("N") = N,
                Rcpp::Named("ncluster_list") = ncluster_list,
                Rcpp::Named("sd_proposal") = sd_proposal
            );
}
///********************************************************************



//    return Rcpp::List::create(
//                Rcpp::Named("beta_hat") = beta_hat
//            );

// print armadillo objects
// mu.print("mu:");

// Rcpp::Rcout << "verbose" << ii << std::endl;
