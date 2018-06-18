//// File Name: miceadds_rcpp_multilevel_mcmc.cpp
//// File Version: 0.8444


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

// [include_header_file]
#include "miceadds_rcpp_sampling_functions.h"

using namespace Rcpp;
using namespace arma;

// [[Rcpp::interfaces(r, cpp)]]



///********************************************************************
///** miceadds_rcpp_ml_mcmc_compute_xtx
// [[Rcpp::export]]
arma::mat miceadds_rcpp_ml_mcmc_compute_xtx( arma::mat X)
{
    int n=X.n_rows;
    int p=X.n_cols;
    arma::mat xtx(p,p);
    xtx.fill(0);
    for (int hh=0; hh<p; hh++){
        for (int jj=hh; jj<p; jj++){
            for (int nn=0; nn<n; nn++){
                xtx(hh,jj) += X(nn,hh)*X(nn,jj);
            }
            xtx(jj,hh) = xtx(hh,jj);
        }
    }
    //--- output
    return xtx;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_compute_ztz
// [[Rcpp::export]]
arma::mat miceadds_rcpp_ml_mcmc_compute_ztz( arma::mat Z, Rcpp::IntegerVector idcluster,
    int ncluster)
{
    int n=Z.n_rows;
    int p=Z.n_cols;
    arma::mat ztz(p*ncluster,p);
    ztz.fill(0);
    for (int hh=0; hh<p; hh++){
        for (int jj=hh; jj<p; jj++){
            for (int nn=0; nn<n; nn++){
                ztz(jj + idcluster[nn]*p,hh) += Z(nn,hh)*Z(nn,jj);
            }
            for (int cc=0; cc<ncluster; cc++){
                ztz(hh+cc*p,jj) = ztz(jj+cc*p,hh);
            }
        }
    }
    //--- output
    return ztz;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_crossprod
// [[RcppNOexport]]
arma::mat miceadds_rcpp_crossprod( arma::mat A, arma::mat B)
{
    // t(A) %*% B = R::crossprod(A,B)
    int NC_A = A.n_cols;
    int NR_A = A.n_rows;
    int NC_B = B.n_cols;
    arma::mat cp(NC_A, NC_B);
    cp.fill(0);
    for (int aa=0; aa<NC_A; aa++){
        for (int bb=0; bb<NC_B; bb++){
            for (int hh=0; hh<NR_A; hh++){
                cp(aa,bb) += A(hh,aa) * B(hh,bb);
            }
        }
    }
    //--- output
    return cp;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_predict_fixed
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_predict_fixed( arma::mat X, arma::colvec beta)
{
    arma::colvec pred = X * beta;
    //--- output
    return pred;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_subtract_fixed
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_subtract_fixed( arma::colvec y,
        arma::mat X, arma::colvec beta)
{
    arma::colvec pred = X * beta;
    arma::colvec ytilde = y - pred;
    //--- output
    return ytilde;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_predict_random
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_predict_random( arma::mat Z, arma::mat u,
    Rcpp::IntegerVector idcluster)
{
    int N=Z.n_rows;
    int NZ=Z.n_cols;
    arma::colvec pred(N);
    pred.fill(0);
    for (int nn=0; nn<N; nn++){
        for (int hh=0; hh<NZ; hh++){
            pred(nn,0) += Z(nn,hh)*u( idcluster[nn], hh);
        }
    }
    //--- output
    return pred;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_predict_random_list
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_predict_random_list( Rcpp::List Z_list, 
    Rcpp::List u_list, Rcpp::List idcluster_list, int NR, int N )
{
    arma::colvec pred(N);
    pred.fill(0);
    arma::colvec pred1(N);
    for (int rr=0; rr<NR; rr++){
        arma::mat Z = Rcpp::as< arma::mat >(Z_list[rr]);
        arma::mat u = Rcpp::as< arma::mat >(u_list[rr]);
        Rcpp::IntegerVector idcluster = Rcpp::as< Rcpp::IntegerVector >(idcluster_list[rr]);
        pred1 = miceadds_rcpp_ml_mcmc_predict_random( Z, u, idcluster);    
        pred = pred + pred1;
    }    
    //--- output
    return pred;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_predict_fixed_random
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_predict_fixed_random( arma::mat X, 
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, 
    Rcpp::List idcluster_list, int NR )
{
    arma::colvec pred1 = miceadds_rcpp_ml_mcmc_predict_fixed( X, beta);
    int N = X.n_rows;
    arma::colvec pred = miceadds_rcpp_ml_mcmc_predict_random_list( Z_list, 
                u_list, idcluster_list, NR, N );
    pred = pred + pred1;
    //--- output
    return pred;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_subtract_random
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_subtract_random( arma::colvec y,
        arma::mat Z, arma::mat u, Rcpp::IntegerVector idcluster,
        bool onlyintercept )
{
    arma::colvec ytilde = y;
    int N=Z.n_rows;
    int NZ=Z.n_cols;
    for (int nn=0; nn<N; nn++){
        if ( ! onlyintercept ){
            for (int hh=0; hh<NZ; hh++){
                ytilde(nn,0) += -Z(nn,hh)*u( idcluster[nn], hh);
            }
        } else {
            ytilde(nn,0) += - u( idcluster[nn], 0);
        }
    }
    //--- output
    return ytilde;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_beta
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_sample_beta( arma::mat xtx_inv,
    arma::mat X, Rcpp::List Z_list, arma::colvec y, Rcpp::List u_list,
    Rcpp::List idcluster_list, double sigma2,
    Rcpp::List onlyintercept_list, int NR)
{
    //** compute ytilde
    arma::colvec ytilde = y;
    for (int rr=0; rr<NR; rr++){
        arma::mat Z = Rcpp::as< arma::mat >(Z_list[rr]);
        arma::mat u = Rcpp::as< arma::mat >(u_list[rr]);
        Rcpp::IntegerVector idcluster = Rcpp::as< Rcpp::IntegerVector >(idcluster_list[rr]);
        bool onlyintercept = Rcpp::as< bool >(onlyintercept_list[rr]);
        ytilde = miceadds_rcpp_ml_mcmc_subtract_random( ytilde, Z, u,
                                idcluster, onlyintercept);
    }
    //** compute beta_hat
    arma::mat txy = miceadds_rcpp_crossprod(X, ytilde);
    arma::colvec beta_hat = xtx_inv * txy;
    //** compute D
    arma::mat D = sigma2 * xtx_inv;
    //** sample beta
    arma::colvec beta = miceadds_rcpp_mvrnorm( beta_hat, D);

    //--- output
    return beta;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_u
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_ml_mcmc_sample_u( arma::mat X, arma::colvec beta,
    Rcpp::List Z_list, arma::colvec y, Rcpp::List ztz_list,
    Rcpp::List idcluster_list, Rcpp::List ncluster_list,
    double sigma2, Rcpp::List Psi_list, Rcpp::List onlyintercept_list, int NR,
    Rcpp::List u0_list)
{
    //** subtract fixed effects
    arma::colvec ytilde0 = miceadds_rcpp_ml_mcmc_subtract_fixed( y, X, beta);
    Rcpp::List u_list(NR);
    arma::colvec ytilde=ytilde0;
    
    //** fill u with u0 elements
    for (int rr=0; rr<NR; rr++){
        u_list[rr] = Rcpp::as< arma::mat >(u0_list[rr]);
    }
    // random effects level rr
    for (int rr=0; rr<NR; rr++){
        ytilde = ytilde0;
        for (int hh=0; hh<NR; hh++){
            if ( hh != rr ){
                    arma::mat Z_hh = Rcpp::as< arma::mat >(Z_list[hh]);
                    arma::mat u_hh = Rcpp::as< arma::mat >(u_list[hh]);
                    Rcpp::IntegerVector idcluster_hh = Rcpp::as< Rcpp::IntegerVector >(idcluster_list[hh]);
                    bool onlyintercept_hh = Rcpp::as< bool >(onlyintercept_list[hh]);
                    ytilde = miceadds_rcpp_ml_mcmc_subtract_random( ytilde, Z_hh, u_hh,
                                    idcluster_hh, onlyintercept_hh);
            }
        }
        arma::mat Z = Rcpp::as< arma::mat >(Z_list[rr]);
        arma::mat Psi = Rcpp::as< arma::mat >(Psi_list[rr]);
        arma::mat ztz = Rcpp::as< arma::mat >(ztz_list[rr]);
        Rcpp::IntegerVector idcluster = Rcpp::as< Rcpp::IntegerVector >(idcluster_list[rr]);
        int ncluster = Rcpp::as< int >(ncluster_list[rr]);
        bool onlyintercept = Rcpp::as< bool >(onlyintercept_list[rr]);

        int NC_Z = Z.n_cols;
        int NR_Z = Z.n_rows;
        arma::mat u(ncluster, NC_Z);
        u.fill(0);
        arma::mat u_hat(ncluster, NC_Z);
        u_hat.fill(0);
        // compute mean and variance matrix for sampling
        arma::mat ze(NR_Z, NC_Z);
        arma::mat ze_sum(ncluster, NC_Z);
        ze.fill(0);
        ze_sum.fill(0);
        for (int nn=0; nn<NR_Z; nn++){
            for (int hh=0; hh<NC_Z; hh++){
                if (! onlyintercept){
                    ze(nn,hh) = Z(nn,hh)*ytilde(nn,0);
                } else {
                    ze(nn,hh) = ytilde(nn,0);
                }
                int cc_temp = idcluster[nn];
                ze_sum( cc_temp, hh ) += ze(nn,hh);
            }
        }
        arma::mat Psi_inv = arma::pinv(Psi);

        //****** sample u for all clusters cc
        arma::mat invmat(NC_Z, NC_Z);
        arma::colvec mu(NC_Z);
        arma::mat Sigma(NC_Z, NC_Z);
        arma::colvec u_samp(NC_Z);
        for (int cc=0; cc<ncluster; cc++){            
            for (int hh=0; hh<NC_Z; hh++){
                for (int uu=0; uu<NC_Z; uu++){
                    if (hh>=uu){
                        invmat(hh,uu) = ztz( cc*NC_Z+hh, uu) + sigma2*Psi_inv(hh,uu);
                        invmat(uu,hh) = invmat(hh,uu);
                    }
                }
            }
            invmat = arma::pinv(invmat);
            mu.fill(0);
            for (int hh=0; hh<NC_Z; hh++){
                for (int uu=0; uu<NC_Z; uu++){
                    mu(hh,0) += invmat(hh,uu)*ze_sum(cc,uu);
                }
            }
            Sigma = sigma2*invmat;
            u_samp = miceadds_rcpp_mvrnorm(mu, Sigma);            
            for (int hh=0; hh<NC_Z; hh++){
                u(cc,hh) = u_samp(hh,0);
            }
        }
        u_list[rr] = u;
    }
    //--- output
    return u_list;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_crossprod_one_matrix
// [[Rcpp::export]]
arma::mat miceadds_rcpp_crossprod_one_matrix(arma::mat X)
{
    int NC_X = X.n_cols;
    int NR_X = X.n_rows;
    arma::mat Z(NC_X, NC_X);
    Z.fill(0);
    for (int rr=0; rr<NC_X; rr++){
        for (int cc=0; cc<NC_X; cc++){
            if (rr>=cc){
                for (int hh=0; hh<NR_X; hh++){
                    Z(rr,cc) += X(hh,rr)*X(hh,cc);
                }
            }
            Z(cc,rr) = Z(rr,cc);
        }
    }
    return Z;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_covariance_matrix
// [[Rcpp::export]]
arma::mat miceadds_rcpp_ml_mcmc_sample_covariance_matrix( arma::mat u,
    int nu0, arma::mat S0 )
{
    arma::mat Su = miceadds_rcpp_crossprod_one_matrix(u);
    Su = Su + S0;
    int ncluster = u.n_rows;
    int df = ncluster + nu0;
    arma::mat covmat = miceadds_rcpp_riwishart(df, Su);
    //--- output
    return covmat;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_print_arma_mat
// [[Rcpp::export]]
void miceadds_rcpp_print_arma_mat( arma::mat x, int row1, int row2,
        int col1, int col2, int digits)
{    
    arma::mat y(row2-row1+1, col2-col1+1);
    int hh=0;
    int zz=0;
    for (int rr=row1; rr<row2+1; rr++){
        zz=0;
        for (int cc=col1; cc<col2+1; cc++){
            y(hh,zz) = ::Rf_fround( x(rr,cc), digits );
            zz++;
        }
        hh++;
    }    
    y.print("");
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_psi
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_ml_mcmc_sample_psi( Rcpp::List u_list,
    Rcpp::List nu0_list, Rcpp::List S0_list, int NR )
{
    Rcpp::List Psi_list(NR);
    for (int rr=0; rr<NR; rr++){
        arma::mat u = Rcpp::as< arma::mat >(u_list[rr]);
        int nu0 = Rcpp::as< int >(nu0_list[rr]);
        arma::mat S0 = Rcpp::as< arma::mat >(S0_list[rr]);        
        arma::mat Psi = miceadds_rcpp_ml_mcmc_sample_covariance_matrix( u, nu0, S0 );
        Psi_list[rr] = Psi;
    }
    //--- output
    return Psi_list;
}
///********************************************************************



///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_variance
// [[Rcpp::export]]
double miceadds_rcpp_ml_mcmc_sample_variance( arma::colvec e,
            int nu0, double sigma2_0 )
{
    arma::mat S0(1,1);
    S0(0,0) = sigma2_0;
    arma::mat covmat = miceadds_rcpp_ml_mcmc_sample_covariance_matrix( e, nu0, S0 );
    double samp = covmat(0,0);
    //--- output
    return samp;
}
///********************************************************************



///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_sigma2
// [[Rcpp::export]]
double miceadds_rcpp_ml_mcmc_sample_sigma2( arma::colvec y, arma::mat X,
        arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list,
        Rcpp::List idcluster_list, Rcpp::List onlyintercept_list,
        int nu0, double sigma2_0, int NR )
{
    arma::colvec e = miceadds_rcpp_ml_mcmc_subtract_fixed( y, X, beta);
    for (int hh=0; hh<NR; hh++){
        arma::mat Z_hh = Rcpp::as< arma::mat >(Z_list[hh]);
        arma::mat u_hh = Rcpp::as< arma::mat >(u_list[hh]);
        Rcpp::IntegerVector idcluster_hh = Rcpp::as< Rcpp::IntegerVector >(idcluster_list[hh]);
        bool onlyintercept_hh = Rcpp::as< bool >(onlyintercept_list[hh]);
        e = miceadds_rcpp_ml_mcmc_subtract_random( e, Z_hh, u_hh,
                                    idcluster_hh, onlyintercept_hh);
    }
    double samp = miceadds_rcpp_ml_mcmc_sample_variance( e, nu0, sigma2_0 );
    //--- output
    return samp;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_save_sampled_values
// [[RcppNOexport]]
Rcpp::NumericMatrix miceadds_rcpp_ml_mcmc_save_sampled_values(
        int NR,    Rcpp::List parameter_index, Rcpp::List est_parameter, int npar,
        arma::colvec beta, Rcpp::List Psi_list, double sigma2,
        Rcpp::NumericMatrix sampled_values, bool est_sigma2, int ss)
{
    if ( ss>=0 ){

        //-- save parameters
        arma::colvec index1 = parameter_index["beta"];
        int NI=index1.n_rows;
        for (int nn=0; nn<NI; nn++){
            sampled_values(ss,index1(nn,0)) = beta(nn,0);
        }

        Rcpp::List parameter_index_psi=parameter_index["Psi"];

        for (int rr=0; rr<NR; rr++){
            arma::mat index2 = parameter_index_psi[rr];
            arma::mat Psi_list_rr = Psi_list[rr];
            NI=index2.n_rows;
            for (int nn=0; nn<NI; nn++){
                for (int mm=0; mm<=nn; mm++){
                    sampled_values(ss,index2(nn,mm)) = Psi_list_rr(nn,mm);
                }
            }
        }
        if (est_sigma2){
            arma::colvec index1 = parameter_index["sigma2"];
            sampled_values(ss, index1(0,0) ) = sigma2;
        }
    }

    //--- output
    return sampled_values;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_print_progress
// [[RcppNOexport]]
int miceadds_rcpp_ml_mcmc_print_progress( int print_iter, int ii,
    int print_iter_temp)
{
    print_iter_temp ++;
    if ( print_iter_temp == print_iter){
        Rcpp::Rcout << "Iteration " << ii+1 << std::endl;
        print_iter_temp = 0;
    }
    //--- output
    return print_iter_temp;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_latent_probit
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_sample_latent_probit( arma::mat X, arma::colvec beta,
        Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list, int NR,
        Rcpp::IntegerVector y_int, arma::colvec alpha, double minval,
        double maxval )
{
    arma::colvec mu = miceadds_rcpp_ml_mcmc_predict_fixed_random( X, beta, Z_list, u_list, 
                    idcluster_list, NR );
    int N = y_int.size();
    arma::colvec sigma_probit(N);
    sigma_probit.fill(1);
    arma::colvec lower(N);
    arma::colvec upper(N);    
    for (int nn=0; nn<N; nn++){
        lower[nn] = alpha[ y_int[nn] ];
        upper[nn] = alpha[ y_int[nn]+1 ];
    }            
    // sample latent y
    arma::colvec y = miceadds_rcpp_rtnorm2( mu, 1.0, lower, upper, minval, maxval );                            
    //--- output
    return y;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_sampler
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_ml_mcmc_sampler(arma::colvec y_obs, arma::mat X,
        arma::mat xtx_inv, Rcpp::List ztz_list, Rcpp::List Z_list,
        arma::colvec beta_init, Rcpp::List Psi_list_init, double sigma2_init,
        arma::colvec alpha_init, Rcpp::List u_list_init, Rcpp::List idcluster_list, 
        Rcpp::List onlyintercept_list,
        Rcpp::List ncluster_list, int sigma2_nu0, double sigma2_sigma2_0,
        Rcpp::List psi_nu0_list, Rcpp::List psi_S0_list, int NR,
        bool est_sigma2, bool est_probit, Rcpp::List parameter_index, 
        Rcpp::List est_parameter,
        int npar, int iter, Rcpp::IntegerVector save_iter, bool verbose,
        int print_iter)
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
        // ....
        
        //-- save parameters
        sampled_values = miceadds_rcpp_ml_mcmc_save_sampled_values(
                NR,    parameter_index, est_parameter, npar, beta, Psi_list, sigma2,
                sampled_values, est_sigma2, save_iter[ii] );
                
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
                Rcpp::Named("sampled_values") = sampled_values
            );
}
///********************************************************************



//    return Rcpp::List::create(
//                Rcpp::Named("beta_hat") = beta_hat
//            );

// print armadillo objects 
// mu.print("mu:");

// Rcpp::Rcout << "verbose" << ii << std::endl;
