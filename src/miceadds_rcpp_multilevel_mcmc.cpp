//// File Name: miceadds_rcpp_multilevel_mcmc.cpp
//// File Version: 0.732


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
// [[RcppNOexport]]
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
        arma::mat Psi_inv = arma::inv(Psi);

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
            invmat = arma::inv(invmat);
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



//    return Rcpp::List::create(
//                Rcpp::Named("beta_hat") = beta_hat
//            );
