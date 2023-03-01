//// File Name: miceadds_rcpp_ml_mcmc_sub.cpp
//// File Version: 0.897


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>


using namespace Rcpp;
using namespace arma;

// [[Rcpp::interfaces(r, cpp)]]



///********************************************************************
///** miceadds_rcpp_arma_chol_ridge
// [[Rcpp::export]]
arma::mat miceadds_rcpp_arma_chol_ridge(arma::mat sigma0, double ridge)
{
    arma::mat sigma=sigma0;
    int np = sigma.n_rows;
    for (int pp=0; pp<np; pp++){
        sigma(pp,pp) = sigma0(pp,pp) * ( 1 + ridge );
        if (sigma(pp,pp)<ridge){
            sigma(pp,pp) = ridge;
        }
    }
    arma::mat sigma_chol = arma::chol(sigma);
    return sigma_chol;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_mvrnorm
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_mvrnorm(arma::colvec mu, arma::mat sigma, double ridge)
{
    int ncols = sigma.n_cols;
    arma::vec Y = arma::randn(ncols);
    arma::mat sigma_chol = miceadds_rcpp_arma_chol_ridge(sigma, ridge);
    arma::colvec samp = mu + sigma_chol * Y;
    return samp;
}
///********************************************************************

///********************************************************************
//*** copied from
//*** https://github.com/coatless/r-to-armadillo/blob/master/src/distributions.cpp
///** miceadds_rcpp_rwishart
// [[Rcpp::export]]
arma::mat miceadds_rcpp_rwishart(int df, arma::mat S, double ridge)
{
    // Dimension of returned wishart
    int m = S.n_rows;
    // Z composition:
    // sqrt chisqs on diagonal
    // random normals below diagonal
    // misc above diagonal
    arma::mat Z(m,m);
    // Fill the diagonal
    for(int i = 0; i < m; i++){
        Z(i,i) = sqrt( ::Rf_rchisq(df-i) );
    }
    // Fill the lower matrix with random guesses
    for(int j = 0; j < m; j++){
        for(int i = j+1; i < m; i++){
            Z(i,j) = ::Rf_rnorm(0, 1);
        }
    }
    // Lower triangle * chol decomp
    arma::mat S_chol = miceadds_rcpp_arma_chol_ridge(S, ridge);
    arma::mat C = arma::trans( arma::trimatl(Z) ) * S_chol;
    // Return random wishart
    arma::mat samp = arma::trans(C)*C;
    return samp;
}
///********************************************************************

///********************************************************************
//*** copied from
//*** https://github.com/coatless/r-to-armadillo/blob/master/src/distributions.cpp
///** miceadds_rcpp_riwishart
// [[Rcpp::export]]
arma::mat miceadds_rcpp_riwishart(int df, arma::mat S, double ridge)
{
    arma::mat S_inv = arma::pinv(S);
    arma::mat samp = miceadds_rcpp_rwishart(df, S_inv, ridge);
    samp = arma::pinv(samp);
    return samp;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_rtnorm_double
// [[Rcpp::export]]
double miceadds_rcpp_rtnorm_double( double mu, double sigma, double lower, double upper )
{
    double y1=0;
    double y2=0;
    double temp=0;
    Rcpp::NumericVector val = Rcpp::runif(1);
    y1 = ::Rf_pnorm5(lower, mu, sigma, 1, 0);
    y2 = ::Rf_pnorm5(upper, mu, sigma, 1, 0);
    temp = y1 + val[0] * ( y2 - y1 );
    double z = ::Rf_qnorm5(temp, mu, sigma, 1, 0);
    //--- output
    return z;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_rtnorm
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_rtnorm( arma::colvec mu, arma::colvec sigma,
    arma::colvec lower, arma::colvec upper )
{
    int N = mu.size();
    arma::colvec z(N);
    for (int nn=0; nn<N; nn++){
        z(nn,0) = miceadds_rcpp_rtnorm_double(mu(nn,0), sigma(nn,0),
                        lower(nn,0), upper(nn,0) );
    }
    //--- output
    return z;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_arma2vec
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_arma2vec(arma::colvec x)
{
    Rcpp::NumericVector y = Rcpp::NumericVector(x.begin(), x.end());
    return y;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_pnorm
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_pnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma)
{
    Rcpp::NumericVector x1 = x - mu;
    Rcpp::NumericVector y = Rcpp::pnorm( x1, 0.0, sigma);
    //--- output
    return y;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_qnorm
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_qnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma)
{
    Rcpp::NumericVector y = Rcpp::qnorm( x, 0.0, sigma);
    y = y + mu;
    //--- output
    return y;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_rtnorm2
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_rtnorm2( arma::colvec mu, double sigma0,
    arma::colvec lower, arma::colvec upper, double minval, double maxval )
{
    int N = mu.size();
    Rcpp::NumericVector val = Rcpp::runif(N);
    Rcpp::NumericVector lower1 = miceadds_rcpp_arma2vec(lower);
    Rcpp::NumericVector upper1 = miceadds_rcpp_arma2vec(upper);
    Rcpp::NumericVector mu1 = miceadds_rcpp_arma2vec(mu);
    Rcpp::NumericVector y1 = miceadds_rcpp_pnorm( lower1, mu1, sigma0);
    Rcpp::NumericVector y2 = miceadds_rcpp_pnorm( upper1, mu1, sigma0);
    Rcpp::NumericVector temp(N);
    for (int nn=0; nn<N; nn++){
        if (y1[nn] < minval){ y1[nn] = minval; }
        if (y2[nn] > maxval){ y2[nn] = maxval; }
        temp[nn] = y1[nn] + val[nn] * ( y2[nn] - y1[nn] );
    }
    Rcpp::NumericVector z0 = miceadds_rcpp_qnorm( temp, mu1, sigma0);
    arma::colvec z = Rcpp::as< arma::colvec >(z0);
    //--- output
    return z;
}
///********************************************************************



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
arma::colvec miceadds_rcpp_ml_mcmc_subtract_random( arma::colvec y, arma::mat Z,
    arma::mat u, Rcpp::IntegerVector idcluster, bool onlyintercept )
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
arma::colvec miceadds_rcpp_ml_mcmc_sample_beta( arma::mat xtx_inv, arma::mat X,
    Rcpp::List Z_list, arma::colvec y, Rcpp::List u_list, Rcpp::List idcluster_list,
    double sigma2, Rcpp::List onlyintercept_list, int NR, double ridge )
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
    arma::colvec beta = miceadds_rcpp_mvrnorm( beta_hat, D, ridge);

    //--- output
    return beta;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_u
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_ml_mcmc_sample_u( arma::mat X, arma::colvec beta,
    Rcpp::List Z_list, arma::colvec y, Rcpp::List ztz_list, Rcpp::List idcluster_list,
    Rcpp::List ncluster_list, double sigma2, Rcpp::List Psi_list,
    Rcpp::List onlyintercept_list, int NR, Rcpp::List u0_list, double ridge )
{
    //** subtract fixed effects
    arma::colvec ytilde0 = miceadds_rcpp_ml_mcmc_subtract_fixed( y, X, beta);
    Rcpp::List u_list(NR);
    arma::colvec ytilde=ytilde0;

    //** fill u with u0 elements
    for (int rr=0; rr<NR; rr++){
        u_list[rr] = Rcpp::as< arma::mat >(u0_list[rr]);
    } // end rr
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
            }  // end if
        } // end hh
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
            } // end hh
        } // end nn
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
            } // end hh
            invmat = arma::pinv(invmat);
            mu.fill(0);
            for (int hh=0; hh<NC_Z; hh++){
                for (int uu=0; uu<NC_Z; uu++){
                    mu(hh,0) += invmat(hh,uu)*ze_sum(cc,uu);
                }
            } // end hh
            Sigma = sigma2*invmat;
            u_samp = miceadds_rcpp_mvrnorm(mu, Sigma, ridge);
            for (int hh=0; hh<NC_Z; hh++){
                u(cc,hh) = u_samp(hh,0);
            } // end hh
        }  // end cc
        u_list[rr] = u;
    } // end rr
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
    int nu0, arma::mat S0, double ridge )
{
    arma::mat Su = miceadds_rcpp_crossprod_one_matrix(u);
    Su = Su + S0;
    int ncluster = u.n_rows;
    int df = ncluster + nu0;
    arma::mat covmat = miceadds_rcpp_riwishart(df, Su, ridge);
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
    Rcpp::List nu0_list, Rcpp::List S0_list, int NR, double ridge )
{
    Rcpp::List Psi_list(NR);
    for (int rr=0; rr<NR; rr++){
        arma::mat u = Rcpp::as< arma::mat >(u_list[rr]);
        int nu0 = Rcpp::as< int >(nu0_list[rr]);
        arma::mat S0 = Rcpp::as< arma::mat >(S0_list[rr]);
        arma::mat Psi = miceadds_rcpp_ml_mcmc_sample_covariance_matrix( u, nu0,
                                S0, ridge );
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
            int nu0, double sigma2_0, double ridge )
{
    arma::mat S0(1,1);
    S0(0,0) = sigma2_0;
    arma::mat covmat = miceadds_rcpp_ml_mcmc_sample_covariance_matrix( e, nu0,
                                S0, ridge );
    double samp = covmat(0,0);
    //--- output
    return samp;
}
///********************************************************************



///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_sigma2
// [[Rcpp::export]]
double miceadds_rcpp_ml_mcmc_sample_sigma2( arma::colvec y, arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list,
    Rcpp::List onlyintercept_list, int nu0, double sigma2_0, int NR, double ridge )
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
    double samp = miceadds_rcpp_ml_mcmc_sample_variance( e, nu0, sigma2_0, ridge );
    //--- output
    return samp;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_save_sampled_values
// [[RcppNOexport]]
Rcpp::NumericMatrix miceadds_rcpp_ml_mcmc_save_sampled_values( int NR,
    Rcpp::List parameter_index, Rcpp::List est_parameter, int npar, arma::colvec beta,
    Rcpp::List Psi_list, double sigma2, Rcpp::NumericMatrix sampled_values,
    bool est_sigma2, int ss, bool est_thresh, int K, arma::colvec alpha )
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
            arma::colvec index3 = parameter_index["sigma2"];
            sampled_values(ss, index3(0,0) ) = sigma2;
        }
        if (est_thresh){
            arma::colvec index4 = parameter_index["thresh"];
            NI=index4.n_rows;
            for (int hh=0; hh<NI; hh++){
                sampled_values(ss, index4(hh,0) ) = alpha(hh+2,0);
            }
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
arma::colvec miceadds_rcpp_ml_mcmc_sample_latent_probit( arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list,
    int NR, Rcpp::IntegerVector y_int, arma::colvec alpha, double minval, double maxval )
{
    arma::colvec mu = miceadds_rcpp_ml_mcmc_predict_fixed_random( X, beta, Z_list, u_list,
                    idcluster_list, NR );
    int N = y_int.size();
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
///** miceadds_rcpp_ml_mcmc_probit_fill_index_lower
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_ml_mcmc_probit_fill_index_lower(
            Rcpp::IntegerVector y_int, arma::colvec alpha )
{
    int N = y_int.size();
    Rcpp::NumericVector lower(N);
    for (int nn=0; nn<N; nn++){
        lower[nn] = alpha( y_int[nn], 0);
    }
    //--- output
    return lower;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_probit_fill_index_upper
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_ml_mcmc_probit_fill_index_upper(
        Rcpp::IntegerVector y_int, arma::colvec alpha )
{
    int N = y_int.size();
    Rcpp::NumericVector upper(N);
    for (int nn=0; nn<N; nn++){
        upper[nn] = alpha( y_int[nn]+1, 0);
    }
    //--- output
    return upper;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_probit_category_prob
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_ml_mcmc_probit_category_prob( Rcpp::IntegerVector y_int,
    arma::colvec alpha, Rcpp::NumericVector mu1, bool use_log )
{
    double sigma0 = 1;
    int N = y_int.size();
    Rcpp::NumericVector lower = miceadds_rcpp_ml_mcmc_probit_fill_index_lower( y_int, alpha );
    Rcpp::NumericVector upper = miceadds_rcpp_ml_mcmc_probit_fill_index_upper( y_int, alpha );
    Rcpp::NumericVector y1 = miceadds_rcpp_pnorm( lower, mu1, sigma0);
    Rcpp::NumericVector y2 = miceadds_rcpp_pnorm( upper, mu1, sigma0);
    Rcpp::NumericVector diff = y2 - y1;
    if (use_log){
        double eps = 1e-60;
        for (int nn=0; nn<N; nn++){
            diff[nn] = std::log( diff[nn] + eps );
        }
    }
    //--- output
    return diff;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_ml_mcmc_probit_loglike
// [[Rcpp::export]]
double miceadds_rcpp_ml_mcmc_probit_loglike( Rcpp::IntegerVector y_int,
    arma::colvec alpha, Rcpp::NumericVector mu1, bool use_log )
{
    double sigma0 = 1;
    int N = y_int.size();
    Rcpp::NumericVector lower = miceadds_rcpp_ml_mcmc_probit_fill_index_lower( y_int, alpha );
    Rcpp::NumericVector upper = miceadds_rcpp_ml_mcmc_probit_fill_index_upper( y_int, alpha );
    Rcpp::NumericVector y1 = miceadds_rcpp_pnorm( lower, mu1, sigma0);
    Rcpp::NumericVector y2 = miceadds_rcpp_pnorm( upper, mu1, sigma0);
    Rcpp::NumericVector diff = y2 - y1;
    double eps = 1e-60;
    double val=0;
    for (int nn=0; nn<N; nn++){
        val += std::log( diff[nn] + eps );
    }
    //--- output
    return val;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_rnorm_double
// [[Rcpp::export]]
double miceadds_rcpp_rnorm_double( double mu, double sigma )
{
    double samp = mu + ::Rf_rnorm(0,1) * sigma;
    //--- output
    return samp;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_ml_mcmc_sample_thresholds
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_ml_mcmc_sample_thresholds( arma::mat X,
    arma::colvec beta, Rcpp::List Z_list, Rcpp::List u_list, Rcpp::List idcluster_list,
    int NR, int K, arma::colvec alpha, Rcpp::NumericVector sd_proposal,
    Rcpp::IntegerVector y_int )
{
    arma::colvec mu = miceadds_rcpp_ml_mcmc_predict_fixed_random( X, beta, Z_list, u_list,
                    idcluster_list, NR );
    Rcpp::NumericVector mu1 = miceadds_rcpp_arma2vec(mu);
    arma::colvec alpha1(K+2);
    for (int hh=0; hh<K+2; hh++){
        alpha1(hh,0) = alpha(hh,0);
    }

    bool accept=FALSE;
    double prob0=0;
    double prob1=0;
    double mh_logratio=0;
    prob0 = miceadds_rcpp_ml_mcmc_probit_loglike( y_int, alpha, mu1, TRUE );

    for (int kk=2; kk<K+1; kk++){
        alpha1(kk,0) = miceadds_rcpp_rnorm_double( alpha(kk,0), sd_proposal[kk] );
        accept = FALSE;
        if ( ( alpha1(kk,0) > alpha1(kk-1,0) ) && ( alpha1(kk,0) < alpha1(kk+1,0) ) ){
            prob1 = miceadds_rcpp_ml_mcmc_probit_loglike( y_int, alpha1, mu1, TRUE );
            mh_logratio = prob1 - prob0;
            if ( mh_logratio > 0 ){    accept = TRUE; }
            if ( ! accept ){ accept = ( ::Rf_runif(0, 1) < std::exp( mh_logratio ) ); }
            if ( accept ){
                alpha(kk,0) = alpha1(kk,0);
                prob0 = prob1;
            }
        }
    }

    //--- output
    return alpha;
}
///********************************************************************


//    return Rcpp::List::create(
//                Rcpp::Named("beta_hat") = beta_hat
//            );

// print armadillo objects
// mu.print("mu:");

// Rcpp::Rcout << "verbose" << ii << std::endl;
