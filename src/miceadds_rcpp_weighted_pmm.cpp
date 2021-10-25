//// File Name: miceadds_rcpp_weighted_pmm.cpp
//// File Version: 0.292

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <queue>

using namespace Rcpp;
using namespace arma;
// using namespace std;


///********************************************************************
/// Function copied and slightly adapted from
/// http://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/
///** miceadds_rcpp_sort_partial_index
// [[Rcpp::export]]
Rcpp::IntegerVector miceadds_rcpp_sort_partial_index( Rcpp::NumericVector v, int n)
{
    typedef std::pair<double, int> Elt;
    std::priority_queue< Elt, std::vector<Elt>, std::greater<Elt> > pq;
    std::vector<int> result;
    int p2 = pq.size();
    for (int i = 0; i != v.size(); ++i) {
        if (p2 < n){
            pq.push(Elt(v[i], i));
        } else {
            Elt elt = Elt(v[i], i);
            if (pq.top() < elt) {
                pq.pop();
                pq.push(elt);
            }
        }
    }
    result.reserve(pq.size());
    while (!pq.empty()) {
        result.push_back(pq.top().second + 1);
        pq.pop();
    }
    Rcpp::IntegerVector out(n);
    for (int ii=0; ii<n; ii++){
        out[ii] = result[ii] - 1;
    }
    return out;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_weighted_pmm_match_case
// [[Rcpp::export]]
int miceadds_rcpp_weighted_pmm_match_case( Rcpp::NumericVector ds,
    Rcpp::NumericVector weights_obs, int donors, Rcpp::IntegerVector donor_ind)
{
    Rcpp::IntegerVector out = miceadds_rcpp_sort_partial_index(ds, donors);
    Rcpp::NumericVector weights_sel(donors);
    double W = 0;
    for (int hh=0; hh<donors; hh++){
        weights_sel[hh] = weights_obs[ out[hh] ];
        W += weights_obs[ out[hh] ];
    }
    weights_sel = weights_sel / W;
    Rcpp::IntegerVector samp_index1 = Rcpp::sample(donor_ind, 1, FALSE, weights_sel);
    int samp_index = out[samp_index1[0]];
    return samp_index;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_weighted_pmm_match
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_weighted_pmm_match( Rcpp::NumericVector yhatmis,
    Rcpp::NumericVector yhatobs, Rcpp::NumericVector yobs,
    Rcpp::NumericVector weights_obs, int donors)
{
    int nmis = yhatmis.size();
    Rcpp::NumericVector yimp(nmis);
    int nobs = yhatobs.size();
    Rcpp::NumericVector ds(nobs);
    Rcpp::IntegerVector donor_ind(donors);
    double z=0;
    for (int dd=0; dd<donors; dd++){
        donor_ind[dd] = dd;
    }
    int samp_index = 0;
    for (int nn=0; nn<nmis; nn++){
        z = yhatmis[nn];
        for (int oo=0; oo<nobs; oo++){
            // ds[oo] = std::abs(z-yobs[oo]);
            // correction thanks to @jarretrt, Github Issue #23
            ds[oo] = std::abs(z-yhatobs[oo]);
        }
        samp_index = miceadds_rcpp_weighted_pmm_match_case(ds, weights_obs, donors, donor_ind);
        yimp[nn] = yobs[samp_index];
    }
    return yimp;
}
///********************************************************************
