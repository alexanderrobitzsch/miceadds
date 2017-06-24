

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** create_interactions_cpp
// [[Rcpp::export]]
Rcpp::List create_interactions_cpp( Rcpp::NumericVector Yr , 
	Rcpp::NumericMatrix Xr, Rcpp::NumericMatrix Xallr, 
	Rcpp::NumericMatrix index_int , 
	Rcpp::NumericVector MI , Rcpp::NumericVector maxcols ){

     int nobj = Xr.nrow();   
     int nall = Xallr.nrow();  
     int npred = Xr.ncol();  
     int nresp = 1 ;  
     int NI = index_int.nrow() ;  
     double min_int_cor = MI[0] ;  
       
     // create vector with correlations  
     Rcpp::NumericMatrix allcorrs(NI,2);         
     arma::mat xobs(Xr.begin(), nobj, npred, false);  
     arma::mat Y(Yr.begin(), nobj, nresp , false);   
     arma::mat xall(Xallr.begin(), nall, npred, false);     
       
     // matrix of interactions  
     arma::mat IM = arma::zeros( nall , maxcols[0]  );   
     arma::mat xint;  
       
     // create interaction vector in arma  
     arma::mat xxi = arma::zeros( nobj , 1  );  
     arma::mat xxi2 = arma::zeros( nall , 1  );  
       
     arma::mat cii = arma::zeros( 1 , 1  );  
       
     int zz = 0 ; // init zz: the number of interactions  
       
     for (int nn=0;nn<NI;nn++){  
     	// int nn = 0 ;   
     	// create vector with interactions  
     	xxi = arma::mat( xobs.col( index_int(nn,0)-1 ) % xobs.col( index_int(nn,1)-1 ) ) ;   
     	cii = arma::abs( arma::cor( Y , xxi )  );  
     	allcorrs(nn,0) =  cii(0,0);  
     	if ( cii(0,0) > min_int_cor ){  
     		xxi2 = arma::mat( xall.col( index_int(nn,0)-1 ) % xall.col( index_int(nn,1)-1 ) ) ;	  
     		IM.col(zz) = xxi2.col(0);   
     		allcorrs(nn,1)=1 ;  
     		zz = zz + 1 ;  
			if ( zz > maxcols[0] -1 ){  
				nn = NI ;  
			}  
		}  
     }  
       
     // select only relevant columns;  
     if (zz > 0 ){  
         xint = IM( arma::span(0,nall-1) , arma::span(0,zz-1) ) ;   
     }   
     			  
     ////////////////////////////////////  
     // OUTPUT:  
     return Rcpp::List::create(  
            Rcpp::Named("index_int") = index_int,  
            Rcpp::Named("xint") = xint ,  
            Rcpp::Named("allcorrs") = allcorrs ,  
            Rcpp::Named("min_int_cor") = min_int_cor ,  
            Rcpp::Named("N_interactions") = zz  
            		) ;  
}



