## File Name: mice.impute.pmm6.R
## File Version: 0.06
mice.impute.pmm6 <- function (y, ry, x, donors=3 , noise = 10^5 , ridge = 10^(-5) , ...){
    x <- cbind(1, as.matrix(x))
	# dummy response indicator 
	ry01 <- 1*ry
	# sample regression coefficients
	coefu <- stats::rnorm( ncol(x) )
	# create donor samples
	donorsample <- sample( 1:(2*donors) , sum( ! ry) , replace=TRUE) - donors
	donorsample <- ifelse( donorsample <= 0 , donorsample - 1 , donorsample )
	# Rcpp code
	# SEXP ma_pmm6_C( SEXP y_, SEXP ry01_, SEXP x_, SEXP ridge_, 
	#          SEXP coefu_, SEXP donorsample_) ;	
	imp <- ma_pmm6_C(y ,  ry01 , x, ridge , coefu , donorsample	)
    return(imp)
}
