
########################################################
# Call to Rcpp function
ma.scale2 <- function (x , missings =FALSE ){ 
    x_ <- as.matrix(x)
	if ( ! missings ){
		res <- scale2_C( x_ )
	} else {
		res <- scale2_NA_C( x_ )
	}
	colnames(res) <- colnames(x)
	return(res)
}
##########################################################
