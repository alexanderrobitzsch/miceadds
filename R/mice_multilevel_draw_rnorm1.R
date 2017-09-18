## File Name: mice_multilevel_draw_rnorm1.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:49

########################################################################
# draw a vector of random variables using a Cholesky decomposition
# input mean vector and covariance matrix
mice_multilevel_draw_rnorm1 <- function( mu  , Sigma){	
	#----	
	#b.star <- b.star + as.vector( t(chol(vcov(fit))) %*% rnorm(length(b.star)) )		
	NP <- length(mu)	
	res <- mu + as.vector( t( chol(Sigma) %*% stats::rnorm(NP) ) )	
	return(res)
}
