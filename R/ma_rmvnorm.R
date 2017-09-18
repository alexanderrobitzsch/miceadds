## File Name: ma_rmvnorm.R
## File Version: 0.03
## File Last Change: 2017-08-22 20:53:45

ma_rmvnorm <- function(n, mu=NULL, sigma , eps = 1E-10 )
{
	p <- ncol(sigma)
	ind <- which( diag(sigma) > eps )
	p2 <- length(ind)
	mat <- matrix( 0 , nrow=n , ncol=p )
	sigma2 <- sigma[ ind, ind , drop=FALSE ]
	V <- chol(sigma2)
	mat2 <- matrix( stats::rnorm( p2*n ) , ncol= p2 ) %*% V
	mat[ , ind ] <- mat2
	return(mat)
}
