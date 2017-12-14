## File Name: ma_rmvnorm.R
## File Version: 0.05

ma_rmvnorm <- function(n, mu=NULL, sigma , eps = 1E-10 )
{
	p <- ncol(sigma)
	V <- chol(sigma)
	mat <- matrix( stats::rnorm( p*n ) , ncol= p ) %*% V
	return(mat)
}
