## File Name: mice.impute.norm3.R
## File Version: 0.06
## File Last Change: 2017-08-25 10:27:14

mice.impute.norm3 <- function (y, ry, x, wy = NULL, ridge = 10^(-5) , ...) 
{
	##**** test function for inclusion of wy
	if ( is.null(wy)){
		ry <- ! wy
	}
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw3(y, ry, x, ridge=ridge ,  ...)	
	n <- sum(wy)
    imp <- x[wy, ] %*% parm$beta + stats::rnorm(n) * parm$sigma
	return(imp)
}
