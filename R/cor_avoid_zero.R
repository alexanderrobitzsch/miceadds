## File Name: cor_avoid_zero.R
## File Version: 0.03

#--- computes correlation by avoiding division by zero
#--- due to zero standard deviations
cor_avoid_zero <- function( x, y , eps = 1E-20)
{
	c1 <- stats::cov( x=x, y=y)
	sd1 <- sd0(x) + eps
	sd2 <- sd0(y) + eps	
	cor1 <- c1 / outer( sd1 , sd2 )
	return(cor1)
}	

