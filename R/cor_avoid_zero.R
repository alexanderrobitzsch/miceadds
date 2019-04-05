## File Name: cor_avoid_zero.R
## File Version: 0.08

#--- computes correlation by avoiding division by zero
#--- due to zero standard deviations
cor_avoid_zero <- function( x, y, eps=1E-20)
{
    c1 <- stats::cov( x=x, y=y)
    sd1 <- sd0(x)
    sd2 <- sd0(y)
    cor1 <- c1 / outer( sd1 + eps, sd2 + eps )
    return(cor1)
}

