## File Name: quantile0.R
## File Version: 0.09


quantile0 <- function(x, probs=seq(0, 1, 0.25), na.rm=TRUE)
{
    sd1 <- stats0( x=x, FUN=stats::quantile, probs=probs, na.rm=na.rm )
    return(sd1)
}

