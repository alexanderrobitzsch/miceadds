## File Name: var0.R
## File Version: 0.06

var0 <- function(x, na.rm=TRUE)
{
    sd1 <- stats0( x=x, FUN=stats::var, na.rm=na.rm )
    return(sd1)
}
