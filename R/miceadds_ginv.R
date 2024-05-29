## File Name: miceadds_ginv.R
## File Version: 0.02

miceadds_ginv <- function(x, ...)
{
    requireNamespace('MASS')
    y <- MASS::ginv(X=x, ...)
    return(y)
}
