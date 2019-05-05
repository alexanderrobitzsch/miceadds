## File Name: stats0.R
## File Version: 0.05


stats0 <- function(x, FUN, na.rm=TRUE,...)
{
    if ( ! is.vector(x) ){
        sd1 <- apply( x, 2, FUN=FUN, na.rm=na.rm, ...)
    } else {
        sd1 <- FUN( x=x, na.rm=na.rm, ...)
    }
    return(sd1)
}
