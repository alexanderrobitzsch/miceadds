## File Name: ma_colMode.R
## File Version: 0.05

ma_colMode <- function(x)
{
    res <- apply( x, 2, FUN=function(samples){
                ks <- stats::density(samples)
                ks$x[ which.max(ks$y) ]
            } )
    return(res)
}
