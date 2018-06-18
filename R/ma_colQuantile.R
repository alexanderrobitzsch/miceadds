## File Name: ma_colQuantile.R
## File Version: 0.06

ma_colQuantile <- function(x, prob)
{
    res <- apply( x, 2, FUN=function(samples){
                stats::quantile(samples, probs=prob )
                    } )
    return(res)
}
