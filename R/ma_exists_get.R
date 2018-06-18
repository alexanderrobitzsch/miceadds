## File Name: ma_exists_get.R
## File Version: 0.14

ma_exists_get <- function( x, pos, n_index=1:8)
{
    res <- ma_exists(x=x, pos=pos, n_index=n_index+1)
    return(res$obj)
}
