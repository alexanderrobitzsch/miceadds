## File Name: adj_groupmean.R
## File Version: 0.01


adj_groupmean <- function( variable, cluster )
{
    a1 <- stats::aggregate( variable, list(cluster), mean  )
    a2 <- stats::aggregate( 1+0*variable, list(cluster), sum  )
    ind <- match( cluster, a1[,1] )
    ( a2[ind,2] * a1[ ind, 2] - variable ) / a2[ ind, 2]
}
