## File Name: NMIwaldtest_compute_statistic.R
## File Version: 0.03

NMIwaldtest_compute_statistic <- function(qhat, u, Cdes, rdes, NB, NW, eps=1E-20)
{
    res0 <- NMIcombine( qhat=qhat, u=u )
    ubar <- res0$ubar
    qbar <- res0$qbar
    Bm <- res0$Bm
    Wm <- res0$Wm
    k <- nrow(Cdes)
    # quadratic form
    uinv <- solve(ubar)
    rmb <- (1+1/NB)*sum(diag( Bm %*% uinv )) / k
    rmw <- (1-1/NW)*sum(diag( Wm %*% uinv )) / k
    stat <- t(qbar) %*% uinv %*% qbar
    stat <- stat / ( k * ( 1 + rmb + rmw ) )
    stat <- stat[1,1]
    df1 <- k
    df2 <- rmb^2 / ( NB - 1 + eps ) / ( 1 + rmw + rmb )^2 +
                    rmw^2 / ( NB*( NW - 1 + eps) ) / ( 1 + rmw + rmb )^2
    df2 <- k / df2
    stat <- data.frame( F=stat, df1=df1, df2=df2,
                pval=1-stats::pf( q=stat, df1=df1, df2=df2 ) )
    res <- list( stat=stat, linear_hyp=res0, qhat=qhat, u=u, Cdes=Cdes, rdes=rdes )
    return(res)
}
