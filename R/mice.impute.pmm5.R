## File Name: mice.impute.pmm5.R
## File Version: 1.22

mice.impute.pmm5 <- function (y, ry, x, donors=3, noise=10^5,
        ridge=10^(-5), ...)
{
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    x <- cbind(1, as.matrix(x))
    res <- mice_imputation_norm_draw(y=y, ry=ry, x=x, ridge=ridge, ...)
    yhatobs <- res$yhatobs
    yhatmis <- res$yhatmis
    yobs <- res$yobs

    GG <- 1000* max( abs( yhatobs[,1] ), abs( yhatmis[,1] ))
    dfr <- cbind( 1, 1:nrow(yhatobs), yhatobs[,1], y[ry] )
    dfr0 <- cbind( 0, 1:nrow(yhatmis), yhatmis[,1], NA)
    dfr <- rbind( dfr, dfr0 )
    colnames(dfr) <- c("obs", "index_obs_miss", "yhat", "y")
    # add some small noise to create unique entries in matrix d0
    d00 <- abs(diff(dfr[,"yhat"]))
    fg1 <- min( d00[ d00 > 0 ] )
    dfr[,"yhat"] <- dfr[,"yhat"] + stats::runif( nrow(dfr), 0, fg1 / noise )
    dfr <- data.frame(dfr[ order(dfr[,3] ), ])
    dfr$sortindex <- seq( 1, nrow(dfr))
    dfr$obsindex_low <- cumsum( dfr$obs )
    ind <- seq( nrow( dfr), 1, -1 )
    Ny <- sum( ry)
    N0 <- sum( ! ry )
    c1 <- Ny - cumsum( dfr$obs[ ind ] )   + 1
    dfr$obsindex_upp <- c1[ ind ]
    dfr$obsindex_low <- mice::squeeze( dfr$obsindex_low, c(1,Ny))
    dfr$obsindex_upp <- mice::squeeze( dfr$obsindex_upp, c(1,Ny))
    dfr0 <- dfr[ dfr$obs==0, ]
    dfr1 <- dfr[ dfr$obs==1, ]

    # create matrix for sampling
    ydonors <- matrix( NA, nrow=nrow(dfr0), ncol=2*donors )

    dfr0 <- dfr0[ order(dfr0$index_obs_miss), ]
    for ( dd in 1:donors){
        ind_low <- mice::squeeze( dfr0$obsindex_low - dd + 1,c(1,Ny) )
        ydonors[, dd] <- dfr1[ ind_low, "y"]
        ind_upp <- mice::squeeze( dfr0$obsindex_upp + dd - 1,c(1,Ny) )
        ydonors[, dd+donors] <- dfr1[ ind_upp, "y"]
    }
    ind.sample <- sample( 1:(2*donors), N0, replace=TRUE )
    imp <- ydonors[ cbind( 1:N0, ind.sample) ]
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)
    #-- output
    return(imp)
}
