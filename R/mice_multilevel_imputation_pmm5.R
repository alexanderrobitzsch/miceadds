## File Name: mice_multilevel_imputation_pmm5.R
## File Version: 0.222


mice_multilevel_imputation_pmm5 <- function (y, ry, x, yhatobs,
        yhatmis, donors=5, noise=1E5, ...)
{
    N1 <- length(yhatobs)
    N0 <- length(yhatmis)
    N <- N0 + N1
    GG <- 1000* max( abs(yhatobs), abs(yhatmis) )
    dfr <- cbind( 1, 1L:N1, yhatobs, y[ry] )
    dfr0 <- cbind( 0, 1L:N0, yhatmis, NA)
    dfr <- rbind( dfr, dfr0 )
    colnames(dfr) <- c("obs", "index_obs_miss", "yhat", "y")
    # add some small noise to create unique entries in matrix d0
    d00 <- abs( diff(dfr[,"yhat"]) )
    fg1 <- min( d00[ d00 > 0 ] )
    dfr[,"yhat"] <- dfr[,"yhat"] + stats::runif( N, 0, fg1 / noise )
    dfr <- data.frame(dfr[ order(dfr[,3] ), ])
    dfr$sortindex <- 1L:N
    dfr$obsindex_low <- cumsum( dfr$obs )
    ind <- seq( N, 1, -1 )
    Ny <- sum( ry)
    N0 <- sum( ! ry )
    c1 <- Ny - cumsum( dfr$obs[ ind ] )   + 1
    dfr$obsindex_upp <- c1[ ind ]
    vy <- c(1,Ny)
    dfr$obsindex_low <- mice::squeeze( dfr$obsindex_low, vy)
    dfr$obsindex_upp <- mice::squeeze( dfr$obsindex_upp, vy)
    dfr0 <- dfr[ dfr$obs==0, ]
    dfr1 <- dfr[ dfr$obs==1, ]
    # create matrix for sampling
    ydonors <- matrix( NA, nrow=nrow(dfr0), ncol=2*donors )
    dfr0 <- dfr0[ order(dfr0$index_obs_miss), ]
    for ( dd in 1L:donors){
        ydonors[,dd] <- dfr1[ mice::squeeze( dfr0$obsindex_low - dd + 1,c(1,Ny) ), "y"]
        ydonors[,dd+donors] <- dfr1[ mice::squeeze( dfr0$obsindex_upp + dd - 1,c(1,Ny) ),
                                            "y"]
    }
    ind.sample <- sample( 1L:(2*donors), N0, replace=TRUE )
    imp <- ydonors[ cbind( 1L:N0, ind.sample) ]
    return(imp)
}
