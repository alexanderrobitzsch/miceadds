## File Name: plot.mids.1chain.R
## File Version: 1.09


# S3 plot method
plot.mids.1chain <- function( x, plot.burnin=FALSE, ask=TRUE, ... )
{
    chain_mean <- x$chainMean
    chain_var <- x$chainVar
    VV <- ncol(chain_mean)
    graphics::par(mfrow=c(2,2))
    if (!plot.burnin){
        iter_vec <- seq( x$burnin + 1, x$iter )
    } else {
        iter_vec <- 1:x$iter
    }
    for (vv in 1:VV){
        if ( sum( is.na( chain_mean[ iter_vec, vv ] ) )==0 ){
            plot( iter_vec, chain_mean[ iter_vec, vv ], type="l",
                        xlab="Iterations", ylab="M",
                        main=paste0("Mean ", colnames(chain_mean)[vv] ) )
            plot( iter_vec, sqrt( x$chainVar[ iter_vec, vv ] ), type="l",
                        xlab="Iterations", ylab="SD",
                        main=paste0("SD ", colnames(chain_mean)[vv] ) )
        }
        if (vv %% 2==0){ par(ask=ask) }
    }
    graphics::par(mfrow=c(1,1))
}
