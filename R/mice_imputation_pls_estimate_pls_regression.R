## File Name: mice_imputation_pls_estimate_pls_regression.R
## File Version: 0.15

mice_imputation_pls_estimate_pls_regression <- function( pls.facs, x, y, ry,
    use.ymat, imputationWeights, use_weights, pls.print.progress )
{
    x00 <- x
    x11a <- NULL
    # calculate partial least squares regression
    nfac <- min( pls.facs, ncol(x) )
    do_pls <- ( pls.facs > 0 ) & ( pls.facs < 1000)
    yobs <- y[ ry ]
    if (use.ymat){
        yobs <- y[ry,]
    }
    # center y obs
    weight.obs <- imputationWeights[ ry ]
    weight.obs <- normalize_vector( weight.obs )
    yobs <- yobs - stats::weighted.mean( yobs, weight.obs )
    xobs <- x[ ry, ]
    # include imputationWeights here and calculate weight.obs
    # in the regression model, only PLS factors of X are used
    if( use_weights ){
        yobs <- weight.obs * yobs
        xobs <- outer( weight.obs, rep(1, ncol(xobs) ) ) * xobs
    }


    if( pls.print.progress  ){
        cat( "\n", paste( ncol(xobs), " Dimensions", sep="")  )
        cat( "\n", paste( nfac, " PLS factors are used", sep="") )
        utils::flush.console()
        if ( pls.facs==0){
            cat( "\n", "All", ncol(x),
                "predictors are used (no PLS dimension reduction)")
        }
        cat("\n\n" )
    }
    
    if ( do_pls ){
        VV <- ncol(xobs)
        mod <- kernelpls.fit2( X=as.matrix(xobs),
                        Y=matrix(yobs,ncol=1),ncomp=nfac)
        if( pls.print.progress ){
            print( round( 100*mod$R2, 2 ))
        }
        dfr2 <- x
        pmod <- predict.kernelpls.fit2( mod, X=as.matrix(x) )
        x <- cbind(1, as.matrix(pmod))
        x11a <- x
        if( pls.print.progress ){
            cat( "\nPLS estimation finished ", substring(Sys.time(),1),"\n" )
            utils::flush.console()
        }
        
        # remove columns with small standard deviations
        sd_cols <- apply(x, 2 , stats::sd)
        eps <- 1E-10
        ind <- which( sd_cols < eps*sd_cols[2] )[-1]
        if ( length(ind) > 0 ){
            x <- x[, -ind ]
        }
    }
            
    if ( pls.facs==0){
        x <- cbind( 1, x )
    }
    if (pls.facs > 1000){
        x <- x00
    }
        
    #--- output
    res <- list( x=x, x11a=x11a )
    return(res)
}
