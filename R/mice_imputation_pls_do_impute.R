## File Name: mice_imputation_pls_do_impute.R
## File Version: 0.172

mice_imputation_pls_do_impute <- function( x, y, ry, imputationWeights,
    use_weights, pls.impMethod, pls.print.progress,
    pls.impMethodArgs, type, use_boot=FALSE, ... )
{
    x <- as.matrix(x)
    #*** logical whether an imputation should be conducted
    do_imputation <- ( pls.impMethod !="xplsfacs" )

    #-- admissible pmm methods
    pls_avai <- paste0("pmm",3:6)

    if ( do_imputation ){
        if (! use_weights){
            use_weights <- TRUE
            imputationWeights <- rep(1,length(y) )
        }
        if ( use_weights ){   # if there exists a real sample weight vector
            if (sd0(x[,1] > 1e-10)){
                x <- cbind(1, x)
            }
            xobs <- x[ry,]
            yobs <- y[ry]
            weights.obs <- imputationWeights[ ry ]
            weights.obs <- normalize_vector( x=weights.obs )
            # check appropriate imputation method
            if ( ! pls.impMethod %in% c( "norm", "pmm", pls_avai) ){
                stop( paste0( "Only imputation methods 'norm' and 'pmm' can be ",
                        "applied when weights are provided.\n") )
            }
            # draw regression coefficients
            sample_pars <- ! use_boot
            parm <- mice_imputation_weighted_norm_draw( yobs=yobs, xobs=xobs,
                        ry=ry, y=y, x=x, weights.obs=weights.obs,
                        sample_pars=sample_pars, ... )
            if (use_boot){
                weights.obs <- 1+0*weights.obs
                parm1 <- mice_imputation_weighted_norm_draw( yobs=yobs, xobs=xobs,
                        ry=ry, y=y, x=x, weights.obs=weights.obs,
                        sample_pars=sample_pars, ... )
                parm$coef <- parm1$coef
            }
            if (pls.impMethod=="norm"){
                x1 <- x[  ! ry, ] %*% parm$beta + stats::rnorm(sum(!ry)) * parm$sigma
            }
            if (pls.impMethod=="pmm"){
                yhatobs <- x[ry, ] %*% parm$coef
                yhatmis <- x[!ry, ] %*% parm$beta
                x1 <- apply(as.array(yhatmis), 1, mice::.pmm.match,
                            yhat=yhatobs, y=y[ry], ... )
            }
            if (pls.impMethod %in% pls_avai){
                args <- list(y=y, ry=ry, x=x, ...)
                fct <- paste0("mice.impute.", pls.impMethod )
                x1 <- do.call(what=fct, args=args)
            }
            do_imputation <- FALSE
        }
    }

    # do Imputation
    if( pls.print.progress  ){
        cat( "\n", paste( "Imputation Method ", pls.impMethod, sep=""), "\n" )
    }
    #-- apply imputation routine
    # if ( do_imputation){
    if (FALSE){
        args <- pls.impMethodArgs
        args$x <- x
        args$y <- y
        args$ry <- ry
        args$type <- type
        impMethod <- paste0("mice.impute.", pls.impMethod)
        state <- ma_exists_get(x='state', pos=parent.frame(n=2) )
        x1 <- do.call( what=impMethod, args=args )
    }

    #-- no imputation
    if ( pls.impMethod=="xplsfacs" ){
        x1 <- x
    }
    #--- output
    return(x1)
}
