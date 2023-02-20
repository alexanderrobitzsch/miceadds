## File Name: mice_imputation_pls_do_impute.R
## File Version: 0.212

mice_imputation_pls_do_impute <- function( x, y, ry, imputationWeights,
    use_weights, pls.impMethod, pls.print.progress,
    pls.impMethodArgs, type, use_boot=FALSE, vname=NULL, donors=5, ... )
{
    # clean missing values
    x <- as.matrix(x)
    x <- mice_imputation_pls_clean_missings(x=x, eps=1e-12)
    colnames(x) <- gsub(" ", "", colnames(x) )

    #*** logical whether an imputation should be conducted
    do_imputation <- ( pls.impMethod !="xplsfacs" )

    #-- admissible pmm methods
    pls_avai <- paste0("pmm",3:6)

    if ( do_imputation ){
        if (! use_weights){
            use_weights <- TRUE
            imputationWeights <- rep(1,length(y) )
        }
        if (sd0(x[,1] > 1e-10)){
            x <- cbind(1, x)
        }
        xobs <- x[ry,]
        yobs <- y[ry]
        weights.obs <- imputationWeights[ ry ]
        weights.obs <- normalize_vector( x=weights.obs )

        # special methods
        imp_methods_special <- c("norm","pmm","xplsfacs")

        # check appropriate imputation method
        # if ( ! pls.impMethod %in% c( "norm", "pmm", pls_avai) ){
            #    stop( paste0( "Only imputation methods 'norm' and 'pmm' can be ",
            #            "applied when weights are provided.\n") )
        # }

        # draw regression coefficients
        sample_pars <- ! use_boot
        if (pls.impMethod %in% imp_methods_special){
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
        }

        if (pls.impMethod=="norm"){
            x1 <- x[ !ry, ] %*% parm$beta + stats::rnorm(sum(!ry)) * parm$sigma
        }
        if (pls.impMethod=="pmm"){
            yhatobs <- x[ry, ] %*% parm$coef
            yhatmis <- x[!ry, ] %*% parm$beta
            x1 <- mice_imputation_pls_pmm_match(yhatobs=yhatobs, yhatmis=yhatmis,
                        y=y, ry=ry, donors=donors)
        }

        if ( ! ( pls.impMethod %in% imp_methods_special) ){
            if (use_boot){
                warning( paste0("Argument 'use_boot' cannot be used for imputation ",
                            "methods other than 'norm' and 'pmm'!\n") )
            }
            args <- pls.impMethodArgs
            x <- x[, apply(x, 2, stats::sd) > 1e-10, drop=FALSE ]
            args$x <- x
            args$y <- y
            args$ry <- ry
            args$type <- type
            imp_fct <- paste0("mice.impute.", pls.impMethod )
            x1 <- do.call(what=imp_fct, args=args)
        }
        do_imputation <- FALSE
    }

    if( pls.print.progress  ){
        cat( "\n", paste( "Imputation Method ", pls.impMethod, sep=""), "\n" )
    }

    #-- no imputation
    if ( pls.impMethod=="xplsfacs" ){
        x1 <- x
    }

    #--- output
    return(x1)
}
