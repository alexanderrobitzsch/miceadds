## File Name: mice.impute.pls.R
## File Version: 3.35
mice.impute.pls <- function(y, ry, x, type, pls.facs=NULL,
                                pls.impMethod="pmm",
                                pls.impMethodArgs=NULL,
                                pls.print.progress=TRUE,
                                imputationWeights=rep( 1,length(y) ),
                                pcamaxcols=1E9,
                                min.int.cor=0,
                                min.all.cor=0, N.largest=0,
                                pls.title=NULL, print.dims=TRUE,
                                pls.maxcols=5000,    envir_pos=NULL,
                                extract_data=TRUE, ... )
{
    #...........................................................................#
    # INPUT                                                                     #
    # pls.facs          ... number of factors for PLS regression                #
    # pls.interactions  ... include.interactions                                #
    #                 -> type==4                                        #
    # pls.quadratics    ... include quadratic terms?                            #
    #                 -> type==5                                        #
    # type                 ...=6 : for these variables no interactions will be created    #
    # pls.impMethod     ... method "norm" or "pmm" or "tricube.pmm"             #
    #                           "xplsfacs" -> return predicted X PLS factors    #
    # pls.print.progress    ... print progress of PLS regression estimation     #
    # imputationWeight  ... vector of weights for imputation                    #
    # min.int.cor       ... minimal correlation for inclusion of interaction    #
    #                           effects                                         #
    # min.all.cor             ... minimal correlation for main effects            #
    # N.largest            ... select N.largest correlations                        #
    # pls.title         ... title which is displayed                             #
    #...........................................................................#

    time1 <- Sys.time()

    #--- extract arguments
    if ( is.null(envir_pos) ){
        pos <- parent.frame(n=1)
    } else {
        pos <- envir_pos
    }
    res <- mice_imputation_get_states( pos=pos )
    vname <- res$vname
    imp.temp <- res$newstate
        #...  newstate <- list(it=k,  im=i,  co=j, dep=vname,  meth=theMethod,
        #...                   log=oldstate$log)

    if (extract_data){
        res <- mice_imputation_prepare_2l_functions( vname=vname, envir=pos )
        y <- res$y
        x <- res$x
        ry <- res$ry
        type <- res$type
    }
    n <- NULL
    # normalize imputation weights
    imputationWeights <- normalize_vector( x=imputationWeights)

    # extract PLS factors
    pls.facs <- mice_imputation_extract_list_arguments( pls.facs, vname,  20 )
    # extract PLS imputation method
    pls.impMethod <- mice_imputation_extract_list_arguments( pls.impMethod,
                vname, "pmm" )
    pls.impMethodArgs <- mice_imputation_extract_list_arguments( pls.impMethodArgs,
                vname, list() )
    # define minimal correlation for interactions
    min.int.cor <- mice_imputation_extract_list_arguments( min.int.cor, vname, 0 )

    #*** print progress | print section 1
    res <- mice_imputation_pls_print_progress1( pls.print.progress, vname,
                    print.dims, y, ry, x, type )

    # include predictor variables with type !=0
    nt <- names(type)[ type !=0 ]
    nt <- intersect( nt, colnames(x) )
    x10 <- x <- x[, nt]
    use.ymat <- ( ! is.null( dim(y) ) )
    x <- as.matrix(x)
    use_weights <- stats::sd( imputationWeights ) > 1E-30

    # standardize matrix of covariates
    x <- mice_imputation_pls_scale_x( x=x, imputationWeights=imputationWeights,
            use_weights=use_weights )

    # include cluster effects (adjusted group mean)
    res <- mice_imputation_include_cluster_effect( x=x, y=y, ry=ry, type=type )
    type <- res$type
    x0 <- x10 <- x <- res$x
    N <- ncol(x)

    #*** print progress | print section 2
    res <- mice_imputation_pls_print_progress2(pls.print.progress, imp.temp,
                pls.title, y, x)

    # extract interactions and quadratic terms
    pls.interactions <- names(type)[ type==4 ]
    pls.quadratics <- names(type)[ type==5 ]

    #-- include interactions
    res <- mice_imputation_pls_include_interactions(pls.interactions,
                pls.print.progress, x, y, ry, type, min.int.cor, pls.maxcols )
    x <- res$x
    xs <- res$xs

    #-- include quadratic terms
    res <- mice_imputation_pls_include_quadratics( pls.quadratics,
                pls.interactions, x0, x, pls.print.progress, xs )
    x <- res$x

    #-- include only terms with largest correlations
    res <- mice_imputation_pls_largest_correlations( y, x, ry, type,
                use.ymat, pls.print.progress, x10, N.largest, min.all.cor )
    x <- res$x

    #-- perform PCA if requested
    x <- mice_imputation_pls_pca_reduction(x, pcamaxcols,    pls.print.progress)
    x10 <- x    # copy dataset of predictors


    #--- perform PLS regression
    res <- mice_imputation_pls_estimate_pls_regression( pls.facs, x, y, ry,
                use.ymat, imputationWeights, use_weights, pls.print.progress )
    x <- res$x
    x11a <- res$x11a

    #--- apply imputation method
    x1 <- mice_imputation_pls_do_impute( x, y, ry, imputationWeights,
                use_weights, pls.impMethod,
                pls.print.progress, pls.impMethodArgs, type, ... )
    #--- finished all steps!
    time2 <- Sys.time()
    res <- mice_imputation_pls_print_progress3(pls.print.progress, time1, time2 )
    return(x1)
}






