## File Name: mice.impute.pls.R
## File Version: 3.788


mice.impute.pls <- function(y, ry, x, type, pls.facs=NULL,
            pls.impMethod="pmm", donors=5, pls.impMethodArgs=NULL,
            pls.print.progress=TRUE, imputationWeights=rep(1, length(y)), pcamaxcols=1E9,
            min.int.cor=0, min.all.cor=0, N.largest=0, pls.title=NULL, print.dims=TRUE,
            pls.maxcols=5000, use_boot=FALSE, envir_pos=NULL, extract_data=TRUE,
            remove_lindep=TRUE, derived_vars=NULL, ... )
{
    time1 <- Sys.time()
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor
    x_ <- x
    type_ <- type

    #--- extract arguments
    if ( is.null(envir_pos) ){
        pos <- parent.frame(n=2)
    } else {
        pos <- envir_pos
    }
    res <- mice_imputation_get_states( pos=pos )
    vname <- res$vname

    imp.temp <- res$newstate
    if (extract_data){
        res <- mice_imputation_prepare_2l_functions( vname=vname, envir=pos,
                    remove_lindep=remove_lindep)
        y <- res$y
        x <- res$x
        ry <- res$ry
        type <- res$type
        res <- mice_imputation_factor_pmm_prepare(y=y)
        y <- res$y
        y_aggr <- res$y_aggr
        is_factor <- res$is_factor
    }
    n <- NULL
    # normalize imputation weights
    imputationWeights <- normalize_vector( x=imputationWeights)

    # extract PLS factors
    pls.facs <- mice_imputation_extract_list_arguments( pls.facs, vname, 20 )
    # extract PLS imputation method
    pls.impMethod <- mice_imputation_extract_list_arguments( pls.impMethod,
                vname, "pmm" )
    pls.impMethodArgs <- mice_imputation_extract_list_arguments( pls.impMethodArgs,
                vname, list() )
    # define minimal correlation for interactions
    min.int.cor <- mice_imputation_extract_list_arguments( min.int.cor, vname, 0 )

    # proportion of explained variance in PCA
    pcamaxcols <- mice_imputation_extract_list_arguments( pcamaxcols, vname, 1e9 )

    # derived variables
    derived_vars <- mice_imputation_extract_list_arguments( derived_vars, vname, NULL )

    #*** print progress | print section 1
    res <- mice_imputation_pls_print_progress1( pls.print.progress=pls.print.progress,
                    vname=vname, print.dims=print.dims, y=y, ry=ry, x=x, type=type )

    #*** bootstrap sample of weights if requested
    imputationWeights <- mice_imputation_pls_draw_bootstrap_sample(ry=ry,
                            imputationWeights=imputationWeights, use_boot=use_boot)

    # include predictor variables with type !=0
    nt <- names(type)[ type !=0 ]
    nt <- intersect( nt, colnames(x) )
    x10 <- x <- x[, nt]
    use.ymat <- ( ! is.null( dim(y) ) )
    x <- as.matrix(x)
    use_weights <- stats::sd(imputationWeights) > 1E-30

    # standardize matrix of covariates
    x <- mice_imputation_pls_scale_x( x=x, imputationWeights=imputationWeights,
            use_weights=use_weights )

    # include cluster effects (adjusted group mean)
    res <- mice_imputation_include_cluster_effect( x=x, y=y, ry=ry, type=type )
    type <- res$type
    x0 <- x10 <- x <- res$x
    N <- ncol(x)

    #*** print progress | print section 2
    res <- mice_imputation_pls_print_progress2(    pls.print.progress=pls.print.progress,
                imp.temp=imp.temp, pls.title=pls.title, y=y, x=x )

    # extract interactions and quadratic terms
    pls.interactions <- names(type)[ type==4 ]
    pls.quadratics <- names(type)[ type==5 ]

    #-- include derived variables in x
    res <- mice_imputation_pls_include_derived_vars(x=x, derived_vars=derived_vars,
                y=y, vname=vname)
    x <- res$x
    added_vars <- res$added_vars

    #-- include interactions
    res <- mice_imputation_pls_include_interactions( pls.interactions=pls.interactions,
                pls.print.progress=pls.print.progress, x=x, y=y, ry=ry, type=type,
                min.int.cor=min.int.cor, pls.maxcols=pls.maxcols,
                imputationWeights=imputationWeights, use_weights=use_weights)
    x <- res$x
    xs <- res$xs

    #-- include quadratic terms
    res <- mice_imputation_pls_include_quadratics( pls.quadratics=pls.quadratics,
                pls.interactions=pls.interactions, x0=x0, x=x,
                pls.print.progress=pls.print.progress, xs=xs )
    x <- res$x

    #-- include only terms with largest correlations
    res <- mice_imputation_pls_largest_correlations( y=y, x=x, ry=ry, type=type,
                use.ymat=use.ymat, pls.print.progress=pls.print.progress, x10=x10,
                N.largest=N.largest, min.all.cor=min.all.cor,
                imputationWeights=imputationWeights )
    x <- res$x

    #-- perform PCA if requested
    x <- mice_imputation_pls_pca_reduction( x=x, pcamaxcols=pcamaxcols,
                imputationWeights=imputationWeights,
                pls.print.progress=pls.print.progress, use_weights=use_weights)
    x10 <- x    # copy dataset of predictors

    #--- perform PLS regression
    res <- mice_imputation_pls_estimate_pls_regression( pls.facs=pls.facs, x=x, y=y,
                ry=ry, use.ymat=use.ymat, imputationWeights=imputationWeights,
                use_weights=use_weights, pls.print.progress=pls.print.progress,
                pls.impMethod=pls.impMethod)
    x <- res$x
    x11a <- res$x11a

    #- handle type vector for multilevel models
    res <- mice_imputation_pls_type_multilevel_models(x=x, x_=x_, type=type, type_=type_)
    x <- res$x
    type <- res$type

    #--- apply imputation method
    x1 <- mice_imputation_pls_do_impute( x=x, y=y, ry=ry,
                imputationWeights=imputationWeights, use_weights=use_weights,
                pls.impMethod=pls.impMethod, pls.print.progress=pls.print.progress,
                pls.impMethodArgs=pls.impMethodArgs, type=type, use_boot=use_boot,
                vname=vname, donors=donors, ... )

    #--- finished all steps!
    time2 <- Sys.time()
    res <- mice_imputation_pls_print_progress3( pls.print.progress=pls.print.progress,
                    time1=time1, time2=time2 )
    x1 <- mice_imputation_factor_pmm_convert_factor(imp=x1,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(x1)
}
