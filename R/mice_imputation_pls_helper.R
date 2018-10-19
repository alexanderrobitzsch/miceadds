## File Name: mice_imputation_pls_helper.R
## File Version: 0.292


# auxiliary function for PLS imputation
mice_imputation_pls_helper <- function( newstate, vname, pls.impMethod, x, y, ry,
            imputationWeights=rep( 1, length(y)), interactions, quadratics, pls.facs,
            envir_pos=NULL, ... )
{
    # interactions and quadratic terms
    interactions <- mice_imputation_extract_list_arguments( micearg=interactions,
                        vname=vname, miceargdefault=NULL )
    names_x <- colnames(x)
    interactions <- intersect( interactions, names_x )
    quadratics <- mice_imputation_extract_list_arguments( micearg=quadratics,
                    vname=vname, miceargdefault=NULL )
    quadratics <- setdiff( intersect( quadratics, names_x ), interactions)
    if ( is.vector(x) ){
        x <- matrix( x, ncol=1 )
    }
    # define variable type
    type <- rep(1, ncol(x)  )
    names(type) <- names_x
    pls.use <- FALSE
    if ( length( interactions)>0 ){
        type[ interactions ] <- 4
        pls.use <- TRUE
    }
    if ( length( quadratics )>0 ){
        type[ quadratics ] <- 5
        pls.use <- TRUE
    }
    if ( pls.use & is.null(pls.facs) ){
        pls.facs <- 10000
    }

    #.*.*.*..*.*.*..*.*.*.
    # PLS imputation if specified
    pls.facs <- mice_imputation_extract_list_arguments( micearg=pls.facs,
                    vname=vname, miceargdefault=NULL )
    if ( is.null(envir_pos) ){
        envir_pos <- parent.frame(n=1)
    }
    yimp <- NULL
    if ( ! is.null(pls.facs) ){
        yimp <- mice.impute.pls(y=y, ry=ry, x=x, type=type, pls.facs=pls.facs,
                        pls.impMethod=pls.impMethod, imputationWeights=imputationWeights,
                        envir_pos=envir_pos, extract_data=FALSE, ... )
    }
    res <- list( yimp=yimp, pls.facs=pls.facs )
    return(res)
}
#------------------------------------------------------------------------

.aux.pls.imputation <- mice_imputation_pls_helper
