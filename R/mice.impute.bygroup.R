## File Name: mice.impute.bygroup.R
## File Version: 0.594

mice.impute.bygroup <- function( y, ry, x, wy=NULL, group,
        imputationFunction, ... )
{
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    #--- extract arguments
    pos <- parent.frame()
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    state_data <- res$data
    pos <- res$pos

    #- where argument
    wy <- mice_imputation_define_wy(wy=wy, ry=ry)

    # imputation function
    imputationFunction_vname <- mice_imputation_extract_list_arguments(
                micearg=imputationFunction, vname=vname, miceargdefault='norm' )

    # group variable
    group_vname <- mice_imputation_extract_list_arguments(
                micearg=group, vname=vname, miceargdefault='' )
    l2_imp_fct <- substring(imputationFunction_vname,1,2)=='2l'
    ml_lmer_imp_fct <- imputationFunction_vname=='ml.lmer'

    #*** full data frame with indices and all groups
    if (group_vname %in% colnames(x)){
        group_values <- x[, group_vname]
    } else {
        group_values <- state_data[, group_vname]
    }

    dfr_index <- data.frame( y=y, ry=ry, wy=wy, group_=group_values )
    groups <- unique( dfr_index$group_ )
    G <- length(groups)
    # remove grouping variable from set of predictors
    vars1 <- setdiff( colnames(x), group_vname )
    if (l2_imp_fct){
        res <- mice_imputation_prepare_2l_functions( vname=vname, envir=pos)
        y <- res$y
        x <- res$x
        ry <- res$ry
        type <- res$type
        vars1 <- setdiff( colnames(x), group_vname )
        type <- type[ vars1 ]
    }
    x <- x[, vars1, drop=FALSE ]
    imp_function <- paste0('mice.impute.', imputationFunction_vname )
    for (gg in 1L:G){
        group_gg <- groups[gg]
        ind_gg <- which( dfr_index$group_==group_gg )
        #-- argument list for imputations
        args <- list( y=y[ind_gg], ry=ry[ind_gg], x=x[ind_gg,,drop=FALSE], ... )
        if (l2_imp_fct){
            args$type <- type
        }
        res <- mice_imputation_bygroup_modify_arguments(args=args, ind_gg=ind_gg,
                    imputation_function=imp_function, wy=wy)
        args <- res$args
        Nmis <- res$Nmis
        args$group_index <- ind_gg
        if (Nmis > 0){
            ximp <- do.call( what=imp_function, args=args )
            ind0_gg <- which( dfr_index$wy )
            ind0_gg <- intersect( ind_gg, ind0_gg )
            dfr_index[ ind0_gg, 'y'] <- as.vector(ximp)
        }
    }
    imp <- dfr_index[ wy, 'y']
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(imp)
}

#    res <- mice_imputation_prepare_2l_functions( vname=vname,
#                    envir=parent.frame(n=1) )
