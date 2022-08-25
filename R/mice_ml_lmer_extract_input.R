## File Name: mice_ml_lmer_extract_input.R
## File Version: 0.297

mice_ml_lmer_extract_input <- function(pos, levels_id, random_slopes, variables_levels,
        pls.facs, min.int.cor, min.all.cor, interactions, quadratics, model,
        group_index=NULL, iter_re=0)
{
    res <- mice_imputation_get_states( pos=pos )
    vname <- res$vname
    imp.temp <- res$newstate
    predictorMatrix <- ma_exists_get(x='predictorMatrix', pos=pos)
    type <- predictorMatrix[ vname, ]
    data <- ma_exists_get(x='data', pos=pos)
    if ( ! is.null(group_index) ){
        data <- data[ group_index,, drop=FALSE ]
    }
    #--- level identifiers
    if ( is.list( levels_id ) ){
        levels_id <- levels_id[[ vname ]]
    }
    #--- random slopes
    levi <- intersect( levels_id, names(random_slopes) )
    if ( ! is.null(levi) ){
        random_slopes <- random_slopes[[ vname ]]
    }
    if ( ! is.null(random_slopes) ){
        NL <- length(random_slopes)
        nrs <- names(random_slopes)
        for (ll in 1:NL){
            level_ll <- nrs[ll]
            random_slopes[[ level_ll ]] <- setdiff( random_slopes[[ level_ll ]], vname )
        }
    }
    #--- level of a variable
    vname_level <- variables_levels[ vname ]

    # extract PLS factors
    pls.facs <- mice_imputation_extract_list_arguments( micearg=pls.facs, vname=vname,
                        miceargdefault=20 )
    # correlations interactions
    min.int.cor <- mice_imputation_extract_list_arguments( micearg=min.int.cor,
                        vname=vname, miceargdefault=0 )
    min.all.cor <- mice_imputation_extract_list_arguments( micearg=min.all.cor,
                        vname=vname, miceargdefault=0 )

    iter_re <- mice_imputation_extract_list_arguments( micearg=iter_re, vname=vname,
                        miceargdefault=0 )

    #--- interactions
    interactions <- mice_imputation_extract_arguments_list(v1=interactions, vname=vname)

    #--- quadratics
    quadratics <- mice_imputation_extract_arguments_list(v1=quadratics, vname=vname)

    #--- model
    model <- mice_imputation_extract_list_arguments( micearg=model, vname=vname,
                        miceargdefault="continuous" )

    #--- output
    res <- list( vname=vname, type=type, data=data, levels_id=levels_id,
                    random_slopes=random_slopes, imp.temp=imp.temp,
                    vname_level=vname_level, pls.facs=pls.facs,
                    min.int.cor=min.int.cor, min.all.cor=min.all.cor,
                    interactions=interactions, quadratics=quadratics,
                    model=model, predictorMatrix=predictorMatrix, iter_re=iter_re )
    return(res)
}

