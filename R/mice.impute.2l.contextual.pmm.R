## File Name: mice.impute.2l.contextual.pmm.R
## File Version: 0.24

mice.impute.2l.contextual.pmm <- function (y, ry, x, type,
            imputationWeights=NULL, interactions=NULL,
            quadratics=NULL, pls.facs=NULL, ...)
{
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    res <- mice_imputation_get_states( pos=parent.frame(n=1) )
    vname <- res$vname
    newstate <- res$newstate

    # data preparation
    xcov <- mice_imputation_create_contextual_variables( y=y, ry=ry, x=x, type=type, ...)
    #------
    # pmm imputation at level 2
    ximp <- mice.impute.weighted.pmm( y=y, ry=ry, x=xcov,
                imputationWeights=imputationWeights, interactions=interactions,
                quadratics=quadratics, pls.facs=pls.facs, ... )
    ximp <- mice_imputation_factor_pmm_convert_factor(imp=ximp,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(ximp)
}
