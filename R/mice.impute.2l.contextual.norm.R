## File Name: mice.impute.2l.contextual.norm.R
## File Version: 0.23


mice.impute.2l.contextual.norm <- function (y, ry, x, type, ridge=10^(-5),
            imputationWeights=NULL, interactions=NULL, quadratics=NULL,
            pls.facs=NULL, ...)
{
    res <- mice_imputation_get_states( pos=parent.frame(n=1) )
    vname <- res$vname
    newstate <- res$newstate
    # data preparation
    xcov <- mice_imputation_create_contextual_variables( y=y, ry=ry, x=x, type=type, ...)
    #------
    # norm imputation at level 2
    ximp <- mice.impute.weighted.norm( y=y, ry=ry, x=xcov, ridge=ridge,
                imputationWeights=imputationWeights, interactions=interactions,
                quadratics=quadratics, pls.facs=pls.facs, ... )
    return(ximp)
}
