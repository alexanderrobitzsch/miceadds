## File Name: mice.impute.imputeR.lmFun.R
## File Version: 0.13


mice.impute.imputeR.lmFun <- function(y, ry, x, Fun=NULL, draw_boot=TRUE,
    add_noise=TRUE, ... )
{
    requireNamespace("imputeR")
    yimp <- mice_imputation_imputeR(y=y, ry=ry, x=x, Fun=Fun, draw_boot=draw_boot,
                    add_noise=add_noise, use_cFun=FALSE,
                    default_fun=imputeR::ridgeR, ... )
    #-- output imputed values
    return(yimp)
}
