## File Name: mice.impute.imputeR.cFun.R
## File Version: 0.06


mice.impute.imputeR.cFun <- function(y, ry, x, Fun=NULL, draw_boot=TRUE, ... )
{
    yimp <- mice_imputation_imputeR(y=y, ry=ry, x=x, Fun=Fun, draw_boot=draw_boot,
                    add_noise=FALSE, use_cFun=TRUE,
                    default_fun=imputeR::ridgeC, ... )
    #-- output imputed values
    return(yimp)
}
