## File Name: mice.impute.lm.R
## File Version: 0.06


mice.impute.lm <- function(y, ry, x, wy=NULL, lm_args=NULL, trafo=NULL,
    antitrafo=NULL, ...)
{
    lm_fun <- stats::lm
    #- do imputation
    imp <- mice_imputation_linear_model_main(x=x, y=y, ry=ry, wy=wy,
                lm_args=lm_args, lm_fun=lm_fun, trafo=trafo,
                antitrafo=antitrafo, ...)
    #-- output
    return(imp)
}
