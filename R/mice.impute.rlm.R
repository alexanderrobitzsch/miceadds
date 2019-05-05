## File Name: mice.impute.rlm.R
## File Version: 0.13


mice.impute.rlm <- function(y, ry, x, wy=NULL, lm_args=NULL, trafo=NULL,
    antitrafo=NULL, ...)
{
    requireNamespace("MASS")
    lm_fun <- MASS::rlm
    #- do imputation
    imp <- mice_imputation_linear_model_main(x=x, y=y, ry=ry, wy=wy,
                lm_args=lm_args, lm_fun=lm_fun, trafo=trafo,
                antitrafo=antitrafo, ...)
    #-- output
    return(imp)
}
