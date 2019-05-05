## File Name: mice.impute.lqs.R
## File Version: 0.05


mice.impute.lqs <- function(y, ry, x, wy=NULL, lm_args=NULL, trafo=NULL,
    antitrafo=NULL, ...)
{
    requireNamespace("MASS")
    lm_fun <- MASS::lqs
    #- do imputation
    imp <- mice_imputation_linear_model_main(x=x, y=y, ry=ry, wy=wy,
                lm_args=lm_args, lm_fun=lm_fun, trafo=trafo,
                antitrafo=antitrafo, ...)
    #-- output
    return(imp)
}
