## File Name: mice.impute.lm_fun.R
## File Version: 0.02


mice.impute.lm_fun <- function(y, ry, x, wy=NULL, lm_args=NULL, lm_fun="lm", trafo=NULL,
    antitrafo=NULL, ...)
{
    #- do imputation
    imp <- mice_imputation_linear_model_main(x=x, y=y, ry=ry, wy=wy,
                lm_args=lm_args, lm_fun=lm_fun, trafo=trafo,
                antitrafo=antitrafo, ...)
    #-- output
    return(imp)
}
