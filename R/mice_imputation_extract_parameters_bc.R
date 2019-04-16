## File Name: mice_imputation_extract_parameters_bc.R
## File Version: 0.04


mice_imputation_extract_parameters_bc <- function(mod)
{
    cmod <- mod$coef
    sigma <- cmod["sigma"]
    lambda <- cmod["lambda"]
    names_beta <- setdiff( names(cmod), c("sigma","lambda"))
    beta <- cmod[ names_beta ]
    df <- mod$df
    parms <- list(beta=beta, sigma=sigma, lambda=lambda, df=df)
    return(parms)
}
