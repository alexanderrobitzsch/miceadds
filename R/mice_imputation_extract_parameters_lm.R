## File Name: mice_imputation_extract_parameters_lm.R
## File Version: 0.03

mice_imputation_extract_parameters_lm <- function(mod)
{
    parms <- list(beta=mod$coef)
    rmod <- residuals(mod)^2
    parms$sigma <- sqrt( sum(rmod) / ( length(rmod) - length(mod$coef) ) )
    return(parms)
}
