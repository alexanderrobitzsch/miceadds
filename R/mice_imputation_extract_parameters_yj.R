## File Name: mice_imputation_extract_parameters_yj.R
## File Version: 0.02


mice_imputation_extract_parameters_yj <- function(mod)
{
    parms <- mice_imputation_extract_parameters_bc(mod=mod)
    return(parms)
}
