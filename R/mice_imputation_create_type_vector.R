## File Name: mice_imputation_create_type_vector.R
## File Version: 0.07

mice_imputation_create_type_vector <- function( variables, value=1)
{
    np <- length(variables)
    type_sel <- rep(value,np)
    names(type_sel) <- variables
    return(type_sel)
}
