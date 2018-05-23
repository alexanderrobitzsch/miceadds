## File Name: mice_impute_create_type_vector.R
## File Version: 0.06

mice_impute_create_type_vector <- function( variables, value=1)
{
    np <- length(variables)
    type_sel <- rep(value,np)
    names(type_sel) <- variables
    return(type_sel)
}
