## File Name: mice_multilevel_create_formula.R
## File Version: 0.05


mice_multilevel_create_formula <- function( variables, include_intercept )
{
    intercept_code <- if ( include_intercept ){ 1 } else { 0 }
    fml <- paste0( c( intercept_code, variables ), collapse='+' )
    return(fml)
}
