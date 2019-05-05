## File Name: mice_imputation_refresh_update_factor.R
## File Version: 0.03


mice_imputation_refresh_update_factor <- function( acc, fac_old)
{
    target <- .5
    min_bound <- .4
    max_bound <- .6
    fac_old <- ifelse( acc < min_bound, fac_old / ( 2 - acc / target ), fac_old )
    fac_old <- ifelse( acc > max_bound, fac_old * ( 2 - (1-acc)/(1-target) ), fac_old )
    return(fac_old)
}
