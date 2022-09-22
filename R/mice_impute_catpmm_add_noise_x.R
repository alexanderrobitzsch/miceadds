## File Name: mice_impute_catpmm_add_noise_x.R
## File Version: 0.02

mice_impute_catpmm_add_noise_x <- function(dfr, vars_x, ridge)
{
    n <- nrow(dfr)
    for (var_hh in vars_x){
        sd_hh <- stats::sd( dfr[,var_hh] )
        ridge_hh <- ridge*sd_hh
        dfr[, var_hh ] <- dfr[, var_hh ] + stats::rnorm(n, mean=0, sd=ridge_hh)
    }
    return(dfr)
}
