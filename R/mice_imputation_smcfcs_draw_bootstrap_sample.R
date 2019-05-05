## File Name: mice_imputation_smcfcs_draw_bootstrap_sample.R
## File Version: 0.03

mice_imputation_smcfcs_draw_bootstrap_sample <- function(data, ry)
{
    ind_boot <- which(ry)
    ind_boot <- sort( sample(ind_boot, length(ind_boot), replace=TRUE) )
    dat1 <- data[ ind_boot, ]
    rownames(dat1) <- NULL
    return(dat1)
}
