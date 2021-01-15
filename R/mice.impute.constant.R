## File Name: mice.impute.constant.R
## File Version: 0.03


mice.impute.constant <- function(y, ry, x, fixed_values, ... )
{
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    pos <- res$pos
    state_data <- res$data
    fixed_default <- rep(-99, sum(!ry))
    fixed_values <- mice_imputation_extract_list_arguments( micearg=fixed_values,
                        vname=vname, miceargdefault=fixed_default)
    yimp <- fixed_values
    #-- output imputed values
    return(yimp)
}
