## File Name: syn_da_extend_list.R
## File Version: 0.05


syn_da_extend_list <- function(formula, syn_vars)
{
    NSV <- length(syn_vars)
    res <- list()
    for (ss in 1L:NSV){
        res[[ syn_vars[ss] ]] <- formula[[ syn_vars[ss] ]]
    }
    return(res)
}
