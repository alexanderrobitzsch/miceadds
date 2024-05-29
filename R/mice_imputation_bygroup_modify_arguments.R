## File Name: mice_imputation_bygroup_modify_arguments.R
## File Version: 0.09


mice_imputation_bygroup_modify_arguments <- function(args, ind_gg,
    imputation_function, wy)
{
    Nmis <- sum(!args$ry)
    args_imputation_function <- methods::formalArgs(def=imputation_function)
    if ( 'wy' %in% args_imputation_function ){
        args$wy <- wy[ind_gg]
        Nmis <- sum(args$wy)
    }
    #--- output
    res <- list( args=args, Nmis=Nmis)
    return(res)
}
