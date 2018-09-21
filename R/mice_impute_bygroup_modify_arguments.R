## File Name: mice_impute_bygroup_modify_arguments.R
## File Version: 0.02


mice_impute_bygroup_modify_arguments <- function(args, ind_gg,
    imputationFunction_vname)
{
    Nmis <- sum( ! args$ry )
    if ( "wy" %in% names(args) ){
        args$wy <- args$wy[ ind_gg ]
        # Nmis <- sum( ! args$wy )
    }
    #--- output
    res <- list( args=args, Nmis=Nmis)
    return(res)
}
