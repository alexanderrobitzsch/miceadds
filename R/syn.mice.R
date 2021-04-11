## File Name: syn.mice.R
## File Version: 0.246


syn.mice <- function(y, x, xp, mice_fun, mice_args, ...)
{
    #-- catch arguments
    arguments <- c("mice_fun", "mice_args")
    res <- syn_mice_catch_arguments(arguments=arguments)
    vname <- res$vname
    mice_arg_list <- res$mice_arg_list

    #-- create design matrix
    res <- syn_mice_create_design_matrix(x=x, xp=xp)
    x <- res$x
    xp <- res$xp

    #-- arrange data for mice imputation
    args <- mice_arg_list[["mice_args"]][[vname]]
    args <- syn_mice_create_argument_list(args=args, y=y, x=x, xp=xp)

    #-- apply mice imputation method for synthesization
    mice_fun <- paste0("mice.impute.", mice_arg_list[["mice_fun"]][[vname]])
    yimp <- do.call(what=mice_fun, args=args)

    #-- output
    res1 <- list(res=yimp, fit=NULL)
    return(res1)
}
