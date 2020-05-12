## File Name: syn.constant.R
## File Version: 0.13


syn.constant <- function(y, x, xp, fixed_values, ...)
{
    #-- catch arguments
    arguments <- c("fixed_values")
    res <- syn_mice_catch_arguments(arguments=arguments)
    vname <- res$vname
    mice_arg_list <- res$mice_arg_list

    #- write imputations
    yimp <- mice_arg_list[["fixed_values"]][[vname]]

    #-- output
    res1 <- list(res=yimp, fit=NULL)
    return(res1)
}
