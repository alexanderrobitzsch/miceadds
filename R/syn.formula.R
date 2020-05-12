## File Name: syn.formula.R
## File Version: 0.09


syn.formula <- function(y, x, xp, proper=FALSE, syn_formula, syn_fun, syn_args, ...)
{
    #-- catch arguments
    arguments <- c("syn_fun", "syn_args", "syn_formula")
    res <- syn_mice_catch_arguments(arguments=arguments)
    vname <- res$vname
    syn_arg_list <- res$mice_arg_list

    #-- create design matrix
    syn_formula <- syn_arg_list[[ "syn_formula" ]][[ vname ]]
    res <- syn_mice_create_design_matrix(x=x, xp=xp, formula=syn_formula)
    x <- res$x
    xp <- res$xp

    #-- arrange data for mice imputation
    args <- syn_arg_list[["syn_args"]][[vname]]
    args$y <- y
    args$x <- x
    args$xp <- xp
    args$proper <- proper

    #-- apply mice imputation method for synthesization
    syn_fun <- paste0("syn.", syn_arg_list[["syn_fun"]][[vname]])
    res <- do.call(what=syn_fun, args=args)

    #-- output
    return(res)
}
