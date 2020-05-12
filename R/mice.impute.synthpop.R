## File Name: mice.impute.synthpop.R
## File Version: 0.13

mice.impute.synthpop <- function(y, ry, x, synthpop_fun="norm", synthpop_args=list(),
    proper=TRUE, ...)
{
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    pos <- res$pos
    state_data <- res$data

    #- collect arguments
    synthpop_fun <- mice_imputation_extract_list_arguments( synthpop_fun, vname, "norm")
    synthpop_args <- mice_imputation_extract_list_arguments( synthpop_args, vname, list())
    proper <- mice_imputation_extract_list_arguments( proper, vname, TRUE)

    #- define function arguments
    synthpop_fun <- paste0("syn.", synthpop_fun)
    synthpop_args$proper <- proper
    synthpop_args$y <- y[ry]
    synthpop_args$x <- as.data.frame(x[ry,,drop=FALSE])
    synthpop_args$xp <- as.data.frame(x[!ry,,drop=FALSE])

    #- apply synthpop method
    res <- do.call(what=synthpop_fun, args=synthpop_args)
    ximp <- res[[1]]
    return(ximp)
}
