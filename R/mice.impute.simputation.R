## File Name: mice.impute.simputation.R
## File Version: 0.07


mice.impute.simputation <- function(y, ry, x, Fun=NULL, Fun_args=NULL, ... )
{
    requireNamespace("simputation")
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    pos <- res$pos
    state_data <- res$data

    default_fun <- simputation::impute_lm
    default_args <- list()
    Fun <- mice_imputation_extract_list_arguments( Fun, vname, default_fun)
    Fun_args <- mice_imputation_extract_list_arguments( Fun_args, vname, default_args)

    dat <- data.frame(y, x)
    colnames(dat) <- c("y", paste0("x",1:ncol(x)) )

    Fun_args$dat <- dat
    Fun_args$formula <- as.formula(paste0("y ~", paste0(colnames(dat)[-1], collapse="+")))
    res <- do.call(what=Fun, args=Fun_args)
    yimp <- res[!ry,"y"]

    #-- output imputed values
    return(yimp)
}
