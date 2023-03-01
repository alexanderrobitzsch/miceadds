## File Name: mice_imputation_pls_include_derived_vars.R
## File Version: 0.103

mice_imputation_pls_include_derived_vars <- function(x, derived_vars)
{
    added_vars <- NULL
    if (!is.null(derived_vars)){
        x0 <- as.data.frame(x)
        x2 <- stats::model.matrix(derived_vars, x0)
        added_vars <- colnames(x2)
        if (ncol(x2)>1){
            if (stats::sd(x2[,1]) < 1e-4){
                x2 <- x2[,-1]
            }
            added_vars <- added_vars[-1]
        }
        x2 <- as.matrix(x2)
        x <- cbind(x, x2)
    }
    #--- output
    res <- list(x=x, added_vars=added_vars)
    return(res)
}
