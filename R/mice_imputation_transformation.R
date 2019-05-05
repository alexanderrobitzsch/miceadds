## File Name: mice_imputation_transformation.R
## File Version: 0.03

mice_imputation_transformation <- function(x, trafo_fun)
{
    y <- x
    if (!is.null(trafo_fun)){
        args1 <- list(x=x)
        y <- do.call(what=trafo_fun, args=args1)
    }
    return(y)
}

