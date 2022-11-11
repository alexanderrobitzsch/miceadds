## File Name: syn_da_extend_vector.R
## File Version: 0.02

syn_da_extend_vector <- function(x, n, names=NULL)
{
    if (length(x)==1 & (n>1) ){
        x <- rep(x,n)
        if (!is.null(names)){
            names(x) <- names
        }
    }
    return(x)
}
