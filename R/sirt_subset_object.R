## File Name: sirt_subset_object.R
## File Version: 0.01

sirt_subset_object <- function(x, subset)
{
    if ( length(x) > 1 ){
        x <- x[subset]
    }
    return(x)
}
